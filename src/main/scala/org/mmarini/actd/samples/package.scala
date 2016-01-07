/**
 *
 */
package org.mmarini.actd

/**
 * @author us00852
 *
 */
import rx.lang.scala.Observable
import rx.lang.scala.Subject
import rx.lang.scala.Subscription
import rx.lang.scala.Observer
import rx.lang.scala.subscriptions.CompositeSubscription
import scala.util.Try
import com.typesafe.scalalogging.LazyLogging
import scala.util.Random
import org.slf4j.Marker
import breeze.linalg.DenseVector
import scalax.file.Path
import breeze.io.CSVWriter
import scalax.io.Resource

/** */
package object samples extends LazyLogging {

  implicit class ObservableFactory[T](subject: Observable[T]) extends LazyLogging {

    val HashLength = 4

    def hash(o: Any): String = o.hashCode.toHexString.takeRight(HashLength)

    /** Creates an observable that emits a sequence of last n values emitted */
    def history(length: Int): Observable[Seq[T]] =
      subject.scan(Seq[T]())(
        (seq, current) => {
          val tail = seq.take(length - 1)
          current +: tail
        })

    /**
     * Creates an observable that emits just the last value of  a variable
     * since creation instant
     * The first value will be emitted with sampled observable the further values are immediate
     */
    def latest: Observable[T] = {
      var value: Observable[T] = subject.take(1)
      subject.subscribe(x => value = Observable.just(x),
        ex => value = Observable.error(ex),
        () => {})
      Observable.defer(value)
    }

    def traced(id: String): Observable[T] = {
      var ct = 0
      Observable.create[T](obsr => {
        ct = ct + 1
        val traceId = s"${hash(subject)}.$ct.${hash(obsr)} $id"
        logger.debug("{} subscribe", traceId)
        val sub = subject.subscribe(
          x => {
            logger.debug("{} onNext {}", traceId, String.valueOf(x))
            obsr.onNext(x)
          },
          e => {
            logger.error("$traceId error", traceId, e)
            obsr.onError(e)
          },
          () => {
            logger.debug("{}on Completed", traceId)
            obsr.onCompleted
          })
        Subscription {
          logger.debug("{} unsubscribe", traceId)
          sub.unsubscribe()
        }
      })
    }

    def trace(msg: String = "") {
      traced(msg).subscribe
    }

    /**
     * Creates an observable that emits the value composed by trigger observable and
     * previous value of sampling observable
     * or optional default values if no previous value available
     */
    def withLatest[S, R](other: Observable[S])(implicit f: (T, S) => R = (t: T, s: S) => (t, s)): Observable[R] = {
      val latestSample = other.latest
      val x = for { t <- subject } yield for { v <- latestSample } yield f(t, v)
      x.flatten
    }
  }

  implicit class StateFlowFactory[T](subject: Observable[T => T]) {
    /** State flow */
    def statusFlowWithInitObs(init: Observable[T], isEnd: T => Boolean = _ => false): Observable[T] = {
      var subj = Subject[Observable[T]]
      init.subscribe(
        init => {
          val sm = subject.statusFlow(init, isEnd)
          subj.onNext(sm)
        },
        ex => subj.onError(ex),
        () => subj.onCompleted)
      subj.flatten
    }

    /** State flow */
    def statusFlow(init: T, isEnd: T => Boolean = _ => false): Observable[T] = {
      var s = init
      val subj = Subject[T]
      subject.takeWhile(_ => !isEnd(s)) subscribe (
        f => {
          // Computes new status
          val s1 = f(s)
          // Stores new status
          s = s1
          // emits new status
          subj.onNext(s1)
        },
        ex => subj.onError(ex),
        () => subj.onCompleted)
      subj
    }
  }

  implicit class OptionObservableFactory[T](subject: Observable[Option[T]]) {
    def optionFlatten: Observable[T] =
      subject.filterNot(_.isEmpty).map(_.get)
  }

  /** Shuffles a sequence */
  def shuffle[T](seq: IndexedSeq[T])(random: Random): IndexedSeq[T] = {
    val n = seq.length
    if (n <= 1) {
      seq
    } else {
      (0 until n - 1).foldLeft(seq)((seq, i) => {
        val j = random.nextInt(n - i) + i
        if (i != j) {
          seq.updated(j, seq(i)).updated(i, seq(j))
        } else {
          seq
        }
      })
    }
  }

  implicit class IteratorFactory[T](iter: Iterator[T]) extends LazyLogging {

    def trace(msg: String = "", step: Int = 1): Iterator[T] =
      iter.zipWithIndex.map {
        case (x, i) =>
          if (i % step == 0) logger.info(f"$msg%s - $i%,d")
          x
      }

  }

  implicit class VectorIteratorFactory(iter: Iterator[DenseVector[Double]]) {

    def toCsvRows(
      separator: Char = ',',
      quote: Char = '\u0000',
      escape: Char = '\\'): Iterator[String] =
      iter.map {
        case (mat) =>
          CSVWriter.mkString(
            IndexedSeq.tabulate(1, mat.size)((_, c) => mat(c).toString),
            separator, quote, escape)
      }

    def write(filename: String,
      separator: Char = ',',
      quote: Char = '\u0000',
      escape: Char = '\\') {

      Path.fromString(filename).deleteIfExists()
      val output = Resource.fromFile(filename)
      for {
        processor <- output.outputProcessor
        out = processor.asOutput
      } {
        for { row <- toCsvRows(separator, quote, escape) } {
          out.write(row)
          out.write("\n")
        }
      }
    }

  }

  object Indexes {
    val RowIdx = 0
    val ColIdx = 1
    val RowSpeedIdx = 2
    val ColSpeedIdx = 3
    val PadIdx = 4
    val ActionIdx = 5
  }

  implicit class WallIteratorFactory(iter: Iterator[(Environment, Environment, Feedback)]) {
    val RowIdx = 0
    val ColIdx = 1
    val RowSpeedIdx = 2
    val ColSpeedIdx = 3
    val PadIdx = 4
    val ActionIdx = 5

    def toSamples: Iterator[DenseVector[Double]] =
      iter.map {
        case (e0, e1, Feedback(_, action, reward, _)) =>
          val WallStatus((r0, c0), (sr0, sc0), pad0) = e0.status.asInstanceOf[WallStatus]
          val WallStatus((r1, c1), (sr1, sc1), pad1) = e1.status.asInstanceOf[WallStatus]
          val s0 = e0.status.toDenseVector
          val s1 = e1.status.toDenseVector
          DenseVector.vertcat(DenseVector(r0, c0, sr0, sc0, pad0,
            action.toDouble, reward,
            r1, c1, sr1, sc1, pad1))
      }

    def toSamplesWithAC: Iterator[DenseVector[Double]] =
      iter.map {
        case (e0, e1, Feedback(_, action, reward, _)) =>
          val WallStatus((r0, c0), (sr0, sc0), pad0) = e0.status.asInstanceOf[WallStatus]
          val WallStatus((r1, c1), (sr1, sc1), pad1) = e1.status.asInstanceOf[WallStatus]
          val s0 = e0.status.toDenseVector
          val s1 = e1.status.toDenseVector
          val s00v = e0.agent.asInstanceOf[TDAgent].critic(s0).output
          val s01v = e0.agent.asInstanceOf[TDAgent].critic(s1).output
          val p0 = e0.agent.asInstanceOf[TDAgent].actor(s0).output
          val s10v = e1.agent.asInstanceOf[TDAgent].critic(s0).output
          val p1 = e1.agent.asInstanceOf[TDAgent].actor(s0).output
          DenseVector.vertcat(DenseVector(r0, c0, sr0, sc0, pad0,
            action.toDouble, reward,
            r1, c1, sr1, sc1, pad1),
            s00v, s01v, p0,
            s10v, p1)
      }
  }

}
