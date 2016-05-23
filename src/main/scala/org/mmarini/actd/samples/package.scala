// Copyright (c) 2016 Marco Marini, marco.marini@mmarini.org
//
// Licensed under the MIT License (MIT);
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://opensource.org/licenses/MIT
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

package org.mmarini.actd

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

import org.mmarini.actd.samples.FlatWallStatus
import org.mmarini.actd.samples.WallStatus

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg.DenseVector
import rx.lang.scala.Observable
import rx.lang.scala.Subject
import rx.lang.scala.Subscription

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

    def traceByInterval(msg: String = "", dt: FiniteDuration = 1 second): Iterator[T] = {
      var t = Deadline.now

      iter.zipWithIndex.map {
        case (x, i) =>
          if (t.isOverdue()) {
            logger.info(f"$msg%s - $i%,d")
            t = Deadline.now + dt
          }
          x
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

  implicit class WallIteratorFactory(iter: Iterator[(Feedback, Double, ACAgent)]) {
    val RowIdx = 0
    val ColIdx = 1
    val RowSpeedIdx = 2
    val ColSpeedIdx = 3
    val PadIdx = 4
    val ActionIdx = 5

    /**
     * Converts a wall iterator to a vector.
     *
     * The vector components are:
     *
     *  -  ball row of status 0
     *  -  ball columns of status 0
     *  -  ball speed row of status 0
     *  -  ball speed columns of status 0
     *  -  pad location  columns of status 0
     *  -  action performed
     *  -  reward
     *  -  ball row of status 1
     *  -  ball columns of status 1
     *  -  ball speed row of status 1
     *  -  ball speed columns of status 1
     *  -  pad location  columns of status 1
     *  -  error from critic
     */
    def toSamples: Iterator[DenseVector[Double]] =
      iter.map {
        case (Feedback(s0, action, reward, s1), err, _) =>
          val WallStatus((r0, c0), dir0, pad0) = s0.asInstanceOf[WallStatusVector].status
          val WallStatus((r1, c1), dir1, pad1) = s1.asInstanceOf[WallStatusVector].status
          DenseVector.vertcat(DenseVector(r0, c0, dir0.id, pad0,
            action.toDouble, reward,
            r1, c1, dir1.id, pad1, err))
      }

    /**
     * Converts a wall iterator to a vector.
     *
     * The vector components are:
     *
     *  -  ball row of status 0
     *  -  ball columns of status 0
     *  -  ball speed row of status 0
     *  -  ball speed columns of status 0
     *  -  pad location  columns of status 0
     *  -  action performed
     *  -  reward
     *  -  ball row of status 1
     *  -  ball columns of status 1
     *  -  ball speed row of status 1
     *  -  ball speed columns of status 1
     *  -  pad location  columns of status 1
     *  -  error from critic
     */
    def toSamplesWithStatus: Iterator[DenseVector[Double]] =
      iter.map(recordToSamplesWithStatus)
  }

  /**
   * Converts a record to a vector.
   *
   * The vector components are:
   *
   *  -  ball row of status 0
   *  -  ball columns of status 0
   *  -  ball speed row of status 0
   *  -  ball speed columns of status 0
   *  -  pad location  columns of status 0
   *  -  action performed
   *  -  reward
   *  -  ball row of status 1
   *  -  ball columns of status 1
   *  -  ball speed row of status 1
   *  -  ball speed columns of status 1
   *  -  pad location  columns of status 1
   *  -  error from critic
   */
  def recordToSamplesWithStatus(record: (Feedback, Double, ACAgent)): DenseVector[Double] = record match {
    case (Feedback(s0, action, reward, s1), err, agent) =>
      val WallStatus((r0, c0), dir0, pad0) = s0.asInstanceOf[WallStatusVector].status
      val WallStatus((r1, c1), dir1, pad1) = s1.asInstanceOf[WallStatusVector].status
      val sv0 = agent.critic(s0.toDenseVector).output
      val sv1 = agent.critic(s1.toDenseVector).output
      DenseVector.vertcat(DenseVector(r0, c0, dir0.id, pad0,
        action.toDouble, reward,
        r1, c1, dir1.id, pad1, err), sv0, sv1)
  }
}
