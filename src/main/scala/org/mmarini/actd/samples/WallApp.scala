package org.mmarini.actd.samples

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import scala.concurrent.duration.DurationInt
import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.SimpleSwingApplication
import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.Environment
import org.mmarini.actd.Feedback
import org.mmarini.actd.TDAgent
import org.mmarini.actd.TDParms
import com.typesafe.scalalogging.LazyLogging
import breeze.stats.distributions.RandBasis
import rx.lang.scala.JavaConversions.javaSchedulerToScalaScheduler
import rx.lang.scala.Observable
import rx.lang.scala.Subject
import rx.schedulers.SwingScheduler
import org.netlib.lapack.Sopgtr

/** */
object WallApp extends SimpleSwingApplication with LazyLogging {
  val WaitTime = 50 millis
  val Alpha = 3e-3
  val Beta = 0.3
  val Gamma = 0.99
  val Epsilon = 0.1
  val Lambda = 3e-3
  val Eta = 3e-1
  val Sigma = 1.0
  val Seed = 1234L

  val OutputCount = 3

  val buttonPane = new BoxPanel(Orientation.Vertical)
  val infoPane = new BoxPanel(Orientation.Horizontal)

  val gamePane = new Component() {

    var sOpt: Option[WallStatus] = None;

    override def paintComponent(g: Graphics2D) {
      val cw = size.width
      val ch = size.height
      g.setColor(Color.BLACK)
      for { s <- sOpt } {
        val (br, bc) = s.ball

        val ball = new Ellipse2D.Float(bc * cw / WallStatus.Width,
          br * ch / (WallStatus.Height + 1),
          cw / WallStatus.Width,
          ch / (WallStatus.Height + 1))
        g.setColor(Color.BLUE)
        g.fill(ball)

        val pad = new Rectangle2D.Float(s.pad * cw / WallStatus.Width,
          WallStatus.Height * ch / (WallStatus.Height + 1),
          3 * cw / WallStatus.Width,
          ch / (WallStatus.Height + 1))

        g.setColor(Color.GREEN)
        g.fill(pad)
      }
    }
  }

  /** */
  val top = new MainFrame {
    title = "Wall App"

    val cr = new BoxPanel(Orientation.Vertical)
    cr.contents += infoPane
    cr.contents += gamePane

    val c = new BoxPanel(Orientation.Horizontal)
    c.contents += buttonPane
    c.contents += cr

    contents = c;
    size = new Dimension(800, 600)
  }

  val initStatus = WallStatus.initial

  val inputCount = initStatus.toDenseVector.length

  val initAgent = TDAgent(
    TDParms(
      Alpha,
      Beta,
      Gamma,
      Lambda,
      Eta,
      new RandBasis(new MersenneTwister(Seed))),
    Sigma,
    inputCount,
    OutputCount)

  val initEnv = Environment(initStatus, initAgent)

  val startObs = Observable.just(initEnv.status.asInstanceOf[WallStatus])

  val timerObs = Observable.interval(WaitTime)

  val txObs = for { _ <- timerObs } yield {
    s: (Environment, Environment, Feedback) => s._2.stepOver
  }

  val gameFlowObs = txObs.statusFlow(initEnv.stepOver)

  val statusObs = gameFlowObs.map(_._3.s1.asInstanceOf[WallStatus])

  statusObs.subscribe(s => {
    gamePane.sOpt = Some(s)
    gamePane.repaint
  })

}