/**
 *
 */
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
import rx.lang.scala.Observable
import java.awt.image.BufferedImage
import scala.swing.Button
import rx.lang.scala.Subject
import scala.swing.event.ButtonClicked
import scala.swing.AbstractButton

/** */
object WallApp extends SimpleSwingApplication with LazyLogging {
  val WaitTime = 200 millis
  val Alpha = 3e-3
  val Beta = 0.3
  val Gamma = 0.962
  val Epsilon = 0.1
  val Lambda = 3e-3
  val Eta = 3e-1
  val Sigma = 1.0
  val Seed = 1234L

  val OutputCount = 3

  val slowBtnObs = Subject[AbstractButton]()
  val fastBtnObs = Subject[AbstractButton]()

  /** Button panel */
  val buttonPane = new BoxPanel(Orientation.Vertical) {

    val slowBtn = new Button("Slow")
    val fastBtn = new Button("Fast")

    slowBtn.reactions += {
      case ButtonClicked(b) =>
        slowBtnObs.onNext(b)
    }
    fastBtn.reactions += {
      case ButtonClicked(b) =>
        fastBtnObs.onNext(b)
    }

    contents += slowBtn
    contents += fastBtn
  }

  /** Info panel */
  val infoPane = new BoxPanel(Orientation.Horizontal)

  /** Game panel */
  val gamePane = new Component() {

    var sOpt: Option[WallStatus] = None;

    private var bfOpt: Option[BufferedImage] = None

    override def paintComponent(cg: Graphics2D) {
      val cw = size.width
      val ch = size.height
      if (bfOpt.isEmpty || bfOpt.get.getWidth != cw || bfOpt.get.getHeight != ch) {
        bfOpt = Some(new BufferedImage(cw, ch, BufferedImage.TYPE_INT_ARGB))
      }
      for { bf <- bfOpt } {
        val g = bf.createGraphics
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, cw, ch)
        for { s <- sOpt } {
          val (br, bc) = s.ball

          val ball = new Ellipse2D.Float(bc * cw / WallStatus.Width,
            br * ch / (WallStatus.Height + 1),
            cw / WallStatus.Width,
            ch / (WallStatus.Height + 1))
          g.setColor(Color.YELLOW)
          g.fill(ball)

          val pad = new Rectangle2D.Float(s.pad * cw / WallStatus.Width,
            WallStatus.Height * ch / (WallStatus.Height + 1),
            3 * cw / WallStatus.Width,
            ch / (WallStatus.Height + 1))

          g.setColor(Color.GREEN)
          g.fill(pad)
        }
        cg.drawImage(bf, 0, 0, peer)
      }
    }
  }

  /** Top frame */
  val top = new MainFrame {
    val WindowWidth = 400
    val WindowHeight = 300

    title = "Wall App"

    val cr = new BoxPanel(Orientation.Vertical)
    cr.contents += infoPane
    cr.contents += gamePane

    val c = new BoxPanel(Orientation.Horizontal)
    c.contents += buttonPane
    c.contents += cr

    contents = c;
    size = new Dimension(WindowWidth, WindowHeight)
  }

  /** Initial game status */
  val initStatus = WallStatus.initial

  val inputCount = initStatus.toDenseVector.length

  /** Creates the agent */
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

  /** Creates the initial environment */
  val initEnv = Environment(initStatus, initAgent)

  /** Creates the initial environment observable */
  val startObs = Observable.just(initEnv.status.asInstanceOf[WallStatus])

  /** Creates the fast timer observable */
  val fastTimerObs = fastBtnObs.map(_ => Observable.interval(1 nanos))

  /** Creates the slow timer observable */
  val slowTimerObs = slowBtnObs.map(_ => Observable.interval(WaitTime))

  val timerObs = (slowTimerObs merge fastTimerObs).switch

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
