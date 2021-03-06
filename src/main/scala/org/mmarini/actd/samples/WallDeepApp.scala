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
import akka.actor.ActorSystem
import org.mmarini.actd.EnvironmentActor
import akka.actor.Actor
import akka.actor.Props
import org.mmarini.actd.EnvironmentActor.Interact
import org.mmarini.actd.EnvironmentActor.Step
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.Duration.Zero
import org.mmarini.actd.TDAgentActor.QueryAgent
import org.mmarini.actd.TDAgentActor.CurrentAgent

/** The UI wall game application */
object WallDeepApp extends SimpleSwingApplication with LazyLogging {
  val SlowTime = 200 millis
  val FastTime = Zero

  /** Button panel */
  val buttonPane = new BoxPanel(Orientation.Vertical) {

    val slowBtn = new Button("Slow")
    val fastBtn = new Button("Fast")
    val saveBtn = new Button("Save")

    slowBtn.reactions += {
      case ButtonClicked(b) =>
        uiActor ! SlowTime
    }
    fastBtn.reactions += {
      case ButtonClicked(b) =>
        uiActor ! FastTime
    }
    saveBtn.reactions += {
      case ButtonClicked(b) =>
        uiActor ! "aaaa"
    }

    contents += slowBtn
    contents += fastBtn
    contents += saveBtn
  }

  /** Info panel */
  val infoPane = new BoxPanel(Orientation.Horizontal)

  /** Game panel */
  val gamePane = new Component() {

    var sOpt: Option[WallDeepStatus] = None;

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

          val ball = new Ellipse2D.Float(bc * cw / WallDeepStatus.Width,
            (WallDeepStatus.Height - br) * ch / (WallDeepStatus.Height + 1),
            cw / WallDeepStatus.Width,
            ch / (WallDeepStatus.Height + 1))
          g.setColor(Color.YELLOW)
          g.fill(ball)

          val pad = new Rectangle2D.Float(s.pad * cw / WallDeepStatus.Width,
            WallDeepStatus.Height * ch / (WallDeepStatus.Height + 1),
            3 * cw / WallDeepStatus.Width,
            ch / (WallDeepStatus.Height + 1))

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

    title = "Wall Deep Learning App"

    val cr = new BoxPanel(Orientation.Vertical)
    cr.contents += infoPane
    cr.contents += gamePane

    val c = new BoxPanel(Orientation.Horizontal)
    c.contents += buttonPane
    c.contents += cr

    contents = c;
    size = new Dimension(WindowWidth, WindowHeight)
  }

  /** Creates the initial environment */
  val (initStatus, parms, critic, actor) = WallDeepStatus.initEnvParms

  val system = ActorSystem("WallDeepApp")

  class UIActor extends Actor {

//    val environment = context.actorOf(EnvironmentActor.props(initStatus, new TDAgent(parms, critic, actor)))
    val environment = context.actorOf(EnvironmentActor.props(initStatus,  TDAgent("data/agent")))

    environment ! Interact

    def receive: Receive = steppingDelayed(SlowTime)

    private def stepping: Receive = {
      case t: FiniteDuration if (t > Zero) =>
        context become steppingDelayed(t)

      case filename: String =>
        environment ! QueryAgent
        context become steppingSave(filename)

      case Step(f, d, _) =>
        gamePane.sOpt = Some(f.s1.asInstanceOf[WallDeepStatus])
        gamePane.repaint
        environment ! Interact
    }

    private def steppingSave(filename: String): Receive = {
      case t: FiniteDuration if (t > Zero) =>
        context become steppingDelayedSave(t, filename)

      case filename: String =>
        context become steppingSave(filename)

      case Step(f, d, _) =>
        gamePane.sOpt = Some(f.s1.asInstanceOf[WallDeepStatus])
        gamePane.repaint
        environment ! Interact

      case CurrentAgent(agent) =>
        agent.write(filename)
        logger.info(s"Saved $filename")
        context become stepping
    }

    private def steppingDelayed(delay: FiniteDuration): Receive = {
      case t: FiniteDuration if (t == Zero) =>
        context become stepping

      case t: FiniteDuration =>
        context become steppingDelayed(t)

      case filename: String =>
        environment ! QueryAgent
        context become steppingDelayedSave(delay, filename)

      case Step(f, d, _) =>
        gamePane.sOpt = Some(f.s1.asInstanceOf[WallDeepStatus])
        gamePane.repaint

        import system.dispatcher

        context.system.scheduler.scheduleOnce(delay, environment, Interact)
    }

    private def steppingDelayedSave(delay: FiniteDuration, filename: String): Receive = {
      case t: FiniteDuration if (t == Zero) =>
        context become steppingSave(filename)

      case t: FiniteDuration =>
        context become steppingDelayedSave(t, filename)

      case filename: String =>
        context become steppingDelayedSave(delay, filename)

      case Step(f, d, _) =>
        gamePane.sOpt = Some(f.s1.asInstanceOf[WallDeepStatus])
        gamePane.repaint

        import system.dispatcher

        context.system.scheduler.scheduleOnce(delay, environment, Interact)

      case CurrentAgent(agent) =>
        agent.write(filename)
        logger.info(s"Saved $filename")
        context become steppingDelayed(delay)
    }
  }

  val uiActor = system.actorOf(Props(classOf[UIActor]))

}
