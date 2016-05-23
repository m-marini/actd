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

import org.mmarini.actd.ACAgent
import org.mmarini.actd.Agent
import org.mmarini.actd.QAgent
import org.mmarini.actd.Status
import org.mmarini.actd.TDNeuralNet

/** */
class WallBuilder(args: WallArguments) {

  /** Creates the initial status */
  lazy val initStatus: Status = WallStatusVector(WallStatus.initial, args.model)

  /** Creates an actor critic agent */
  private def acAgent: ACAgent =
    new ACAgent(args.tdParms,
      TDNeuralNet(args.tdParms)(initStatus.toDenseVector.length +: args.hiddens :+ 1),
      TDNeuralNet(args.tdParms)(initStatus.toDenseVector.length +: args.hiddens :+ WallStatus.PadAction.maxId))

  /** Creates a Q agent */
  private def qAgent: QAgent =
    new QAgent(args.tdParms,
      TDNeuralNet(args.tdParms)(initStatus.toDenseVector.length +: args.hiddens :+ WallStatus.PadAction.maxId))

  /** Creates an agent */
  lazy val initAgent: Agent =
    args.agent match {
      case "QAgent" => qAgent
      case "ACAgent" => acAgent
    }
}
