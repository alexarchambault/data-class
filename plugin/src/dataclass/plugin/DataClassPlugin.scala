// SPDX-License-Identifier: MIT

package dataclass.plugin

import dotty.tools.dotc.plugins._

class DataClassPlugin extends StandardPlugin {
  val name: String = "data-class"
  override val description: String =
    "Generates binary-compatible overloads for @since parameters, and implements @data options"
  override def init(options: List[String]): List[PluginPhase] =
    List(new DataClassCopyPhase(), new UnrollPhase(), new DataClassPhase())
}
