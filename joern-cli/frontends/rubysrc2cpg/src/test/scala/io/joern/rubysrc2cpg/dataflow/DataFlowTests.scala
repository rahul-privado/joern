package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DataFlowTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  "flow through a proc definition with non-empty block and zero parameters" should {
    val cpg = code("""
        |x=10
        |y = x
        |-> {
        |puts y
        |}.call
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }
}
