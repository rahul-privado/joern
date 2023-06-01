package io.joern.rubysrc2cpg.dataflow

import io.joern.rubysrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class DataFlowTests extends DataFlowCodeToCpgSuite {

  "CPG for code with flow through a function and if-elseif-else" should {
    val cpg = code("""
        |def add_three_numbers(num1, num2, num3)
        | sum = num1 + num2 + num3
        | p = sum
        | q = p
        | r = q
        | s = r
        | x = 7
        |
        |if x > 4
        | a = x + 1
        | return a
        |elseif x < 2
        | b = x - 1
        | return b
        |else
        | c = x + 10
        | return c
        |end
        |
        |end
        |
        |n1 = 1
        |n2 = 2
        |n3 = 3
        |
        |n = add_three_numbers( n1, n2, n3 )
        |to_print = n
        |puts to_print
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("n1").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }
}
