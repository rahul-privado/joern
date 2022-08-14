package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class ExtendedCfgNodeTests extends DataFlowCodeToCpgSuite {

  private implicit val resolver: NoResolve.type = NoResolve

  private val cpg = code("""
      |int foo(int y) {
      | int x = source();
      | x += y;
      | sink(y);
      |}
      |""".stripMargin)

  "allow traversing from argument of sink back to param via `ddgIn`" in {
    inside(cpg.method("sink").parameter.argument.ddgIn(semantics).l) { case List(param: nodes.MethodParameterIn) =>
      param.name shouldBe "y"
    }
  }

  "allow traversing from argument node to param via `ddgIn`" in {
    inside(cpg.method("sink").parameter.argument.l) { case List(t) =>
      t.code shouldBe "y"
      inside(t.ddgIn(semantics).l) { case List(param: nodes.MethodParameterIn) =>
        param.name shouldBe "y"
      }
    }
  }

  "allow traversing from argument back to param while inspecting edge" in {
    inside(cpg.method("sink").parameter.argument.ddgInPathElem(semantics).l) { case List(pathElem) =>
      pathElem.outEdgeLabel shouldBe "y"
      pathElem.node.isInstanceOf[nodes.MethodParameterIn] shouldBe true
    }
  }

}
