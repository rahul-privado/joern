package io.joern.scanners.android

import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.joern.suites.KotlinQueryTestSuite
import overflowdb.traversal.iterableToTraversal
import io.shiftleft.semanticcpg.language._

class CommandInjectionTests extends KotlinQueryTestSuite {
  override def queryBundle = CommandInjection

  "should match all positive examples" in {
    val query = queryBundle.uiInputToExec()
    query(cpg).flatMap(_.evidence).collect { case cfgNode: CfgNode => (cfgNode.code) }.l shouldBe
      List("matchingExample1")
  }
}
