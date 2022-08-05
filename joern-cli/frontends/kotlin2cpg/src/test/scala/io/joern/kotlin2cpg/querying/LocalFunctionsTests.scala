package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

class LocalFunctionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  implicit val resolver = NoResolve

  "CPG for code with local function definition" should {
    val cpg = code("""
        |package mypkg
        |
        |fun sink(s: String) = println(s)
        |
        |fun f1(p: String) {
        |    fun f2(q: String, o: String) {
        |        sink(o)
        |    }
        |    f2("IRRELEVANT", p)
        |}
        |
        |""".stripMargin)

    "should contain a METHOD node for the local function" in {
      cpg.method.name(".*f2.*").size shouldBe 1
    }
  }
}
