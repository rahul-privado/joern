package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class AstCreationPassTests extends GoCodeToCpgSuite {

  "Method Ast layout" should {

    "be correct for decl assignment" in {
      val cpg = code("""
          |package main
          |func main() {
          |   var local int = 1
          |}
          |""".stripMargin)

      inside(cpg.method.name("main").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        local.order shouldBe 1
        call.name shouldBe Operators.assignment
        call.order shouldBe 2
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          literal.typeFullName shouldBe "int"
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }

    "be correct for nested expression" in {
      val cpg = code("""
          |package main
          |func method() {
          |  var x int
          |  var y int
          |  var z int
          |
          |  x = y + z
          |}
      """.stripMargin)

      val localX = cpg.local.order(1)
      localX.name.l shouldBe List("x")
      val localY = cpg.local.order(2)
      localY.name.l shouldBe List("y")
      val localZ = cpg.local.order(3)
      localZ.name.l shouldBe List("z")

      inside(cpg.method.name("method").ast.isCall.name(Operators.assignment).map(new OpNodes.Assignment(_)).l) {
        case List(assignment) =>
          assignment.target.code shouldBe "x"
          assignment.source.start.isCall.name.l shouldBe List(Operators.addition)
          inside(assignment.source.astChildren.l) { case List(id1: Identifier, id2: Identifier) =>
            id1.order shouldBe 1
            id1.code shouldBe "y"
            id2.order shouldBe 2
            id2.code shouldBe "z"
          }
      }
    }
  }

  "multiple declaration on single line" should {
    val cpg = code("""
        |package main
        |func main(){
        |   var  i, j int
        |   var  f, salary float32 = 10.0, 20.0
        |}
        |""".stripMargin)
    "create local and identifier nodes" in {
      val locals      = cpg.local.l
      val identifiers = cpg.identifier.l

      locals.size shouldBe 4
      locals.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(
        ("i", "int"),
        ("j", "int"),
        ("f", "float32"),
        ("salary", "float32")
      )

      identifiers.size shouldBe 2
      identifiers.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("f", "float32"), ("salary", "float32"))
    }
  }

  "dynamic declaration" should {
    val cpg = code("""
        |package main
        |func main(){
        |   d := 43
        |   c := "value"
        |}
        |""".stripMargin)
    "have local and identifier nodes created" in {
      val locals      = cpg.local.l
      val identifiers = cpg.identifier.l

      locals.size shouldBe 2
      locals.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("d", "int"), ("c", "string"))

      identifiers.size shouldBe 2
      identifiers.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("d", "int"), ("c", "string"))
    }
  }

  "be correct for if" in {

    val cpg = code("""
      |package main
      |func method() {
      |  var y int = 5
      |  var x int = 2
      |  if (x > 0) {
      |    y = 0
      |  }
      |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
      controlStruct.code shouldBe "if (x > 0)"
      controlStruct.controlStructureType shouldBe ControlStructureTypes.IF
      inside(controlStruct.condition.l) { case List(cndNode) =>
        cndNode.code shouldBe "x > 0"

      }
      controlStruct.whenTrue.assignment.code.l shouldBe List("y = 0")
    }
  }

  "be correct for if-else" in {
    val cpg = code("""
        |package main
        |func method() {
        |  var y int = 6
        |  var x int = 9
        |  if (x > 0) {
        |    y = 0
        |  } else {
        |    y = 1
        |  }
        |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(ifStmt, elseStmt) =>
      ifStmt.controlStructureType shouldBe ControlStructureTypes.IF
      ifStmt.code shouldBe "if (x > 0)"
      elseStmt.controlStructureType shouldBe ControlStructureTypes.ELSE
      elseStmt.code shouldBe "else"

      inside(ifStmt.condition.l) { case List(cndNode) =>
        cndNode.code shouldBe "x > 0"
      }

      ifStmt.whenTrue.assignment
        .map(x => (x.target.code, x.source.code))
        .headOption shouldBe Some(("y", "0"))
      ifStmt.whenFalse.assignment
        .map(x => (x.target.code, x.source.code))
        .headOption shouldBe Some(("y", "1"))
    }
  }

  "be correct for switch case 1" in {

    val cpg = code("""
        |package main
        |func method() {
        |  var marks int = 90
        |  var grade string = "B"
        |  switch marks {
        |      case 90: grade = "A"
        |      case 50,60,70: grade = "C"
        |      default: grade = "D"
        |   }
        |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
      controlStruct.code shouldBe "switch marks"
      controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
      inside(controlStruct.astChildren.l) { case List(cond: Identifier, switchBlock: Block) =>
        cond.code shouldBe "marks"
        switchBlock.astChildren.size shouldBe 12
        switchBlock.astChildren.code.l shouldBe List(
          "case 90",
          "90",
          "grade = \"A\"",
          "case 50",
          "50",
          "case 60",
          "60",
          "case 70",
          "70",
          "grade = \"C\"",
          "default",
          "grade = \"D\""
        )
      }
    }
  }

  "be correct for switch case 2" in {

    val cpg = code("""
        |package main
        |func method() {
        |  var marks int = 90
        |  var grade string = "B"
        |  switch {
        |      case grade == "A" :
        |         marks = 95
        |      case grade == "B":
        |         marks = 80
        |   }
        |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
      controlStruct.code shouldBe "switch "
      controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
      inside(controlStruct.astChildren.l) { case List(switchBlock: Block) =>
        switchBlock.astChildren.size shouldBe 6
        switchBlock.astChildren.code.l shouldBe List(
          "case grade == \"A\"",
          "grade == \"A\"",
          "marks = 95",
          "case grade == \"B\"",
          "grade == \"B\"",
          "marks = 80"
        )
      }
    }
  }

  "be correct for switch case 3" ignore {

    val cpg = code("""
        |package main
        |func method() {
        |   var x interface{}
        |   var y int = 6
        |   switch i := x.(type) {
        |      case nil:
        |         y = 5
        |      case int:
        |         y = 8
        |      case float64:
        |         y= 12
        |   }
        |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
      controlStruct.code shouldBe "switch i := x.(type)"
      controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
      inside(controlStruct.astChildren.l) { case List(assignment: Call, switchBlock: Block) =>
        switchBlock.astChildren.size shouldBe 9
        switchBlock.astChildren.code.l shouldBe List(
          "case nil",
          "nil",
          "y = 5",
          "case int",
          "int",
          "y = 8",
          "case float64",
          "float64",
          "y = 12"
        )
      }
    }
  }

  "be correct for switch case 4" in {

    val cpg = code("""
        |package main
        |func method() {
        |   var x interface{}
        |   var y int = 6
        |   switch x.(type) {
        |      case nil:
        |         y = 5
        |      case int:
        |         y = 8
        |      case float64:
        |         y = 12
        |   }
        |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
      controlStruct.code shouldBe "switch x.(type)"
      controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
      inside(controlStruct.astChildren.l) { case List(identifier: Identifier, switchBlock: Block) =>
        identifier.code shouldBe "x"
        switchBlock.astChildren.size shouldBe 9
        switchBlock.astChildren.code.l shouldBe List(
          "case nil",
          "nil",
          "y = 5",
          "case int",
          "int",
          "y = 8",
          "case float64",
          "float64",
          "y = 12"
        )
      }
    }
  }

  "be correct for for-loop case 1" in {
    val cpg = code("""
        |package main
        |
        |func main() {
        |   var b int
        |
        |   /* for loop execution */
        |   for a := 0; a < 10; a++ {
        |       b += a
        |   }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.l) { case List(forStmt) =>
      forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
      inside(forStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
        initializerBlock.astChildren.isCall.code.l shouldBe List("a := 0")
      }
      inside(forStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
        lessThanCall.code shouldBe "a < 10"
      }
      inside(forStmt.astChildren.order(3).l) { case List(increCall: Call) =>
        increCall.code shouldBe "a++"
      }
      inside(forStmt.astChildren.order(4).l) { case List(body: Block) =>
        body.astChildren.isCall.code.l shouldBe List("b += a")
      }
    }
  }

  "be correct for for-loop case 2" in {
    val cpg = code("""
        |package main
        |
        |func main() {
        |   var b int = 15
        |   var a int
        |
        |   /* for loop execution */
        |   for a < b {
        |       a++
        |   }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.l) { case List(forStmt) =>
      forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
      inside(forStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
        initializerBlock.astChildren.size shouldBe 0
      }
      inside(forStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
        lessThanCall.code shouldBe "a < b"
      }

      forStmt.astChildren.order(3).size shouldBe 0

      inside(forStmt.astChildren.order(4).l) { case List(body: Block) =>
        body.astChildren.isCall.code.l shouldBe List("a++")
      }
    }
  }

  "be correct for for-loop case 3" in {
    val cpg = code("""
        |package main
        |import "fmt"
        |func main() {
        |   message := "Hello, Gophers!"
        |
        |   var counter int
        |   for index, char := range message {
        |        counter++
        |    }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.l) { case List(forStmt) =>
      forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
      inside(forStmt.astChildren.order(1).l) { case List(identifier: Identifier) =>
        identifier.name shouldBe "message"
      }
      inside(forStmt.astChildren.order(2).l) { case List(localBlock: Block) =>
        localBlock.astChildren.isLocal.code.l shouldBe List("index", "char")
      }

      inside(forStmt.astChildren.order(3).l) { case List(assignCall: Call) =>
        assignCall.code shouldBe "index, char := range message"
      }

      inside(forStmt.astChildren.order(4).l) { case List(body: Block) =>
        body.astChildren.isCall.code.l shouldBe List("counter++")
      }
    }
  }

  "be correct for for-loop nested structure" in {
    val cpg = code("""
        |package main
        |import "fmt"
        |func main() {
        |   var i, j int
        |   counter := 0
        |   for i = 2; i < 100; i++ {
        |      for j = 2; j <= (i/j); j++ {
        |         if(i%j==0) {
        |            counter++
        |         }
        |      }
        |   }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.l) { case List(outerForStmt, innerForStmt, ifStmt) =>
      outerForStmt.controlStructureType shouldBe ControlStructureTypes.FOR
      inside(outerForStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
        initializerBlock.astChildren.isCall.code.l shouldBe List("i = 2")
      }
      inside(outerForStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
        lessThanCall.code shouldBe "i < 100"
      }
      inside(outerForStmt.astChildren.order(3).l) { case List(increCall: Call) =>
        increCall.code shouldBe "i++"
      }

      outerForStmt.astChildren.ast.isControlStructure.l shouldBe List(innerForStmt, ifStmt)

      inside(innerForStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
        initializerBlock.astChildren.isCall.code.l shouldBe List("j = 2")
      }
      inside(innerForStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
        lessThanCall.code shouldBe "j <= (i/j)"
      }
      inside(innerForStmt.astChildren.order(3).l) { case List(increCall: Call) =>
        increCall.code shouldBe "j++"
      }

      innerForStmt.astChildren.ast.isControlStructure.l shouldBe List(ifStmt)
    }
  }

  "ast creation for break, continue, goto" should {

    "be correct" in {

      val cpg = code("""
          |package main
          |
          |func main() {
          |   var b int
          |   for a := 0; a < 10; a++ {
          |       b += a
          |       if (b == 8) {
          |         break;
          |       }
          |       if (b < 3) {
          |         continue;
          |       }
          |       if (b == 5) {
          |          goto End
          |       }
          |   }
          |  End:
          |     b = 9
          |}
          |""".stripMargin)

      inside(cpg.method.name("main").controlStructure.l) {
        case List(forStmt, ifStmtFirst, breakStmt, ifStmtSecond, continueStmt, ifStmtThird, gotoStmt) =>
          breakStmt.controlStructureType shouldBe ControlStructureTypes.BREAK
          breakStmt.code shouldBe "break"

          continueStmt.controlStructureType shouldBe ControlStructureTypes.CONTINUE
          continueStmt.code shouldBe "continue"

          gotoStmt.controlStructureType shouldBe ControlStructureTypes.GOTO
          gotoStmt.code shouldBe "goto End"
      }
    }

  }

  // TODO Need to handle `fallthrough` statements
  "ast creation for fallthrough" ignore {
    "be correct" in {

      val cpg = code("""package main
                       |import "fmt"
                       |func main() {
                       |	num := 2
                       |
                       |	switch num {
                       |	case 1:
                       |		fmt.Println("Number is 1.")
                       |	case 2:
                       |		fmt.Println("Number is 2.")
                       |		fallthrough
                       |	case 3:
                       |		fmt.Println("Number is 3.")
                       |	default:
                       |		fmt.Println("Number is not 1, 2, or 3.")
                       |	}
                       |}""".stripMargin)

      inside(cpg.method("main").controlStructure.l) { case List(switchStmt, fallThrough) =>
        fallThrough.controlStructureType shouldBe "fallthrough"
        fallThrough.code shouldBe "fallthrough"

      }

    }
  }

}
