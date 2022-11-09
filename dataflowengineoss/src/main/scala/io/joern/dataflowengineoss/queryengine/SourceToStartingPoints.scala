package io.joern.dataflowengineoss.queryengine

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  CfgNode,
  Expression,
  FieldIdentifier,
  Identifier,
  Literal,
  Member,
  StoredNode,
  TypeDecl
}
import overflowdb.traversal._
import io.shiftleft.semanticcpg.language._

import java.util.concurrent.RecursiveTask

/** The code below deals with member variables, and specifically with the situation where literals that initialize
  * static members are passed to `reachableBy` as sources. In this case, we determine the first usages of this member in
  * each method, traversing the AST from left to right. This isn't fool-proof, e.g., goto-statements would be
  * problematic, but it works quite well in practice.
  */
class SourceToStartingPoints(src: StoredNode) extends RecursiveTask[List[CfgNode]] {

  override def compute(): List[CfgNode] =
    src match {
      case lit: Literal =>
        List(lit) ++ usages(targetsToClassIdentifierPair(literalToInitializedMembers(lit)))
      case member: Member =>
        val initializedMember = memberToInitializedMembers(member)
        usages(targetsToClassIdentifierPair(initializedMember))
      case x => List(x).collect { case y: CfgNode => y }
    }

  private def usages(pairs: List[(TypeDecl, Expression)]): List[CfgNode] = {
    pairs.flatMap { case (typeDecl, expression) =>
      val cpg = Cpg(typeDecl.graph())
      val usagesInSameClass =
        typeDecl.method
          .whereNot(_.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName))
          .flatMap { m =>
            expression match {
              case identifier: Identifier =>
                m.ast.isIdentifier.nameExact(identifier.name).takeWhile(notLeftHandOfAssignment)
              case fieldIdentifier: FieldIdentifier =>
                m.ast.isFieldIdentifier
                  .canonicalNameExact(fieldIdentifier.canonicalName)
                  .takeWhile(notLeftHandOfAssignment)
              case _ => List()
            }
          }
          .headOption
      val usagesInOtherClasses = cpg.method.flatMap { m =>
        m.fieldAccess
          .where(_.argument(1).isIdentifier.typeFullNameExact(typeDecl.fullName))
          .where { x =>
            expression match {
              case identifier: Identifier =>
                x.argument(2).isFieldIdentifier.canonicalNameExact(identifier.name)
              case fieldIdentifier: FieldIdentifier =>
                x.argument(2).isFieldIdentifier.canonicalNameExact(fieldIdentifier.canonicalName)
              case _ => List()
            }
          }
          .takeWhile(notLeftHandOfAssignment)
          .headOption
      }
      usagesInSameClass ++ usagesInOtherClasses
    }
  }

  /** For a literal, determine if it is used in the initialization of any member variables. Return list of initialized
    * members. An initialized member is either an identifier or a field-identifier.
    */
  private def literalToInitializedMembers(lit: Literal): List[Expression] = {
    lit.inAssignment
      .where(_.method.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName))
      .target
      .flatMap {
        case identifier: Identifier => List(identifier)
        case call: Call if call.name == Operators.fieldAccess =>
          call.ast.isFieldIdentifier.l
        case _ => List[Expression]()
      }
      .l
  }

  private def memberToInitializedMembers(member: Member): List[Expression] = {
    member.typeDecl.method
      .nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName)
      .ast
      .flatMap { x =>
        x match {
          case identifier: Identifier if identifier.name == member.name =>
            Traversal(identifier).argumentIndex(1).where(_.inAssignment).l
          case fieldIdentifier: FieldIdentifier if fieldIdentifier.canonicalName == member.head.name =>
            Traversal(fieldIdentifier).where(_.inAssignment).l
          case _ => List[Expression]()
        }
      }
      .l
  }

  private def notLeftHandOfAssignment(x: Expression): Boolean = {
    !(x.argumentIndex == 1 && x.inAssignment.nonEmpty)
  }

  private def targetsToClassIdentifierPair(targets: List[Expression]): List[(TypeDecl, Expression)] = {
    targets.flatMap(target => target.method.typeDecl.map { typeDecl => (typeDecl, target) })
  }
}
