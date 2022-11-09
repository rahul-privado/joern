package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{
  Engine,
  EngineContext,
  PathElement,
  ReachableByResult,
  SourceToStartingPoints
}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal._

import java.util.concurrent.{
  ForkJoinPool,
  ForkJoinTask,
  RecursiveTask,
  RejectedExecutionException,
  RejectedExecutionHandler
}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class StartingPointWithSource(startingPoint: CfgNode, source: StoredNode)

/** Base class for nodes that can occur in data flows
  */
class ExtendedCfgNode(val traversal: Traversal[CfgNode]) extends AnyVal {

  def ddgIn(implicit semantics: Semantics = DefaultSemantics()): Traversal[CfgNode] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgIn(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def ddgInPathElem(implicit semantics: Semantics = DefaultSemantics()): Traversal[PathElement] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgInPathElem(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def reachableBy[NodeType](sourceTravs: Traversal[NodeType]*)(implicit context: EngineContext): Traversal[NodeType] = {
    val sources = ExtendedCfgNode.sourceTravsToStartingPoints(sourceTravs: _*)
    val reachedSources =
      reachableByInternal(sources).map(_.startingPoint)
    Traversal.from(reachedSources).cast[NodeType]
  }

  def reachableByFlows[A](sourceTravs: Traversal[A]*)(implicit context: EngineContext): Traversal[Path] = {
    val sources        = ExtendedCfgNode.sourceTravsToStartingPoints(sourceTravs: _*)
    val startingPoints = sources.map(_.startingPoint)
    val paths = reachableByInternal(sources)
      .map { result =>
        // We can get back results that start in nodes that are invisible
        // according to the semantic, e.g., arguments that are only used
        // but not defined. We filter these results here prior to returning
        val first = result.path.headOption
        if (first.isDefined && !first.get.visible && !startingPoints.contains(first.get.node)) {
          None
        } else {
          val visiblePathElements = result.path.filter(x => startingPoints.contains(x.node) || x.visible)
          Some(Path(removeConsecutiveDuplicates(visiblePathElements.map(_.node))))
        }
      }
      .filter(_.isDefined)
      .dedup
      .flatten
    paths.to(Traversal)
  }

  def reachableByDetailed[NodeType](
    sourceTravs: Traversal[NodeType]*
  )(implicit context: EngineContext): List[ReachableByResult] = {
    val sources = ExtendedCfgNode.sourceTravsToStartingPoints(sourceTravs: _*)
    reachableByInternal(sources)
  }

  private def removeConsecutiveDuplicates[T](l: Vector[T]): List[T] = {
    l.headOption.map(x => x :: l.sliding(2).collect { case Seq(a, b) if a != b => b }.toList).getOrElse(List())
  }

  private def reachableByInternal(
    startingPointsWithSources: List[StartingPointWithSource]
  )(implicit context: EngineContext): List[ReachableByResult] = {
    val sinks  = traversal.dedup.toList.sortBy(_.id)
    val engine = new Engine(context)
    val result = engine.backwards(sinks, startingPointsWithSources.map(_.startingPoint))

    engine.shutdown()
    val sources               = startingPointsWithSources.map(_.source)
    val startingPointToSource = startingPointsWithSources.map { x => x.startingPoint -> x.source }.toMap
    result.map { r =>
      if (sources.contains(r.startingPoint) || !startingPointToSource(r.startingPoint).isInstanceOf[CfgNode]) {
        r
      } else {
        r.copy(path =
          PathElement(startingPointToSource(r.startingPoint).asInstanceOf[CfgNode], r.callSiteStack.clone()) +: r.path
        )
      }
    }
  }

}

object ExtendedCfgNode {

  private val log = LoggerFactory.getLogger(ExtendedCfgNode.getClass)

  def sourceTravsToStartingPoints[NodeType](sourceTravs: Traversal[NodeType]*): List[StartingPointWithSource] = {
    val fjp = ForkJoinPool.commonPool()
    try {
      fjp.invoke(new SourceTravsToStartingPointsTask(sourceTravs: _*))
    } catch {
      case e: RejectedExecutionException =>
        log.error("Unable to execute 'SourceTravsToStartingPoints` task", e); List()
    } finally {
      fjp.shutdown()
    }
  }
}

class SourceTravsToStartingPointsTask[NodeType](sourceTravs: Traversal[NodeType]*)
    extends RecursiveTask[List[StartingPointWithSource]] {

  private val log = LoggerFactory.getLogger(this.getClass)

  override def compute(): List[StartingPointWithSource] = {
    val sources: List[StoredNode] = sourceTravs
      .flatMap(_.toList)
      .collect { case n: StoredNode => n }
      .dedup
      .toList
      .sortBy(_.id)
    val tasks = sources.map(src => (src, new SourceToStartingPoints(src).fork()))
    tasks.flatMap { case (src, t: ForkJoinTask[List[CfgNode]]) =>
      Try(t.get()) match {
        case Failure(e)       => log.error("Unable to complete 'SourceToStartingPoints' task", e); List()
        case Success(sources) => sources.map(s => StartingPointWithSource(s, src))
      }
    }
  }
}
