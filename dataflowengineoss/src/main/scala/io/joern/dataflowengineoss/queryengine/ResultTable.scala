package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Expression, StoredNode}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** We can only reuse results if we have calculated them for the same context, and therefore, the key must capture the
  * entire context. Currently, the context consists of the sink node and the call site stack upon invoking the task.
  */
case class TaskFingerprint(sink: StoredNode, callSiteStack: List[Call] = List())

/** The Result Table is a cache that allows retrieving known paths for nodes, that is, paths that end in the node.
  */
class ResultTable(
  val table: mutable.Map[TaskFingerprint, Vector[ReachableByResult]] =
    new java.util.concurrent.ConcurrentHashMap[TaskFingerprint, Vector[ReachableByResult]].asScala
) {

  /** Add all results in `results` to table at `key`, appending to existing results.
    */
  def add(key: TaskFingerprint, results: Vector[ReachableByResult]): Unit = {
    table.asJava.compute(
      key,
      { (_, existingValue) =>
        Option(existingValue).toVector.flatten ++ results
      }
    )
  }

  /** For a given path, determine whether results for the first element (`first`) are stored in the table, and if so,
    * for each result, determine the path up to `first` and prepend it to `path`, giving us new results via table
    * lookup.
    */
  def createFromTable(
    first: PathElement,
    callSiteStack: List[Call],
    remainder: Vector[PathElement]
  ): Option[Vector[ReachableByResult]] = {
    table.get(TaskFingerprint(first.node, callSiteStack)).map { res =>
      res.map { r =>
        val pathToFirstNode = r.path.slice(0, r.path.map(_.node).indexOf(first.node))
        val completePath    = pathToFirstNode ++ (first +: remainder)
        r.copy(path = Vector(completePath.head) ++ completePath.tail)
      }
    }
  }

  /** Retrieve list of results for `node` or None if they are not available in the table.
    */
  def get(key: TaskFingerprint): Option[Vector[ReachableByResult]] = {
    table.get(key)
  }

  /** Returns all keys to allow for iteration through the table.
    */
  def keys(): Vector[TaskFingerprint] = table.keys.toVector

}

/** A (partial) result, informing about a path that exists from a source to another node in the graph.
  *
  * @param fingerprint
  *   this is the task fingerprint, that is, the task that was solved
  * @param path
  *   this is the main result - a known path
  * @param partial
  *   indicate whether this result stands on its own or requires further analysis, e.g., by expanding output arguments
  *   backwards into method output parameters.
  */
case class ReachableByResult(
  fingerprint: TaskFingerprint,
  path: Vector[PathElement],
  parentTasks: List[TaskFingerprint],
  callDepth: Int = 0,
  partial: Boolean = false
) {
  def startingPoint: CfgNode = path.head.node

  /** If the result begins in an output argument, return it.
    */
  def outputArgument: Option[CfgNode] = {
    path.headOption.collect {
      case elem: PathElement if elem.isOutputArg =>
        elem.node
    }
  }
}

/** We represent data flows as sequences of path elements, where each path element consists of a node, flags and the
  * label of its outgoing edge.
  *
  * @param node
  *   The parent node
  *
  * @param callSiteStack
  *   The call stack when this path element was created. Since we may enter the same function via two different call
  *   sites, path elements should only be treated as the same if they are the same node and we've reached them via the
  *   same call sequence.
  *
  * @param visible
  *   whether this path element should be shown in the flow
  * @param isOutputArg
  *   input and output arguments are the same node in the CPG, so, we need this additional flag to determine whether we
  *   are on an input or output argument. By default, we consider arguments to be input arguments, meaning that when
  *   tracking `x` at `f(x)`, we do not expand into `f` but rather upwards to producers of `x`.
  * @param outEdgeLabel
  *   label of the outgoing DDG edge
  */
case class PathElement(
  node: CfgNode,
  callSiteStack: List[Call] = List(),
  visible: Boolean = true,
  isOutputArg: Boolean = false,
  outEdgeLabel: String = ""
)
