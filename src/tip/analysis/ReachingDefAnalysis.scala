package tip.analysis

import tip.ast._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.solvers._
import tip.cfg._

/**
  * Base class for reaching defenitions analysis.
  */
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {

  val lattice: MapLattice[CfgNode, PowersetLattice[AStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def removerefs(s: lattice.sublattice.Element, x: ADeclaration): lattice.sublattice.Element =
    s.filter {
      case AVarStmt(declIds, _) => declIds.head != x
      case AAssignStmt(id: AIdentifier, _, _) => declData(id) != x
      case _ => ???
    }

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case AVarStmt(declIds, loc) => declIds.map(id => AVarStmt(List(id), loc)).toSet
          case as: AAssignStmt => removerefs(s, as.left.asInstanceOf[AIdentifier]) + as
          case _ => s
        }
      case _ => s
    }
}

/**
  * Reaching defenitions analysis that uses the simple fixpoint solver.
  */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
  extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies
