package tip.analysis

import tip.ast._
import tip.ast.AstNodeData.DeclarationData
import tip.ast.AstPrinters._
import tip.cfg._
import tip.lattices.IntervalLattice
import tip.lattices.IntervalLattice._
import tip.solvers.WorklistFixpointSolverWithReachabilityAndWidening

trait VariableSizeAnalysisWidening extends IntervalAnalysisWidening {

  override val B = Set(MInf, PInf) ++
    (0 to 7).flatMap(x => Set(IntNum((1 << x) - 1), IntNum(- (1 << x)))) ++
    (8 to 16).flatMap(x => Set(IntNum(1 << x), IntNum(- (1 << x)))) ++
    (17 to 31 by 2).flatMap(x => Set(IntNum((1 << x) - 1), IntNum(- (1 << x))))
}

object VariableSizeTyping {

  sealed trait IType {

    def check(inter: IntervalLattice.Element): Boolean
  }

  object BoolIType extends IType {

    override def check(inter: IntervalLattice.Element) = leq(inter, (IntNum(0), IntNum(1)))

    override def toString = "bool"
  }

  object ByteIType extends IType {

    override def check(inter: IntervalLattice.Element) = leq(inter, (IntNum(-128), IntNum(127)))

    override def toString = "byte"
  }

  object CharIType extends IType {

    override def check(inter: IntervalLattice.Element) = leq(inter, (IntNum(0), IntNum(65536)))

    override def toString = "char"
  }

  object IntIType extends IType {

    override def check(inter: IntervalLattice.Element) = leq(inter, (IntNum(-2147483648), IntNum(2147483647)))

    override def toString = "int"
  }

  object BigintIType extends IType {

    override def check(inter: IntervalLattice.Element) = true

    override def toString = "bigint"
  }
  
  val itypes = List(BoolIType, ByteIType, CharIType, IntIType, BigintIType)
  
  def convertInter(inter: IntervalLattice.Element): IType = itypes.filter(_.check(inter)).head

  type ITypeData = Map[AIdentifierDeclaration, IType]

  implicit class AstNodeWithIType(n: AstNode)(implicit val itypeData: ITypeData) {

    private def printer: PartialFunction[AstNode, String] = {
      case id: AIdentifierDeclaration => s"${id.name}: ${itypeData.getOrElse(id, "any")}"
    }

    def toITypedString = n.print(printer)
  }
}

object VariableSizeAnalysis {
  object Intraprocedural {
    class WorklistSolverWithWidening(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
      extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWidening[CfgNode]
        with VariableSizeAnalysisWidening {

      import VariableSizeTyping._

      var itypeData: ITypeData = Map()

      override def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
        n match {
          case CfgFunExitNode(_, _, _, funDecl) =>
            itypeData ++= funDecl.stmts.declarations.flatMap(_.declIds).map(decl => decl -> convertInter(s(decl)))
          case _ =>
        }
        super.localTransfer(n, s)
      }
    }
  }
}
