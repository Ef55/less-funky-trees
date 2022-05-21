package funkycompiler

import scala.quoted.*
import funkycompiler.{ stage3 => s3 }

object funky 
extends exproc.AstBuilder[s3.Tree] 
with exproc.ControlFlow[s3.Tree]
with exproc.EmptyWhile[s3.Tree]:
  import s3.{ Tree, Block }

  type Variable[T] = s3.Variable[T]

  inline def freshVariable[T]: Variable[T] = 
    freshVar[T]("var")

  inline def initialize[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit] =
    s3.Assignment(va, init)

  inline def assign[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit] =
    s3.Assignment(va, init)

  inline def constant[T](inline t: T): Tree[T] = (inline t match {
    case d: Double => s3.NumericConst(d)
    case b: Boolean => s3.BooleanConst(b)
  }).asInstanceOf[Tree[T]]
    
  inline def combine[T, S](inline l: Tree[T], inline r: Tree[S]): Tree[S] = 
    ((l, r): (Tree[T], Tree[S])) match 
      case (Block(ls, le), Block(rs, re)) => Block(ls ++ (le +: rs), re)
      case (Block(ls, le), r) => Block(ls :+ le, r)
      case (l, Block(rs, re)) => Block(l +: rs, re)
      case (l, r) => Block(List(l), r)

  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T] = 
    // assert(T =:= Unit)
    s3.If(cond, thenn, null)

  inline def ifThenElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T] = 
    s3.If(cond, thenn, elze)

  inline def whileLoop[T](inline cond: Tree[Boolean], inline body: Tree[T]): Tree[Unit] =
    s3.While(cond, body)

  inline def emptyWhileLoop[T](inline cond: Tree[Boolean]): Tree[Unit] =
    s3.While(cond, s3.NumericConst(0))

  given ImplicitElse[Unit] = ???

// inline def funky(inline expr: Any): s3.Tree = ${funkyImpl('expr)}
// def funkyImpl(expr: Expr[Any])(using Quotes): Expr[s3.Tree] =
//   import quotes.reflect.*

//   extension (t: Tree)
//     def is[T: Type] = t match
//       case t: Term => t.tpe <:< TypeRepr.of[T]
//       case _ => false

//     def isS3: Boolean = t.is[s3.Tree] || t.is[Boolean] || t.is[Int] || t.is[Double]

//     def maybeS3: Expr[s3.Tree] | Null =
//       if t.isS3 then t.asS3 else null

//     def asS3: Expr[s3.Tree] = t match
//       case t: Term if t.is[Boolean] => '{booleanToConst(${t.asExprOf[Boolean]})}
//       case t: Term if t.is[Int] => '{intToConst(${t.asExprOf[Int]})}
//       case t: Term if t.is[Double] => '{doubleToConst(${t.asExprOf[Double]})}
//       case t: Term if t.is[Unit] => '{intToConst(0)}
//       case t => t.asExprOf[s3.Tree]

//   end extension

//   object stage3Interpreter extends TreeMap:
//     override def transformTerm(t: Term)(owner: Symbol): Term = t match
//       case Apply(Select(Ident("given_Conversion_Tree_Boolean"), "apply")
//         , s3tree :: Nil) => s3tree

//       case Block(stats, res) =>
//         def treeTermsToBlockTerm(t: Term): Term = t match
//           case t: Term if t.is[Seq[s3.Tree]] =>
//             '{treesToBlock(${t.asExprOf[Seq[s3.Tree]]})}.asTerm
//           case t => t

//         val tStats = transformStats(stats)(owner).map {
//           case t: Term => treeTermsToBlockTerm(t)
//           case s => s
//         }
//         val tRes = treeTermsToBlockTerm(transformTerm(res)(owner)) match
//           case t: Term if t.is[Boolean] | t.is[Int] | t.is[Double] => t.asS3.asTerm
//           case t => t

//         if tStats.exists(_.isS3) then
//           val tStatsRecorded = Expr.ofList((tStats :+ tRes).collect {
//             case s: Term if s.isS3 => s.asS3 })
//           Block.copy(t)(tStats :+ tRes, '{s3.Block($tStatsRecorded)}.asTerm)
//         else Block.copy(t)(tStats, tRes)

//       case If(p, pos, neg) => transformTerm(p)(owner).maybeS3 match
//         case null => super.transformTerm(t)(owner)
//         case tPExpr: Expr[s3.Tree] =>
//           val tPos = transformTerm(pos)(owner).asS3
//           val tNeg = transformTerm(neg)(owner) match
//             case Literal(UnitConstant()) => '{null}
//             case x => x.asS3
//           '{s3.If($tPExpr, $tPos, $tNeg)}.asTerm

//       case While(p, body) => transformTerm(p)(owner).maybeS3 match
//         case null => super.transformTerm(t)(owner)
//         case tPExpr: Expr[s3.Tree] =>
//           val bodyT = transformTerm(body)(owner).asS3
//           '{s3.While($tPExpr, $bodyT)}.asTerm

//       case _ => super.transformTerm(t)(owner)
//   end stage3Interpreter

//   val out = stage3Interpreter.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[s3.Tree]
//   // println(s"Output:\n${out.show}")
//   out
// end funkyImpl
