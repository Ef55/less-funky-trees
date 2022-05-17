package funkycompiler

import funkycompiler.{ stage4 => s4 }


object stage3:
  sealed trait Function(val name: String)
  class Function1[T, R](name: String) extends Function(name: String):
    def apply(arg1: Tree[T]) = Call1(this, arg1)
  class Function2[T, U, R](name: String) extends Function(name: String):
    def apply(arg1: Tree[T], arg2: Tree[U]) = Call2(this, arg1, arg2)
  class Function3[T, U, V, R](name: String) extends Function(name: String):
    def apply(arg1: Tree[T], arg2: Tree[U], arg3: Tree[V]) = Call3(this, arg1, arg2, arg3)
  class Function4[T, U, V, W, R](name: String) extends Function(name: String):
    def apply(arg1: Tree[T], arg2: Tree[U], arg3: Tree[V], arg4: Tree[W]) = Call4(this, arg1, arg2, arg3, arg4)
  class Function5[T, U, V, W, X, R](name: String) extends Function(name: String):
    def apply(arg1: Tree[T], arg2: Tree[U], arg3: Tree[V], arg4: Tree[W], arg5: Tree[X]) = Call5(this, arg1, arg2, arg3, arg4, arg5)

  sealed trait Tree[+T]
  sealed trait MemoizedCondition[T](prefix: String) extends Tree[T]:
    val condition: Tree[Boolean]
    val memoisedCondition = s4.VarRef(freshVarName(s"${prefix}_condition_"))
    def updateMemoisedCondition = Assignment(Variable(memoisedCondition.name), condition)

  case class BooleanConst(value: Boolean) extends Tree[Boolean]
  case class NumericConst(value: Double) extends Tree[Double]
  case class Variable[T](name: String) extends Tree[T]
  sealed trait Call[R] extends Tree[R]:
    def fun: Function = this match 
      case Call1(f, _) => f
      case Call2(f, _, _) => f
      case Call3(f, _, _, _) => f
      case Call4(f, _, _, _, _) => f
      case Call5(f, _, _, _, _, _) => f

    def args: List[Tree[?]] = this match 
      case Call1(_, arg1) => List(arg1)
      case Call2(_, arg1, arg2) => List(arg1, arg2)
      case Call3(_, arg1, arg2, arg3) => List(arg1, arg2, arg3)
      case Call4(_, arg1, arg2, arg3, arg4) => List(arg1, arg2, arg3, arg4)
      case Call5(_, arg1, arg2, arg3, arg4, arg5) => List(arg1, arg2, arg3, arg4, arg5)
  case class Call1[T, R](function: Function1[T, R], arg: Tree[T]) extends Call[R]
  case class Call2[T, U, R](function: Function2[T, U, R], arg1: Tree[T ], arg2: Tree[U]) extends Call[R]
  case class Call3[T, U, V, R](function: Function3[T, U, V, R], arg1: Tree[T], arg2: Tree[U], arg3: Tree[V]) extends Call[R]
  case class Call4[T, U, V, W, R](function: Function4[T, U, V, W, R], arg1: Tree[T], arg2: Tree[U], arg3: Tree[V], arg4: Tree[W]) extends Call[R]
  case class Call5[T, U, V, W, X, R](function: Function5[T, U, V, W, X, R], arg1: Tree[T], arg2: Tree[U], arg3: Tree[V], arg4: Tree[W], arg5: Tree[X]) extends Call[R]
  case class BinaryOp[T, R](lhs: Tree[T], rhs: Tree[T], sign: String) extends Tree[R]
  case class UnaryOp[T](rhs: Tree[T], sign: String) extends Tree[T]
  case class If[T](condition: Tree[Boolean], lhs: Tree[T], rhs: Tree[T] | Null) extends MemoizedCondition[T]("if")
  case class Block[T](stats: List[Tree[?]], last: Tree[T]) extends Tree[T]
  case class While(condition: Tree[Boolean], body: Tree[?]) extends MemoizedCondition[Unit]("while")
  case class ParallelAnd(stats: List[Tree[?]]) extends Tree[Unit]
  case class ParallelOr(stats: List[Tree[?]]) extends Tree[Unit]

  case class Assignment[T](lhs: Variable[T], rhs: Tree[T]) extends Tree[Unit]:
    val evaluationFlag = freshEvaluationFlag(lhs.name)
  
  extension (t: Tree[?])
    /**
     * A sequence of variable definitions that represent `t` on
     * the Funky Trees level. It is guaranteed that these variable
     * definitions are evaluated only once across all frame updates.
     * For loops, 'only once' means 'as many times as the
     * loop parameters permit the loop to run, but not more'.
     * It is possible to reevaluate these defs explicitly via `t.allowReevaluation`.
     */
    def defs: List[s4.VarDef] = t match
      case BooleanConst(_) | NumericConst(_) => Nil
      case Variable(n) => Nil
      case c: Call[?] => c.args.flatMap(_.defs)
      case BinaryOp(lhs, rhs, sign) => lhs.defs ++ rhs.defs.whileAfter(lhs)
      case UnaryOp(rhs, sign) => rhs.defs
      case t@Assignment(Variable(name), rhs) =>
        rhs.defs ++ List(
          s4.VarDef(name, rhs.result, !t.evaluationFlag),
          s4.VarDef(t.evaluationFlag.name, s4.Const(true), s4.Const(true))).whileAfter(rhs)
      case t@If(cnd, lhs, rhs) =>
        val lhsDefs = lhs.defs
        val rhsDefs = if rhs != null then rhs.defs else Nil
        t.updateMemoisedCondition.defs ++
          lhsDefs.whileTrue(t.memoisedCondition) ++
          lhsDefs.disableEvaluation.whileTrue(!t.memoisedCondition) ++
          rhsDefs.whileTrue(!t.memoisedCondition) ++
          rhsDefs.disableEvaluation.whileTrue(t.memoisedCondition)
      case Block(ts, expr) => defsOneAfterAnother(ts :+ expr)
      case ParallelAnd(ts) => ts.flatMap(_.defs)
      case ParallelOr(ts) => ts.flatMap { t => t.defs.whileTrue(!ts.filter(_ != t).atLeastOneEvaluated) }
      case t@While(cnd, body) =>
        // Memoise body evaluation status because allowReevaluation will change it before it has a chance to complete
        val memoisedBodyEvaluatedName = freshVarName(s"memoised_whileBodyEvaluated_")
        val bodyDefs = body.defs
        val loopedDefs =
          t.updateMemoisedCondition.defs ++
            bodyDefs.whileTrue(t.memoisedCondition) ++
            bodyDefs.disableEvaluation.whileTrue(!t.memoisedCondition)
        val controlFlowDefs =
          s4.VarDef(memoisedBodyEvaluatedName, body.evaluated, s4.Const(true)) ::
          loopedDefs.allowReevaluation.whileTrue(s4.VarRef(memoisedBodyEvaluatedName) && t.memoisedCondition)
        loopedDefs ++ controlFlowDefs

    /**
     * Defines what it means for a tree to be fully evaluated.
     * Returns an expression that is true if and only if `t` has been
     * fully evaluated and its `result` is fully computed.
     */
    def evaluated: s4.Expr = t match
      case BooleanConst(_) | NumericConst(_) | Variable(_) => s4.Const(true)
      case c: Call[?] => c.args.allEvaluated
      case UnaryOp(rhs, _) => rhs.evaluated
      case BinaryOp(lhs, rhs, _) => lhs.evaluated && rhs.evaluated
      case t@Assignment(_, rhs) => rhs.evaluated && t.evaluationFlag
      case If(cnd, lhs, rhs) => cnd.evaluated && (lhs.evaluated ||
        (if rhs != null then rhs.evaluated else s4.Const(false)))
      case Block(ts, expr) => ts.allEvaluated && expr.evaluated
      case t@While(cnd, ts) => cnd.evaluated && !t.memoisedCondition && ts.evaluated
      case ParallelAnd(ts) => ts.allEvaluated
      case ParallelOr(ts) => ts.atLeastOneEvaluated

    def result: s4.Expr = t match
      case BooleanConst(x) => s4.Const(x)
      case NumericConst(x) => s4.Const(x)
      case Variable(n) => s4.VarRef(n)
      case c: Call[?] => s4.Call(c.fun.name, c.args.map(_.result))
      case BinaryOp(lhs, rhs, sign) => s4.BinaryOp(lhs.result, rhs.result, sign)
      case UnaryOp(rhs, sign) => s4.UnaryOp(rhs.result, sign)
      case Assignment(Variable(name), _) => s4.VarRef(name)
      case If(cnd, lhs, rhs) => s4.If(cnd.result, lhs.result,
        if rhs != null then rhs.result else null)
      case Block(_, expr) => expr.result
      case ParallelAnd(ts) => ts.last.result
      case ParallelOr(ts) => ts.foldLeft(s4.Const(-1): s4.Expr) { case (accum, t) =>
          s4.If(t.evaluated, t.result, accum) }
      case While(cnd, ts) => ts.result

    def compile = s4.VarDefs(t.defs).simplify.toXml
  end extension

  extension (vd: s4.VarDef)
    /** Evaluate `vd` only if `t` is evaluated. */
    def whileAfter(t: Tree[?]): s4.VarDef = vd.whileTrue(t.evaluated)

    /**
     * Evaluate `vd` only when `cnd`'s result is `true`.
     * When calling it, ensure that `vd` does not change `cnd`
     * (use memoisation if such a change is needed).
     */
    def whileTrue(cnd: s4.Expr): s4.VarDef =
      vd.copy(condition = vd.condition && cnd)
  end extension

  def freshEvaluationFlag(prefix: String) =
    s4.VarRef(freshVarName(s"evaluationFlag_${prefix}_"))

  def defsOneAfterAnother(ts: List[Tree[?]]) =
    ts.head.defs ++ (
      if ts.length > 1 then ts.sliding(2).flatMap {
        case prev :: next :: Nil => next.defs.whileAfter(prev) }
      else Nil)


  extension (vds: List[s4.VarDef])
    def whileAfter(t: Tree[?]): List[s4.VarDef] = vds.map(_.whileAfter(t))
    def whileTrue(cnd: s4.Expr): List[s4.VarDef] = vds.map(_.whileTrue(cnd))

    /** Guarantees that the given tree's vardefs aren't going to be evaluated. */
    def disableEvaluation: List[s4.VarDef] = setEvaluationFlag(true)

    /**
     * Reset all the variable evaluation flags, thus allowing them
     * to be computed one more time.
     */
    def allowReevaluation: List[s4.VarDef] = setEvaluationFlag(false)

    def setEvaluationFlag(value: Boolean): List[s4.VarDef] =
      vds.collect { case s4.VarDef(name, _, _) if name.startsWith("evaluationFlag_") =>
        s4.VarDef(name, s4.Const(value), s4.Const(true)) }.distinct

  extension (ts: List[Tree[?]])
    def allEvaluated: s4.Expr = ts.foldLeft(s4.Const(true): s4.Expr) {
      (accum, t) => accum && t.evaluated }
    def atLeastOneEvaluated: s4.Expr = ts.foldLeft(s4.Const(ts.isEmpty): s4.Expr) {
      (accum, t) => accum || t.evaluated }
