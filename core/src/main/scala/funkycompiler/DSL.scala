package funkycompiler

import java.io.File

import stage3.*

given doubleToConst: Conversion[Double, Tree[Double]] = NumericConst(_)
given intToConst: Conversion[Int, Tree[Double]] = NumericConst(_)
given booleanToConst: Conversion[Boolean, Tree[Boolean]] = BooleanConst(_)
given treesToBlock[T]: Conversion[Seq[Tree[T]], Block[T]] = ts => {
  require(!ts.isEmpty)
  val sts :+ expr = ts
  Block(sts.toList, expr)
}

extension (n: Tree[Double])
  def +(t: Tree[Double]) = BinaryOp[Double, Double](n, t, "+")
  def -(t: Tree[Double]) = BinaryOp[Double, Double](n, t, "-")
  def *(t: Tree[Double]) = BinaryOp[Double, Double](n, t, "*")
  def /(t: Tree[Double]) = BinaryOp[Double, Double](n, t, "/")

  def <(t: Tree[Double]) = BinaryOp[Double, Boolean](n, t, "&lt;")
  def >(t: Tree[Double]) = BinaryOp[Double, Boolean](n, t, "&gt;")
  def <=(t: Tree[Double]) = BinaryOp[Double, Boolean](n, t, "&lt;=")
  def >=(t: Tree[Double]) = BinaryOp[Double, Boolean](n, t, "&gt;=")
  def !==(t: Tree[Double]) = BinaryOp[Double, Boolean](n, t, "!=")
  def ===(t: Tree[Double]) = BinaryOp[Double, Boolean](n, t, "=")

  def unary_- = NumericConst(0) - n
end extension

extension (n: Tree[Boolean])

  def &&(t: Tree[Boolean]) = BinaryOp(n, t, "&amp;")
  def ||(t: Tree[Boolean]) = BinaryOp(n, t, "|")

  def unary_! = UnaryOp(n, "!")
end extension

extension (n: Tree[?])
  def & (t: Tree[?]) = ParallelAnd(n :: t :: Nil)
  def | (t: Tree[?]) = ParallelOr (n :: t :: Nil)
end extension

extension [T] (v: Variable[T])
  def :=(value: Tree[T]) = Assignment(v, value)
end extension

extension (v: Variable[Double])
  def :+=(value: Tree[Double]) = v := v + value
  def :-=(value: Tree[Double]) = v := v - value
  def :/=(value: Tree[Double]) = v := v / value
  def :*=(value: Tree[Double]) = v := v * value
end extension

extension [T, R] (f: Function1[T, R]) def apply(arg1: Tree[T]) =
  Call1(f, arg1)

extension [T, U, R] (f: Function2[T, U, R]) def apply(arg1: Tree[T], arg2: Tree[U]) =
  Call2(f, arg1, arg2)

extension [T, U, V, R] (f: Function3[T, U, V, R]) def apply(arg1: Tree[T], arg2: Tree[U], arg3: Tree[V]) =
  Call3(f, arg1, arg2, arg3)


extension (ts: Seq[Tree[Double]]) def sumTrees =
  ts.foldLeft(0: Tree[Double]) { case (accum, t) => accum + t }

inline def program(plane: File, debugTrees: Boolean = false)(inline expr: Tree[Any]): Unit =
  val tree = funky(expr)
  if debugTrees then println(tree)
  val xml = tree.compile
  writeVariables(plane, xml)

private var freshVarCounter = 0
def resetFreshVarCounter() = freshVarCounter = 0
def freshVarName(prefix: String = "syntheticVar") =
  freshVarCounter += 1
  s"${prefix}${freshVarCounter}"

def freshVar[T](prefix: String = "syntheticVar") =
  Variable[T](freshVarName(prefix))

def defineVariable[T](initialValue: Tree[T]) = ??? // todo
