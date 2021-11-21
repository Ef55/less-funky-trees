package funkycompiler

import utest._
import testPrograms.*


object Stage3Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage3.mkVarDefs(stage(ifStatement)).toString ==> "VarDefs(List(VarDef(a,Const(20.0),BinaryOp(VarRef(b),Const(10.0),>)), VarDef(a,Const(10.0),UnaryOp(BinaryOp(VarRef(b),Const(10.0),>),!))))"
    }

    test("nested if statement") {
      stage3.mkVarDefs(stage(nestedIfStatement)).toString ==> "VarDefs(List(VarDef(Flaps,BinaryOp(VarRef(Flaps),Const(0.02),+),BinaryOp(BinaryOp(VarRef(PitchRate),Const(0.01),>),BinaryOp(VarRef(Flaps),Const(30.0),<),&)), VarDef(Autotrim,BinaryOp(VarRef(Autotrim),Const(0.02),+),BinaryOp(BinaryOp(VarRef(PitchRate),Const(0.01),>),UnaryOp(BinaryOp(VarRef(Flaps),Const(30.0),<),!),&)), VarDef(Flaps,BinaryOp(VarRef(Flaps),Const(0.02),-),BinaryOp(UnaryOp(BinaryOp(VarRef(PitchRate),Const(0.01),>),!),BinaryOp(VarRef(PitchRate),Const(-0.01),<),&)), VarDef(Flaps,VarRef(Flaps),BinaryOp(UnaryOp(BinaryOp(VarRef(PitchRate),Const(0.01),>),!),UnaryOp(BinaryOp(VarRef(PitchRate),Const(-0.01),<),!),&))))"
    }

    test("blocks should accumulate assignments and if statements") {
      stage3.mkVarDefs(stage(blocksAccumulation)).toString ==> "VarDefs(List(VarDef(x,Const(10.0),null), VarDef(y,Const(20.0),null), VarDef(x,Const(30.0),null), VarDef(x,Const(40.0),BinaryOp(VarRef(x),Const(30.0),==))))"
    }

    test("calls") {
      stage3.mkVarDefs(stage(calls)).toString ==> "VarDefs(List(VarDef(x,Call(abs,List(Const(25.0))),null)))"
    }
  }
