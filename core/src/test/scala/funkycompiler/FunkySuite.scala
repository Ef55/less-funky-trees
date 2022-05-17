package funkycompiler

import utest.*
import stage3.{ Variable, Tree }
import funky.*
import stdlib.*


object FunkySuite extends TestSuite:
  def testStr(str: String) = str.stripMargin.drop(1)

  override def utestBeforeEach(path: Seq[String]): Unit =
    resetFreshVarCounter()

  val tests = Tests {
    test("if statement") {
      funky {
        val a = Variable[Double]("a")
        val b = Variable[Double]("b")
        If(b > 10){ a := 20 }{ a := 10 }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_3" function="(b &gt; 10.0)" activator="(!evaluationFlag_if_condition_3_4)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_4" function="true" />
        |  <Setter variable="a" function="20.0" activator="((!evaluationFlag_a_1) &amp; if_condition_3)"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="if_condition_3"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="a" function="10.0" activator="((!evaluationFlag_a_2) &amp; (!if_condition_3))"/>
        |  <Setter variable="evaluationFlag_a_2" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="evaluationFlag_a_2" function="true" activator="if_condition_3"/>
        |</Variables>""")
    }

    test("nested if statement") {
      funky {
        val Flaps = Variable[Double]("Flaps")
        val Autotrim = Variable[Double]("Autotrim")

        val maxPitchRate = 0.01
        object flaps:
          val extensionRate = 0.02
          val maxExtension = 30

        If(PitchRate > maxPitchRate){
          If(Flaps < flaps.maxExtension){
            Flaps := Flaps + flaps.extensionRate
          }{
            Autotrim := Autotrim + flaps.extensionRate
          }
        }{
          If(PitchRate < -maxPitchRate){
            Flaps := Flaps - flaps.extensionRate
          }{
            Flaps := Flaps
          }
        }
        
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_7" function="(PitchRate &gt; 0.01)" activator="(!evaluationFlag_if_condition_7_10)"/>
        |  <Setter variable="evaluationFlag_if_condition_7_10" function="true" />
        |  <Setter variable="if_condition_3" function="(Flaps &lt; 30.0)" activator="((!evaluationFlag_if_condition_3_8) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" activator="if_condition_7"/>
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="(((!evaluationFlag_Flaps_1) &amp; if_condition_3) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(if_condition_3 &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="((!if_condition_3) &amp; if_condition_7)"/>
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="(((!evaluationFlag_Autotrim_2) &amp; (!if_condition_3)) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="((!if_condition_3) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(if_condition_3 &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="if_condition_6" function="(PitchRate &lt; -0.01)" activator="((!evaluationFlag_if_condition_6_9) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_if_condition_6_9" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="(((!evaluationFlag_Flaps_4) &amp; if_condition_6) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="(if_condition_6 &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="((!if_condition_6) &amp; (!if_condition_7))"/>
        |  <Setter variable="Flaps" function="Flaps" activator="(((!evaluationFlag_Flaps_5) &amp; (!if_condition_6)) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="((!if_condition_6) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="(if_condition_6 &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_if_condition_6_9" function="true" activator="if_condition_7"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="if_condition_7"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="if_condition_7"/>
        |</Variables>""")
    }

    test("blocks should accumulate assignments and if statements") {
      funky {
        val x = Variable[Double]("x")
        val y = Variable[Double]("y")
        x := 10
        y := 20
        x := 30
        If(x === 30){
          x := 40
        }
        50
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="10.0" activator="(!evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" />
        |  <Setter variable="y" function="20.0" activator="((!evaluationFlag_y_2) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_y_2" function="true" activator="evaluationFlag_x_1"/>
        |  <Setter variable="x" function="30.0" activator="((!evaluationFlag_x_3) &amp; evaluationFlag_y_2)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="evaluationFlag_y_2"/>
        |  <Setter variable="if_condition_5" function="(x = 30.0)" activator="((!evaluationFlag_if_condition_5_6) &amp; evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_if_condition_5_6" function="true" activator="evaluationFlag_x_3"/>
        |  <Setter variable="x" function="40.0" activator="(((!evaluationFlag_x_4) &amp; if_condition_5) &amp; evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_x_4" function="true" activator="(if_condition_5 &amp; evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_x_4" function="true" activator="((!if_condition_5) &amp; evaluationFlag_x_3)"/>
        |</Variables>""")
    }

    test("calls") {
      funky {
        val x = Variable[Double]("x")
        x := abs(25)
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="abs(25.0)" activator="(!evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" />
        |</Variables>""")
    }

    test("loop unrolling") {
      funky {
        val x = Variable[Double]("x")
        for i <- 1 to 5 yield
          x := i
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" activator="(!evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" />
        |  <Setter variable="x" function="2.0" activator="((!evaluationFlag_x_2) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="evaluationFlag_x_1"/>
        |  <Setter variable="x" function="3.0" activator="((!evaluationFlag_x_3) &amp; evaluationFlag_x_2)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="evaluationFlag_x_2"/>
        |  <Setter variable="x" function="4.0" activator="((!evaluationFlag_x_4) &amp; evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_x_4" function="true" activator="evaluationFlag_x_3"/>
        |  <Setter variable="x" function="5.0" activator="((!evaluationFlag_x_5) &amp; evaluationFlag_x_4)"/>
        |  <Setter variable="evaluationFlag_x_5" function="true" activator="evaluationFlag_x_4"/>
        |</Variables>""")
    }

    test("boolean function") {
      def f(x: Tree[Double]): Tree[Boolean] = funky {
        If(x === 0){
          Pitch := 10
          false
        }{
          Pitch := 20
          true
        }
      }

      funky {
        If(f(10)){
          Roll := 1
        }{
          Roll := 20
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_3" function="(10.0 = 0.0)" activator="(!evaluationFlag_if_condition_3_8)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" />
        |  <Setter variable="Pitch" function="10.0" activator="((!evaluationFlag_Pitch_1) &amp; if_condition_3)"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="true" activator="if_condition_3"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_2) &amp; (!if_condition_3))"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="if_condition_3"/>
        |  <Setter variable="if_condition_6" function="((10.0 = 0.0)?false:true)" activator="((!evaluationFlag_if_condition_6_7) &amp; (evaluationFlag_Pitch_1 | evaluationFlag_Pitch_2))"/>
        |  <Setter variable="evaluationFlag_if_condition_6_7" function="true" activator="(evaluationFlag_Pitch_1 | evaluationFlag_Pitch_2)"/>
        |  <Setter variable="Roll" function="1.0" activator="((!evaluationFlag_Roll_4) &amp; if_condition_6)"/>
        |  <Setter variable="evaluationFlag_Roll_4" function="true" activator="if_condition_6"/>
        |  <Setter variable="evaluationFlag_Roll_4" function="true" activator="(!if_condition_6)"/>
        |  <Setter variable="Roll" function="20.0" activator="((!evaluationFlag_Roll_5) &amp; (!if_condition_6))"/>
        |  <Setter variable="evaluationFlag_Roll_5" function="true" activator="(!if_condition_6)"/>
        |  <Setter variable="evaluationFlag_Roll_5" function="true" activator="if_condition_6"/>
        |</Variables>""")
    }

    test("while loop") {
      funky {
        While(Altitude <= 1000){
          Pitch := 20
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_2" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_2_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_4" function="true" />
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_1) &amp; while_condition_2)"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="true" activator="while_condition_2"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="true" activator="(!while_condition_2)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_3" function="evaluationFlag_Pitch_1" />
        |  <Setter variable="evaluationFlag_while_condition_2_4" function="false" activator="(memoised_whileBodyEvaluated_3 &amp; while_condition_2)"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="false" activator="(memoised_whileBodyEvaluated_3 &amp; while_condition_2)"/>
        |</Variables>""")
    }

    test("nested loop") {
      funky {
        While(Altitude <= 1000){
          val x = Variable[Double]("x")
          x := 0
          While(Pitch <= 12345){
            true
          }
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_3" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_3_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_1) &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="while_condition_3"/>
        |  <Setter variable="while_condition_2" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_2_6) &amp; evaluationFlag_x_1) &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_6" function="true" activator="(evaluationFlag_x_1 &amp; while_condition_3)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_5" function="true" activator="(evaluationFlag_x_1 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_6" function="false" activator="(((memoised_whileBodyEvaluated_5 &amp; while_condition_2) &amp; evaluationFlag_x_1) &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="(!while_condition_3)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_6" function="true" activator="(!while_condition_3)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_4" function="(evaluationFlag_x_1 &amp; (!while_condition_2))" />
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_x_1" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_6" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
        |</Variables>""")
    }

    test("embedded nested loop") {
      funky {
        While(Altitude <= 1000){
          val x = Variable[Double]("x")
          x := 0
          While(Pitch <= 12345){
            true
          }
          x := 50
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_4" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_4_8)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_8" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_1) &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="while_condition_4"/>
        |  <Setter variable="while_condition_2" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_2_7) &amp; evaluationFlag_x_1) &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_7" function="true" activator="(evaluationFlag_x_1 &amp; while_condition_4)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_6" function="true" activator="(evaluationFlag_x_1 &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_7" function="false" activator="(((memoised_whileBodyEvaluated_6 &amp; while_condition_2) &amp; evaluationFlag_x_1) &amp; while_condition_4)"/>
        |  <Setter variable="x" function="50.0" activator="(((!evaluationFlag_x_3) &amp; (!while_condition_2)) &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="((!while_condition_2) &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="(!while_condition_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_7" function="true" activator="(!while_condition_4)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="(!while_condition_4)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_5" function="((evaluationFlag_x_1 &amp; (!while_condition_2)) &amp; evaluationFlag_x_3)" />
        |  <Setter variable="evaluationFlag_while_condition_4_8" function="false" activator="(memoised_whileBodyEvaluated_5 &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_x_1" function="false" activator="(memoised_whileBodyEvaluated_5 &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_2_7" function="false" activator="(memoised_whileBodyEvaluated_5 &amp; while_condition_4)"/>
        |  <Setter variable="evaluationFlag_x_3" function="false" activator="(memoised_whileBodyEvaluated_5 &amp; while_condition_4)"/>
        |</Variables>""")  
    }

    test("while nested in if") {
      funky {
        val x = Variable[Double]("x")
        x := 10
        If(x > 5){
          While(x > 0){
            x :-= 1
          }
        }
        x := 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="10.0" activator="(!evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" />
        |  <Setter variable="if_condition_4" function="(x &gt; 5.0)" activator="((!evaluationFlag_if_condition_4_8) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_if_condition_4_8" function="true" activator="evaluationFlag_x_1"/>
        |  <Setter variable="while_condition_3" function="(x &gt; 0.0)" activator="(((!evaluationFlag_while_condition_3_7) &amp; if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="true" activator="(if_condition_4 &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="x" function="(x - 1.0)" activator="((((!evaluationFlag_x_2) &amp; while_condition_3) &amp; if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="((while_condition_3 &amp; if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="(((!while_condition_3) &amp; if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_6" function="evaluationFlag_x_2" activator="(if_condition_4 &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="false" activator="(((memoised_whileBodyEvaluated_6 &amp; while_condition_3) &amp; if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="false" activator="(((memoised_whileBodyEvaluated_6 &amp; while_condition_3) &amp; if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="true" activator="((!if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="((!if_condition_4) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_5) &amp; ((!while_condition_3) &amp; evaluationFlag_x_2))"/>
        |  <Setter variable="evaluationFlag_x_5" function="true" activator="((!while_condition_3) &amp; evaluationFlag_x_2)"/>
        |</Variables>""")
    }

    test("parallel and") {
      val elevators = Variable[Double]("elevators")
      val ailerons = Variable[Double]("ailerons")
      val thrust = Variable[Double]("thrust")
      val levelFlight = funky {
        While(true){
          elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
          thrust := 1
        }
      }
      val wingLevelling = funky {
        While(true){
          ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
        }
      }

      funky {
        levelFlight & wingLevelling
        thrust := 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_3" function="true" activator="(!evaluationFlag_while_condition_3_8)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_8" function="true" />
        |  <Setter variable="elevators" function="(smooth(PID(0.0,(PitchAngle + smooth(AngleOfAttack,0.1)),0.1,0.0,0.1),0.1) + Pitch)" activator="((!evaluationFlag_elevators_1) &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_elevators_1" function="true" activator="while_condition_3"/>
        |  <Setter variable="thrust" function="1.0" activator="(((!evaluationFlag_thrust_2) &amp; evaluationFlag_elevators_1) &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_thrust_2" function="true" activator="(evaluationFlag_elevators_1 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_elevators_1" function="true" activator="(!while_condition_3)"/>
        |  <Setter variable="evaluationFlag_thrust_2" function="true" activator="(!while_condition_3)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_7" function="(evaluationFlag_elevators_1 &amp; evaluationFlag_thrust_2)" />
        |  <Setter variable="evaluationFlag_while_condition_3_8" function="false" activator="(memoised_whileBodyEvaluated_7 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_elevators_1" function="false" activator="(memoised_whileBodyEvaluated_7 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_thrust_2" function="false" activator="(memoised_whileBodyEvaluated_7 &amp; while_condition_3)"/>
        |  <Setter variable="while_condition_5" function="true" activator="(!evaluationFlag_while_condition_5_10)"/>
        |  <Setter variable="evaluationFlag_while_condition_5_10" function="true" />
        |  <Setter variable="ailerons" function="(smooth(PID(0.0,RollRate,0.001,0.0,0.0),0.1) + Roll)" activator="((!evaluationFlag_ailerons_4) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_ailerons_4" function="true" activator="while_condition_5"/>
        |  <Setter variable="evaluationFlag_ailerons_4" function="true" activator="(!while_condition_5)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_9" function="evaluationFlag_ailerons_4" />
        |  <Setter variable="evaluationFlag_while_condition_5_10" function="false" activator="(memoised_whileBodyEvaluated_9 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_ailerons_4" function="false" activator="(memoised_whileBodyEvaluated_9 &amp; while_condition_5)"/>
        |  <Setter variable="thrust" function="0.0" activator="((!evaluationFlag_thrust_6) &amp; (((!while_condition_3) &amp; (evaluationFlag_elevators_1 &amp; evaluationFlag_thrust_2)) &amp; ((!while_condition_5) &amp; evaluationFlag_ailerons_4)))"/>
        |  <Setter variable="evaluationFlag_thrust_6" function="true" activator="(((!while_condition_3) &amp; (evaluationFlag_elevators_1 &amp; evaluationFlag_thrust_2)) &amp; ((!while_condition_5) &amp; evaluationFlag_ailerons_4))"/>
        |</Variables>""")
    }

    // test("parallel or") {
    //   val elevators = Variable[Double]("elevators")
    //   val thrust = Variable[Double]("thrust")
    //   val levelFlight = funky {
    //     While(true){
    //       elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
    //       thrust := 1
    //     }
    //   }
    //   val maintainFor5Seconds = funky {
    //     val t = Variable[Double]("start")
    //     t := Time
    //     While(Time - t < 5){} // Empty while
    //   }

    //   funky {
    //     levelFlight | maintainFor5Seconds
    //     thrust := 0
    //   }.compile ==> testStr("""
    //     |<Variables>
    //     |  <Setter variable="while_condition_7" function="true" activator="((!evaluationFlag_while_condition_7_15) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_while_condition_7_15" function="true" activator="(!(evaluationFlag_start_10 &amp; (!while_condition_11)))"/>
    //     |  <Setter variable="elevators" function="(smooth(PID(0.0,(PitchAngle + smooth(AngleOfAttack,0.1)),0.1,0.0,0.1),0.1) + Pitch)" activator="(((!evaluationFlag_elevators_4) &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_elevators_4" function="true" activator="(while_condition_7 &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="thrust" function="1.0" activator="((((!evaluationFlag_thrust_6) &amp; evaluationFlag_elevators_4) &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_thrust_6" function="true" activator="((evaluationFlag_elevators_4 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_elevators_4" function="true" activator="((!while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_thrust_6" function="true" activator="((!while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="memoised_whileBodyEvaluated_14" function="(evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)" activator="(!(evaluationFlag_start_10 &amp; (!while_condition_11)))"/>
    //     |  <Setter variable="evaluationFlag_while_condition_7_15" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_elevators_4" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_thrust_6" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="start" function="Time" activator="((!evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
    //     |  <Setter variable="evaluationFlag_start_10" function="true" activator="(!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)))"/>
    //     |  <Setter variable="while_condition_11" function="((Time - start) &lt; 5.0)" activator="(((!evaluationFlag_while_condition_11_17) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
    //     |  <Setter variable="evaluationFlag_while_condition_11_17" function="true" activator="(evaluationFlag_start_10 &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
    //     |  <Setter variable="memoised_whileBodyEvaluated_16" function="true" activator="(evaluationFlag_start_10 &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
    //     |  <Setter variable="evaluationFlag_while_condition_11_17" function="false" activator="(((memoised_whileBodyEvaluated_16 &amp; while_condition_11) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
    //     |  <Setter variable="thrust" function="0.0" activator="((!evaluationFlag_thrust_13) &amp; (((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) | (evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
    //     |  <Setter variable="evaluationFlag_thrust_13" function="true" activator="(((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) | (evaluationFlag_start_10 &amp; (!while_condition_11)))"/>
    //     |</Variables>""")
    // }
  }
