package funkycompiler

import utest.*
import stage3.{ Variable, Tree }
import funky.*
import stdlib.*


object FunkySuite extends TestSuite:
  def testStr(str: String) = str.stripMargin.drop(1)

  override def utestBeforeEach(path: Seq[String]): Unit =
    resetFreshVarCounters()

  val tests = Tests {
    test("if statement") {
      funky {
        var a = Variable[Double]("a")
        var b = Variable[Double]("b")
        If(b > 10){ a =! 20 }{ a =! 10 }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_0" function="(b &gt; 10.0)" activator="(!evaluationFlag_if_condition_0_0)"/>
        |  <Setter variable="evaluationFlag_if_condition_0_0" function="true" />
        |  <Setter variable="a" function="20.0" activator="((!evaluationFlag_a_0) &amp; if_condition_0)"/>
        |  <Setter variable="evaluationFlag_a_0" function="true" activator="if_condition_0"/>
        |  <Setter variable="evaluationFlag_a_0" function="true" activator="(!if_condition_0)"/>
        |  <Setter variable="a" function="10.0" activator="((!evaluationFlag_a_1) &amp; (!if_condition_0))"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="(!if_condition_0)"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="if_condition_0"/>
        |</Variables>""")
    }

    test("nested if statement") {
      funky {
        var Flaps = Variable[Double]("Flaps")
        var Autotrim = Variable[Double]("Autotrim")

        val maxPitchRate = 0.01
        object flaps:
          val extensionRate = 0.02
          val maxExtension = 30

        If(PitchRate > maxPitchRate){
          If(Flaps < flaps.maxExtension){
            Flaps =! Flaps + flaps.extensionRate
          }{
            Autotrim =! Autotrim + flaps.extensionRate
          }
        }{
          If(PitchRate < -maxPitchRate){
            Flaps =! Flaps - flaps.extensionRate
          }{
            Flaps =! Flaps
          }
        }
        
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_2" function="(PitchRate &gt; 0.01)" activator="(!evaluationFlag_if_condition_2_0)"/>
        |  <Setter variable="evaluationFlag_if_condition_2_0" function="true" />
        |  <Setter variable="if_condition_0" function="(Flaps &lt; 30.0)" activator="((!evaluationFlag_if_condition_0_0) &amp; if_condition_2)"/>
        |  <Setter variable="evaluationFlag_if_condition_0_0" function="true" activator="if_condition_2"/>
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="(((!evaluationFlag_Flaps_0) &amp; if_condition_0) &amp; if_condition_2)"/>
        |  <Setter variable="evaluationFlag_Flaps_0" function="true" activator="(if_condition_0 &amp; if_condition_2)"/>
        |  <Setter variable="evaluationFlag_Flaps_0" function="true" activator="((!if_condition_0) &amp; if_condition_2)"/>
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="(((!evaluationFlag_Autotrim_0) &amp; (!if_condition_0)) &amp; if_condition_2)"/>
        |  <Setter variable="evaluationFlag_Autotrim_0" function="true" activator="((!if_condition_0) &amp; if_condition_2)"/>
        |  <Setter variable="evaluationFlag_Autotrim_0" function="true" activator="(if_condition_0 &amp; if_condition_2)"/>
        |  <Setter variable="evaluationFlag_if_condition_0_0" function="true" activator="(!if_condition_2)"/>
        |  <Setter variable="evaluationFlag_Flaps_0" function="true" activator="(!if_condition_2)"/>
        |  <Setter variable="evaluationFlag_Autotrim_0" function="true" activator="(!if_condition_2)"/>
        |  <Setter variable="if_condition_1" function="(PitchRate &lt; -0.01)" activator="((!evaluationFlag_if_condition_1_0) &amp; (!if_condition_2))"/>
        |  <Setter variable="evaluationFlag_if_condition_1_0" function="true" activator="(!if_condition_2)"/>
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="(((!evaluationFlag_Flaps_1) &amp; if_condition_1) &amp; (!if_condition_2))"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(if_condition_1 &amp; (!if_condition_2))"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="((!if_condition_1) &amp; (!if_condition_2))"/>
        |  <Setter variable="Flaps" function="Flaps" activator="(((!evaluationFlag_Flaps_2) &amp; (!if_condition_1)) &amp; (!if_condition_2))"/>
        |  <Setter variable="evaluationFlag_Flaps_2" function="true" activator="((!if_condition_1) &amp; (!if_condition_2))"/>
        |  <Setter variable="evaluationFlag_Flaps_2" function="true" activator="(if_condition_1 &amp; (!if_condition_2))"/>
        |  <Setter variable="evaluationFlag_if_condition_1_0" function="true" activator="if_condition_2"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="if_condition_2"/>
        |  <Setter variable="evaluationFlag_Flaps_2" function="true" activator="if_condition_2"/>
        |</Variables>""")
    }

    test("blocks should accumulate assignments and if statements") {
      funky {
        var x = ! 10
        var y = ! 20
        x =! 30
        If(x === 30){
          x =! 40
        }
        50
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="var0" function="10.0" activator="(!evaluationFlag_var0_0)"/>
        |  <Setter variable="evaluationFlag_var0_0" function="true" />
        |  <Setter variable="var1" function="20.0" activator="((!evaluationFlag_var1_0) &amp; evaluationFlag_var0_0)"/>
        |  <Setter variable="evaluationFlag_var1_0" function="true" activator="evaluationFlag_var0_0"/>
        |  <Setter variable="var0" function="30.0" activator="((!evaluationFlag_var0_1) &amp; evaluationFlag_var1_0)"/>
        |  <Setter variable="evaluationFlag_var0_1" function="true" activator="evaluationFlag_var1_0"/>
        |  <Setter variable="if_condition_0" function="(var0 = 30.0)" activator="((!evaluationFlag_if_condition_0_0) &amp; evaluationFlag_var0_1)"/>
        |  <Setter variable="evaluationFlag_if_condition_0_0" function="true" activator="evaluationFlag_var0_1"/>
        |  <Setter variable="var0" function="40.0" activator="(((!evaluationFlag_var0_2) &amp; if_condition_0) &amp; evaluationFlag_var0_1)"/>
        |  <Setter variable="evaluationFlag_var0_2" function="true" activator="(if_condition_0 &amp; evaluationFlag_var0_1)"/>
        |  <Setter variable="evaluationFlag_var0_2" function="true" activator="((!if_condition_0) &amp; evaluationFlag_var0_1)"/>
        |</Variables>""")
    }

    test("calls") {
      funky {
        val x = ! abs(25)
        x
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="var0" function="abs(25.0)" activator="(!evaluationFlag_var0_0)"/>
        |  <Setter variable="evaluationFlag_var0_0" function="true" />
        |</Variables>""")
    }

    test("loop unrolling") {
      funky {
        var x = Variable[Double]("x")
        for i <- 1 to 5 yield
          x =! i
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" activator="(!evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_0" function="true" />
        |  <Setter variable="x" function="2.0" activator="((!evaluationFlag_x_1) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="evaluationFlag_x_0"/>
        |  <Setter variable="x" function="3.0" activator="((!evaluationFlag_x_2) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="evaluationFlag_x_1"/>
        |  <Setter variable="x" function="4.0" activator="((!evaluationFlag_x_3) &amp; evaluationFlag_x_2)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="evaluationFlag_x_2"/>
        |  <Setter variable="x" function="5.0" activator="((!evaluationFlag_x_4) &amp; evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_x_4" function="true" activator="evaluationFlag_x_3"/>
        |</Variables>""")
    }

    test("boolean function") {
      def f(x: Tree[Double]): Tree[Boolean] = funky {
        If(x === 0){
          Pitch =! 10
          false
        }{
          Pitch =! 20
          true
        }
      }

      funky {
        If(f(10)){
          Roll =! 1
        }{
          Roll =! 20
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_0" function="(10.0 = 0.0)" activator="(!evaluationFlag_if_condition_0_0)"/>
        |  <Setter variable="evaluationFlag_if_condition_0_0" function="true" />
        |  <Setter variable="Pitch" function="10.0" activator="((!evaluationFlag_Pitch_0) &amp; if_condition_0)"/>
        |  <Setter variable="evaluationFlag_Pitch_0" function="true" activator="if_condition_0"/>
        |  <Setter variable="evaluationFlag_Pitch_0" function="true" activator="(!if_condition_0)"/>
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_1) &amp; (!if_condition_0))"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="true" activator="(!if_condition_0)"/>
        |  <Setter variable="evaluationFlag_Pitch_1" function="true" activator="if_condition_0"/>
        |  <Setter variable="if_condition_1" function="((10.0 = 0.0)?false:true)" activator="((!evaluationFlag_if_condition_1_0) &amp; (evaluationFlag_Pitch_0 | evaluationFlag_Pitch_1))"/>
        |  <Setter variable="evaluationFlag_if_condition_1_0" function="true" activator="(evaluationFlag_Pitch_0 | evaluationFlag_Pitch_1)"/>
        |  <Setter variable="Roll" function="1.0" activator="((!evaluationFlag_Roll_0) &amp; if_condition_1)"/>
        |  <Setter variable="evaluationFlag_Roll_0" function="true" activator="if_condition_1"/>
        |  <Setter variable="evaluationFlag_Roll_0" function="true" activator="(!if_condition_1)"/>
        |  <Setter variable="Roll" function="20.0" activator="((!evaluationFlag_Roll_1) &amp; (!if_condition_1))"/>
        |  <Setter variable="evaluationFlag_Roll_1" function="true" activator="(!if_condition_1)"/>
        |  <Setter variable="evaluationFlag_Roll_1" function="true" activator="if_condition_1"/>
        |</Variables>""")
    }

    test("while loop") {
      funky {
        While(Altitude <= 1000){
          Pitch =! 20
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_0" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_0_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" />
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_0) &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_Pitch_0" function="true" activator="while_condition_0"/>
        |  <Setter variable="evaluationFlag_Pitch_0" function="true" activator="(!while_condition_0)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_0" function="evaluationFlag_Pitch_0" />
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_Pitch_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_0)"/>
        |</Variables>""")
    }

    test("nested loop") {
      funky {
        While(Altitude <= 1000){
          var x = ! 0
          While(Pitch <= 12345){
            true
          }
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_1" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_1_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="true" />
        |  <Setter variable="var0" function="0.0" activator="((!evaluationFlag_var0_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_var0_0" function="true" activator="while_condition_1"/>
        |  <Setter variable="while_condition_0" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_0_0) &amp; evaluationFlag_var0_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="(evaluationFlag_var0_0 &amp; while_condition_1)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_1" function="true" activator="(evaluationFlag_var0_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(((memoised_whileBodyEvaluated_1 &amp; while_condition_0) &amp; evaluationFlag_var0_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_var0_0" function="true" activator="(!while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="(!while_condition_1)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_0" function="(evaluationFlag_var0_0 &amp; (!while_condition_0))" />
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_var0_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |</Variables>""")
    }

    test("embedded nested loop") {
      funky {
        While(Altitude <= 1000){
          var x = Variable[Double]("x")
          x =! 0
          While(Pitch <= 12345){
            true
          }
          x =! 50
        }
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_1" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_1_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_x_0" function="true" activator="while_condition_1"/>
        |  <Setter variable="while_condition_0" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_0_0) &amp; evaluationFlag_x_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="(evaluationFlag_x_0 &amp; while_condition_1)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_1" function="true" activator="(evaluationFlag_x_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(((memoised_whileBodyEvaluated_1 &amp; while_condition_0) &amp; evaluationFlag_x_0) &amp; while_condition_1)"/>
        |  <Setter variable="x" function="50.0" activator="(((!evaluationFlag_x_1) &amp; (!while_condition_0)) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="((!while_condition_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_x_0" function="true" activator="(!while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="(!while_condition_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="(!while_condition_1)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_0" function="((evaluationFlag_x_0 &amp; (!while_condition_0)) &amp; evaluationFlag_x_1)" />
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_x_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_1)"/>
        |</Variables>""")  
    }

    test("while nested in if") {
      funky {
        var x = Variable[Double]("x")
        x =! 10
        If(x > 5){
          While(x > 0){
            //x -=! 1
            x =! x - 1
          }
        }
        x =! 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="10.0" activator="(!evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_0" function="true" />
        |  <Setter variable="if_condition_0" function="(x &gt; 5.0)" activator="((!evaluationFlag_if_condition_0_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_if_condition_0_0" function="true" activator="evaluationFlag_x_0"/>
        |  <Setter variable="while_condition_0" function="(x &gt; 0.0)" activator="(((!evaluationFlag_while_condition_0_0) &amp; if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="(if_condition_0 &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="x" function="(x - 1.0)" activator="((((!evaluationFlag_x_1) &amp; while_condition_0) &amp; if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="((while_condition_0 &amp; if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="(((!while_condition_0) &amp; if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_0" function="evaluationFlag_x_1" activator="(if_condition_0 &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(((memoised_whileBodyEvaluated_0 &amp; while_condition_0) &amp; if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_1" function="false" activator="(((memoised_whileBodyEvaluated_0 &amp; while_condition_0) &amp; if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="((!if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" activator="((!if_condition_0) &amp; evaluationFlag_x_0)"/>
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_2) &amp; ((!while_condition_0) &amp; evaluationFlag_x_1))"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="((!while_condition_0) &amp; evaluationFlag_x_1)"/>
        |</Variables>""")
    }

    test("parallel and") {
      var elevators = Variable[Double]("elevators")
      var ailerons = Variable[Double]("ailerons")
      var thrust = Variable[Double]("thrust")
      val levelFlight = funky {
        While(true){
          elevators =! smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
          thrust =! 1
        }
      }
      val wingLevelling = funky {
        While(true){
          ailerons =! smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
        }
      }

      funky {
        levelFlight & wingLevelling
        thrust =! 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_0" function="true" activator="(!evaluationFlag_while_condition_0_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" />
        |  <Setter variable="elevators" function="(smooth(PID(0.0,(PitchAngle + smooth(AngleOfAttack,0.1)),0.1,0.0,0.1),0.1) + Pitch)" activator="((!evaluationFlag_elevators_0) &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_elevators_0" function="true" activator="while_condition_0"/>
        |  <Setter variable="thrust" function="1.0" activator="(((!evaluationFlag_thrust_0) &amp; evaluationFlag_elevators_0) &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_thrust_0" function="true" activator="(evaluationFlag_elevators_0 &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_elevators_0" function="true" activator="(!while_condition_0)"/>
        |  <Setter variable="evaluationFlag_thrust_0" function="true" activator="(!while_condition_0)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_0" function="(evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)" />
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_elevators_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_0)"/>
        |  <Setter variable="evaluationFlag_thrust_0" function="false" activator="(memoised_whileBodyEvaluated_0 &amp; while_condition_0)"/>
        |  <Setter variable="while_condition_1" function="true" activator="(!evaluationFlag_while_condition_1_0)"/>
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="true" />
        |  <Setter variable="ailerons" function="(smooth(PID(0.0,RollRate,0.001,0.0,0.0),0.1) + Roll)" activator="((!evaluationFlag_ailerons_0) &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_ailerons_0" function="true" activator="while_condition_1"/>
        |  <Setter variable="evaluationFlag_ailerons_0" function="true" activator="(!while_condition_1)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_1" function="evaluationFlag_ailerons_0" />
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="false" activator="(memoised_whileBodyEvaluated_1 &amp; while_condition_1)"/>
        |  <Setter variable="evaluationFlag_ailerons_0" function="false" activator="(memoised_whileBodyEvaluated_1 &amp; while_condition_1)"/>
        |  <Setter variable="thrust" function="0.0" activator="((!evaluationFlag_thrust_1) &amp; (((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)) &amp; ((!while_condition_1) &amp; evaluationFlag_ailerons_0)))"/>
        |  <Setter variable="evaluationFlag_thrust_1" function="true" activator="(((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)) &amp; ((!while_condition_1) &amp; evaluationFlag_ailerons_0))"/>
        |</Variables>""")
    }

    test("parallel or") {
      var elevators = Variable[Double]("elevators")
      var thrust = Variable[Double]("thrust")
      val levelFlight = funky {
        While(true){
          elevators =! smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
          thrust =! 1
        }
      }
      val maintainFor5Seconds = funky {
        var t = ! Time
        While(Time - t < 5)()
      }

      funky {
        levelFlight | maintainFor5Seconds
        thrust =! 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_0" function="true" activator="((!evaluationFlag_while_condition_0_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="true" activator="(!(evaluationFlag_var0_0 &amp; (!while_condition_1)))"/>
        |  <Setter variable="elevators" function="(smooth(PID(0.0,(PitchAngle + smooth(AngleOfAttack,0.1)),0.1,0.0,0.1),0.1) + Pitch)" activator="(((!evaluationFlag_elevators_0) &amp; while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_elevators_0" function="true" activator="(while_condition_0 &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="thrust" function="1.0" activator="((((!evaluationFlag_thrust_0) &amp; evaluationFlag_elevators_0) &amp; while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_thrust_0" function="true" activator="((evaluationFlag_elevators_0 &amp; while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_elevators_0" function="true" activator="((!while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_thrust_0" function="true" activator="((!while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="memoised_whileBodyEvaluated_0" function="(evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)" activator="(!(evaluationFlag_var0_0 &amp; (!while_condition_1)))"/>
        |  <Setter variable="evaluationFlag_while_condition_0_0" function="false" activator="((memoised_whileBodyEvaluated_0 &amp; while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_elevators_0" function="false" activator="((memoised_whileBodyEvaluated_0 &amp; while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_thrust_0" function="false" activator="((memoised_whileBodyEvaluated_0 &amp; while_condition_0) &amp; (!(evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="var0" function="Time" activator="((!evaluationFlag_var0_0) &amp; (!((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0))))"/>
        |  <Setter variable="evaluationFlag_var0_0" function="true" activator="(!((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)))"/>
        |  <Setter variable="while_condition_1" function="((Time - var0) &lt; 5.0)" activator="(((!evaluationFlag_while_condition_1_0) &amp; evaluationFlag_var0_0) &amp; (!((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0))))"/>
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="true" activator="(evaluationFlag_var0_0 &amp; (!((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0))))"/>
        |  <Setter variable="memoised_whileBodyEvaluated_1" function="true" activator="(evaluationFlag_var0_0 &amp; (!((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0))))"/>
        |  <Setter variable="evaluationFlag_while_condition_1_0" function="false" activator="(((memoised_whileBodyEvaluated_1 &amp; while_condition_1) &amp; evaluationFlag_var0_0) &amp; (!((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0))))"/>
        |  <Setter variable="thrust" function="0.0" activator="((!evaluationFlag_thrust_1) &amp; (((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)) | (evaluationFlag_var0_0 &amp; (!while_condition_1))))"/>
        |  <Setter variable="evaluationFlag_thrust_1" function="true" activator="(((!while_condition_0) &amp; (evaluationFlag_elevators_0 &amp; evaluationFlag_thrust_0)) | (evaluationFlag_var0_0 &amp; (!while_condition_1)))"/>
        |</Variables>""")
    }
  }
