package com.chocola

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ChocoHelpers._

class BoolVarTest extends FlatSpec with ShouldMatchers{
  "A BoolVar" should "support implication" in {
    val _ = new CPProblem {
      val bool = BoolVar()
      val a = IntVar(1->3)
      val b = IntVar(4->9)

      subjectsTo {
        bool ==> (a < b)
      }

      solver.getNbCstrs should equal (1)
    }
  }

  it should "support nested implication" in {
    val _ = new CPProblem {
      val bool = BoolVar()
      val bool2 = BoolVar()
      val a = IntVar(1->3)
      val b = IntVar(4->9)

      subjectsTo {
        bool ==> (bool2 ==> (a < b))
      }

      solver.getNbCstrs should equal (1)
    }
  }
}
