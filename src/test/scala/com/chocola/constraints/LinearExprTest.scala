/*
 * chocola library
 *
 * Copyright (C) 2013 hanny
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the “Software”), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.chocola.constraints

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.chocola.ChocoHelpers._
import com.chocola.CPProblem

class LinearExprTest extends FlatSpec with ShouldMatchers{
  "LinearExpr" should "post sum constraint" in {
    val problem = new CPProblem {
      val name: String = "Linearexp test"

      val a = IntVar(1 to 10, "a")
      val b = IntVar(1 to 10, "b")
      val c = IntVar(2 to 10, "c")
      val d = IntVar(4 -> 4, "d")

      subjectsTo{
        a + b + c === d
      }

      branchFirstFailDomainMin(Array(a,b,c))
    }

    problem.solver.getNbCstrs should equal(1)

    problem.findAllSolutions() should equal(1)

    problem.a.getValue should equal (1)
    problem.b.getValue should equal (1)
    problem.c.getValue should equal (2)
  }

  it should "support =/= operator" in {
    val problem = new CPProblem {
      val name: String = "Linearexp =/= operator test"

      val a = IntVar(1 to 10, "a")
      val b = IntVar(1 to 10, "b")
      val c = IntVar(2 to 4, "c")

      subjectsTo{
        a + b =/= c
      }

      branchFirstFailDomainMin(Array(a,b,c))
    }

    problem.solver.getNbCstrs should equal(2)

    problem.findAllSolutions() should equal(294)
  }

  it should "support < operator" in {
    val problem = new CPProblem {
      val name: String = "Linearexp < operator test"

      val a = IntVar(1 to 10, "a")
      val b = IntVar(1 to 10, "b")
      val c = IntVar(4 -> 4, "c")

      subjectsTo{
        a + b < c
      }

      branchFirstFailDomainMin(Array(a,b))
    }

    problem.solver.getNbCstrs should equal(2)

    var num = 0
    while(problem.findSolution())
    {
      problem.a.getValue + problem.b.getValue should be < (problem.c.getValue)
      num += 1
    }

    num should equal(3)
  }

  it should "support <= operator" in {
    val problem = new CPProblem {
      val name: String = "Linearexp <= operator test"

      val a = IntVar(1 to 10, "a")
      val b = IntVar(1 to 10, "b")
      val c = IntVar(4 -> 4, "c")

      subjectsTo{
        a + b <= c
      }

      branchFirstFailDomainMin(Array(a,b))
    }

    problem.solver.getNbCstrs should equal(2)

    var num = 0
    while(problem.findSolution())
    {
      problem.a.getValue + problem.b.getValue should be <= (problem.c.getValue)
      num += 1
    }
    num should equal(6)
  }

  it should "support > operator" in {
    val problem = new CPProblem {
      val name: String = "Linearexp > operator test"

      val a = IntVar(1 to 10, "a")
      val b = IntVar(1 to 10, "b")
      val c = IntVar(4 -> 4, "c")

      subjectsTo{
        a + b > c
      }

      branchFirstFailDomainMin(Array(a,b))
    }

    problem.solver.getNbCstrs should equal(2)

    problem.findAllSolutions() should equal(94)
  }

  it should "support >= operator" in {
    val problem = new CPProblem {
      val name: String = "Linearexp >= operator test"

      val a = IntVar(1 to 10, "a")
      val b = IntVar(1 to 10, "b")
      val c = IntVar(4 -> 4, "c")

      subjectsTo{
        a + b >= c
      }

      branchFirstFailDomainMin(Array(a,b))
    }

    problem.solver.getNbCstrs should equal(2)

    problem.findAllSolutions() should equal(97)
  }
}
