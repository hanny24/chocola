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
import com.chocola.CPProblem
import com.chocola.ChocoHelpers._


class LinearExprEqTest extends FlatSpec with ShouldMatchers{
  "LinearExpr" should "support <==> operator" in {
    val problem = new CPProblem {
      val name: String = "Linearexp bool var test"

      val a = BoolVar("a")
      val b = BoolVar("b")
      val c = BoolVar("c")
      val xor = BoolVar("xor")

      subjectsTo{
        xor <==> ((a || b || c) === 1)
        xor === true
      }

      branchFirstFailDomainMin(Array(a,b,c,xor))
    }

    problem.solver.getNbCstrs should equal(4)

    problem.findAllSolutions() should equal(3)
  }
}
