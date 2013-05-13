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

package com.chocola

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ChocoHelpers._
import solver.constraints.binary.Element

class IntVarElementTest extends FlatSpec with ShouldMatchers{
  "An element constraint" should "produce single constraint with no additional variable" in {
    val _ = new CPProblem {

    val array = IntVar.matrix(0 to 10, 10)
    val index = IntVar(2 to 7)
    val value = IntVar(7 -> 9)

    subjectsTo {
      array(index) === value
    }

    solver.getNbCstrs should equal (1)
    solver.getNbVars should equal (array.length + 2)
    solver.getCstrs()(0).getClass should be (classOf[Element])
    }
  }

  it should "support Seq collections" in {
    val _ = new CPProblem {
    val a = IntVar(0 -> 10)
    val b = IntVar(0 -> 10)
    val c = IntVar(0 -> 10)
    val d = IntVar(0 -> 10)

    subjectsTo {
      List(a,b)(c) === d
    }

    solver.getNbCstrs should equal (1)
    solver.getNbVars should equal (4)
    solver.getCstrs()(0).getClass should be (classOf[Element])
    }
  }
}
