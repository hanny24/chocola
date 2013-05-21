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

class AllDifferentTest extends FlatSpec with ShouldMatchers{
  "An alldifferent" should "support Seq collection" in {
    val _ = new CPProblem {
      val name = ""
      val a = IntVar(1 -> 4)
      val b = IntVar(1 -> 4)
      val c = IntVar(1 -> 4)

      subjectsTo {
        alldifferent(Seq(a,b,c))
      }
    }
  }

  it should "support array" in {
    val _ = new CPProblem {
      val name = ""
      val array = IntVar.matrix(1 -> 4, 42)
      subjectsTo {
        alldifferent(array)
      }
    }
  }
}
