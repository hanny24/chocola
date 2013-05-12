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

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.chocola.ChocoHelpers._
import solver.variables.fast.IntervalIntVarImpl

class IntVarTest extends FunSpec with ShouldMatchers{
  implicit val solver = Solver("IntVarTest")
  describe("An IntVar") {
    it("should create bounded domain using Int argument") {
      val a = IntVar(42, "bounded42")
      a.getClass should be (classOf[IntervalIntVarImpl])
    }

    it("should create bounded domain using bounds argument") {
      val a = IntVar(24 -> 42, "bounded24->42")
      a.getClass should be (classOf[IntervalIntVarImpl])
    }

    it("should create enumerated domain using iterable argument") {
      val a = IntVar(0 to 42, "enumerated42")
      a.getClass should be (classOf[IntervalIntVarImpl])
    }

    it("should Array as iterable argument") {
      val a = IntVar((0 to 42).toArray, "array")
      a.getClass should be (classOf[IntervalIntVarImpl])
    }
  }
}
