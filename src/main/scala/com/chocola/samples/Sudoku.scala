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

package com.chocola.samples

import com.chocola.ChocoHelpers._
import com.chocola.CPProblem

class Sudoku(hints: Seq[((Int, Int), Int)]) extends CPProblem {
  // define name of a problem
  val name = "Sudoku"

  // create int variables
  val fields = IntVar.matrix(1 to 9, 9, 9, "fields")

  subjectsTo {
    // post all hints
    hints.foreach {
      case ((x,y), value) => fields(x)(y) === value
    }

    // post alldifferent for all rows
    0 until 9 foreach (r => alldifferent(fields.row(r)))

    // post alldifferent for all columns
    0 until 9 foreach (c => alldifferent(fields.column(c)))

    // post alldifferent for all 3x3 blocks
    val blocks = for {
        x <- 0 until 9 by 3
        y <- 0 until 9 by 3
    } yield fields.submatrix((x,y),3,3).flatten
    blocks.foreach(alldifferent(_))
  }

  branchFirstFailDomainMin(fields.flatten)
}