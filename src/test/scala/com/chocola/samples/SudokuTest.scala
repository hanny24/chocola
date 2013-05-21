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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SudokuTest extends FlatSpec with ShouldMatchers{
  "SudokuTest sample" should "be solved" in {
    val matrixHints = List(
      "...3.2...",
      ".5.798.3.",
      "..7...8..",
      "..86.73..",
      ".7.....6.",
      "..35.41..",
      "..5...6..",
      ".2.419.5.",
      "...8.6...").map(_.map {
      case '.' => 0
      case a => a - '0'
    })

    val hints = for {
      (row, x) <- matrixHints.zipWithIndex
      (value,y) <- row.zipWithIndex
      if value != 0
    } yield ((x,y),value)

    val sudoku = new Sudoku(hints)
    sudoku.findSolution()

    val result = sudoku.fields.map { r =>
      r.map(_.getValue).toList
    }

    val expected = List(
      List(6,8,9,3,4,2,5,1,7),
      List(1,5,2,7,9,8,4,3,6),
      List(3,4,7,1,6,5,8,9,2),
      List(9,1,8,6,2,7,3,4,5),
      List(5,7,4,9,3,1,2,6,8),
      List(2,6,3,5,8,4,1,7,9),
      List(4,9,5,2,7,3,6,8,1),
      List(8,2,6,4,1,9,7,5,3),
      List(7,3,1,8,5,6,9,2,4)
    )

    result.toList should equal(expected)
  }
}
