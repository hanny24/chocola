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

package com.chocola.util

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.chocola.ChocoHelpers._

class MatrixViewTest extends FlatSpec with ShouldMatchers{
  val data = Array.tabulate(10,10)(10 * _ + _)

  "MatrixView" should "access row" in {
    data.row(3).toList should equal(30 until 40 toList)
  }

  it should "access column" in {
    data.column(3).toList should equal(3 until 100 by 10 toList)
  }

  it should "access diagonal" in {
    data.diagonal.toList should equal(0 until 100 by 11 toList)
  }

  it should "support backward stripped access" in {
    data.strip((9,0),-11).toList should equal(90 to 0 by -11 toList)
  }
}
