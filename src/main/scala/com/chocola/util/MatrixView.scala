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

import scala.language.implicitConversions

/**
 * Provides useful methods that treats Seq[Seq[T]]
 * as matrix. Implicit conversion is defined (for
 * Array[Array[T]] as well). Row-major representation
 * is assumed.
 * @param underlying - underlying data representation, usually Array[Array[T]]
 * @tparam T
 */
case class MatrixView[+T](underlying: Seq[Seq[T]]) {
  /**
   * Flattened two nested collections into single one.
   */
  lazy val flatten : Seq[T] = underlying.flatten.toIndexedSeq

  /**
   * Extracts single element form matrix.
   * @param x x coordinate
   * @param y y coordinate
   * @return element at [x,y] coordinates
   */
  def apply(x: Int, y:Int):T = underlying(x)(y)

  /**
   * Extracts all elements from i-th row.
   * @param i row number
   * @return elements from i-th row
   */
  def row(i: Int):Seq[T] = underlying(i)

  /**
   * Extracts all elements from i-th column.
   * @param j column number
   * @return elements from i-th column
   */
  def column(j: Int):Seq[T] = {
    (0 until underlying.length map (underlying(_)(j)))
  }

  /**
   * Extracts submatrix.
   * @param topleft pair of coordinates of top left corner
   * @param height height of submatrix
   * @param width width of submatrix
   * @return
   */
  def submatrix(topleft: (Int, Int), height: Int, width: Int):MatrixView[T] = {
    val seq = (topleft._1 until topleft._1 + height).map { i =>
      underlying(i).drop(topleft._2).take(width)
    }
    MatrixView(seq)
  }

  /**
   * Flattens nested collection into a single one. Then it returns
   * every length element starting with an element that was at
   * coordinate start. Length may be negative as well.
   * @param start where to start
   * @param length distance
   * @return
   */
  def strip(start: (Int,Int), length: Int):Seq[T] = {
    val width = if (underlying.isEmpty)
                  0
                else
                  underlying(0).size
    val (end, step) = if (length < 0) (-1, length)
                      else (flatten.size, length)
    ((start._1 * width + start._2) until end by step).map { i =>
      flatten(i)
    }
  }

  /**
   * Extracts elements on diagonal.
   */
  lazy val diagonal = strip((0,0),underlying.size + 1)
}

object MatrixView {
  def apply[T](array: Array[Array[T]]) = new MatrixView(array.map(_.toSeq).toSeq)
}

/**
 * Provides implicit conversions from Seq[Seq[T]] and Array[Array[T]]
 * to MatrixView[T] and vice-versa.
 */
trait MatrixViewConversions {
  implicit def seqSeqToMatrixView[T](seq: Seq[Seq[T]]) = MatrixView(seq)

  implicit def arrayArrayToMatrixView[T](array: Array[Array[T]]) = MatrixView(array)

  implicit def matrixViewToSeqSeq[T](view: MatrixView[T]) = view.underlying
}
