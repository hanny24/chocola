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

package com.chocola.helpers

import solver.Solver
import VariableTypes._

/**
 * Provides factory methods for Int variable.
 */
trait IntVarFactory {
  /**
   * Creates a new Int variable whose domain
   * is equal to a given iterable. Name is empty.
   * @param iterable domain of new variables
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  def apply(iterable: Iterable[Int])(implicit solver: Solver) :IntVarType =  apply(iterable, "")

  /**
   * Creates a new Int variable whose domain
   * is equal to a given iterable. Name is empty.
   * @param iterable domain of new variables
   * @param name name of variable
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  def apply(iterable: Iterable[Int], name: String)(implicit solver: Solver) : IntVarType

  /**
   * Creates a new matrix of Int variables whose domains
   * are between 0 and given value. Name is empty.
   * @param max maximum of domain
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  def apply(max: Int)(implicit solver: Solver) : IntVarType = apply(max, "")

  /**
   * Creates a new matrix of Int variables whose domains
   * are between 0 and given value. Name is empty.
   * @param max maximum of domain
   * @param name name of variable
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  def apply(max: Int, name: String)(implicit solver: Solver) : IntVarType

  /**
   * Creates a new matrix of Int variables whose domains
   * are between two given values. Name is empty.
   * @param bounds tuple of minimal and maximal values
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  def apply(bounds: (Int, Int))(implicit solver: Solver) : IntVarType = apply(bounds, "")

  /**
   * Creates a new matrix of Int variables whose domains
   * are between two given values. Name is empty.
   * @param bounds tuple of minimal and maximal values
   * @param name name of variable
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  def apply(bounds: (Int, Int), name: String)(implicit solver: Solver) : IntVarType
}