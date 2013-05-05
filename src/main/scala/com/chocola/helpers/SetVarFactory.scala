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

import util.objects.setDataStructures.SetType
import solver.Solver
import com.chocola.helpers.VariableTypes._

/**
 * Provides factory methods for Set variables.
 */
trait SetVarFactory {
  /**
   * Creates a new Set variable. Name is empty.
   * @param kernel kernel of a set
   * @param envelope envelope of a set
   * @param setType set implementation
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.BoolVarType]]
   */
  def apply(kernel: Set[Int], envelope: Set[Int], setType: SetType)(implicit solver: Solver): SetVarType = apply(kernel, envelope, setType, "")

  /**
   * Creates a new Set variable.
   * @param kernel kernel of a set
   * @param envelope envelope of a set
   * @param setType set implementation
   * @param name name of a new variable
   * @param solver Solver where the variable will be used
   * @return instance of [[com.chocola.helpers.VariableTypes.BoolVarType]]
   */
  def apply(kernel: Set[Int], envelope: Set[Int], setType: SetType, name: String)(implicit solver: Solver): SetVarType
}
