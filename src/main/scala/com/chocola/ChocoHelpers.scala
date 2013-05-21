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

import scala.language.implicitConversions
import com.chocola.helpers.{VariableTypesDef, Variables}
import solver.Solver
import com.chocola.helpers.constraints.{IntVarArithConstraints, BoolVarConstraints, IntVarSeqConstraints}
import com.chocola.util.MatrixViewConversions

/**
 * Provides all-in-one helpers import.
 */
package object ChocoHelpers extends Variables with ConstrainTypeDef
                                              with VariableTypesDef
                                              with IntVarArithConstraints
                                              with BoolVarConstraints
                                              with IntVarSeqConstraints
                                              with MatrixViewConversions
{

  /**
   * Helper for Solver.
   */
  object Solver {
    /**
     * Creates new Solver.
     * @param problem name of solver
     * @return instance of [[solver.Solver]]
     */
    def apply(problem: String) = new Solver(problem)
  }
}
