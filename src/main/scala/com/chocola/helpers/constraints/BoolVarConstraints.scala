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

package com.chocola.helpers.constraints

import scala.language.existentials
import com.chocola.helpers.VariableTypes._
import com.chocola.ConstraintPoster
import solver.constraints.IntConstraintFactory
import solver.variables.VariableFactory

trait BoolVarConstraints {
  implicit class BoolVarWrapper(val variable: BoolVarType) {
    /**
     * Create a view over Bool variable holding the logical negation of Bool.
     * @return instance of [[com.chocola.helpers.VariableTypes.BoolVarType]]
     */
    def unary_! = {
      VariableFactory.not(variable)
    }

    /**
     * Syntactical support for implies constraint
     * @param cons
     * @param poster
     * @return
     */
    def ==> (cons: ChocolaConstraint)(implicit poster: ConstraintPoster) = {
      poster -= cons
      poster += IntConstraintFactory.implies(variable, cons.constraint)
    }

    /**
     * Syntactic sugar for variable => constraint && !variable => !constraint
     * @param cons
     * @param poster
     * @return
     */
    def <==> (cons: InversibleConstraint)(implicit poster: ConstraintPoster) = {
      poster -= cons
      val a = IntConstraintFactory.implies(variable, cons.constraint)
      val b = IntConstraintFactory.implies(!variable, cons.not)
      poster += a
      poster += b
      (a, b)
    }
  }
}
