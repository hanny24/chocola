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
import solver.Solver

/**
 * Provides arithmetic constraints helpers
 */
trait IntVarArithConstraints {
  implicit class IntVarArithmWrapper(val variable: IntVarType) extends ComparisonOperators
  {
    type TypeOfConstraint = InversibleConstraint

    override def postUnderlyingConstraint(other: Int, op: String, notop: String)
                                         (implicit poster: ConstraintPoster, solver: Solver) = {
      val cons = IntConstraintFactory.arithm(variable, op, other)
      poster += cons
      val consnot = IntConstraintFactory.arithm(variable, notop, other)
      InversibleConstraint(cons, consnot)
    }

    override def postUnderlyingConstraint(other: IntVarType, op: String, notop: String)
                                         (implicit poster: ConstraintPoster, solver: Solver) = {
      val cons = IntConstraintFactory.arithm(variable, op, other)
      poster += cons
      val consnot = IntConstraintFactory.arithm(variable, notop, other)
      InversibleConstraint(cons, consnot)
    }
  }
}
