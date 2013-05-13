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

import scala.language.existentials
import solver.Solver
import solver.constraints.Constraint
import solver.variables.Variable
import solver.variables.delta.IDelta
import solver.constraints.propagators.Propagator

/**
 * Data types of constraints
 */
package object ConstraintTypes{
  type ConstraintType = Constraint[T, U] forSome {type T <: Variable[_ <:IDelta]; type U <: Propagator[T]}
  type StackType = collection.mutable.ArrayStack[ConstraintType]
}

import ConstraintTypes._

/**
 * Trait that is used for delayed constraint posting.
 * It basically acts as a stack.
 */
trait ConstraintPoster {

  def push(constraint: ConstraintType): Unit

  def pop(): ConstraintType

  def isEmpty:Boolean

  def postAllAndClear() : Unit

  def postAllAndPush(constraint: ConstraintType) {
    postAllAndClear()
    push(constraint)
  }
}

/**
 * General Constraint programming problem. Includes Solver,
 * some syntactical sugar and some implicit values needed.
 */
trait CPProblem {

  implicit val solver = new Solver()

  private val constraintsStack = new StackType()

  /**
   * Implementation of ConstraintPoster
   */
  implicit object Poster extends ConstraintPoster {
    def push(constraint: ConstraintType) {
      constraintsStack.push(constraint)
    }

    def pop(): ConstraintType = constraintsStack.pop()

    def isEmpty: Boolean = constraintsStack.isEmpty

    def postAllAndClear() {
      constraintsStack.foreach(solver.post(_))
      constraintsStack.clear()
    }
  }

  /**
   * This blocks ensures correct constraints posting.
   * Name was inspired by OscaR.
   * @param body contains problem's constraints
   */
  def subjectsTo(body: => Unit) = {
    body
    Poster.postAllAndClear()
  }
}


