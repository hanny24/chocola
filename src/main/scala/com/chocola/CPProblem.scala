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
import com.chocola.helpers.constraints.{RegularConstraint, ChocolaConstraint}
import solver.search.strategy.IntStrategyFactory
import com.chocola.helpers.VariableTypes

trait ConstrainTypeDef {
  type ConstraintType = Constraint[T, U] forSome {type T <: Variable[_ <:IDelta]; type U <: Propagator[T]}
}

/**
 * Data types of constraints
 */
package object ConstraintTypes extends ConstrainTypeDef{
}

import ConstraintTypes._
import VariableTypes._
/**
  * Trait that is used for delayed constraint posting.
 * It basically acts as a stack.
 */
trait ConstraintPoster {
  def += (constraint: ConstraintType) : ChocolaConstraint

  def += [T <: ChocolaConstraint](constraint: T):T = {
    this += constraint.constraint
    constraint
  }

  def -= (constraint: ConstraintType) : Unit

  def -= (constraint: ChocolaConstraint) {
    this -= constraint.constraint
  }

}

/**
  * General Constraint programming problem. Includes Solver,
 * some syntactical sugar and some implicit values needed.
 */
trait CPProblem {
  val name: String

  implicit val solver = new Solver(name)

  /**
   * Implementation of ConstraintPoster
   */
  implicit object Poster extends ConstraintPoster {
    type SetType = collection.mutable.HashSet[ConstraintType]
    val constraints = new SetType()

    def += (constraint: ConstraintType) = {
      constraints += constraint
      RegularConstraint(constraint)
    }

    def -= (constraint: ConstraintType) {
      constraints -= constraint
    }
  }

  /**
   * This blocks ensures correct constraints posting.
   * Name was inspired by OscaR.
   * @param body contains problem's constraints
   */
  def subjectsTo(body: => Unit) = {
    body
    Poster.constraints.foreach(solver.post(_))
    Poster.constraints.clear()
  }

  def branchFirstFailDomainMin(vars: Seq[IntVarType]) {
    branchFirstFailDomainMin(vars.toArray)
  }

  def branchFirstFailDomainMin(vars: Array[IntVarType]) {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(vars))
  }

  def findSolution() = {
    if (solver.getMeasures.getSolutionCount == 0) {
      solver.findSolution()
    }
    else{
      solver.nextSolution()
    }
  }

  def findAllSolutions() = {
    solver.findAllSolutions()
  }
}


