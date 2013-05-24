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
import com.chocola.ConstraintPoster
import solver.constraints.IntConstraintFactory
import solver.variables.VariableFactory
import solver.Solver
import com.chocola.ChocoHelpers._

case class LinearExprElement(variable: IntVarType, coef: Int)

case class LinearExpr(elements: Seq[LinearExprElement]) extends ComparisonOperators{
  type TypeOfConstraint = InversibleConstraint

  def +(variable: IntVarType) = {
    LinearExpr(LinearExprElement(variable,1) +: elements)
  }

  def -(variable: IntVarType) = {
    LinearExpr(LinearExprElement(variable,1) +: elements)
  }

  def ||(variable: BoolVarType) = {
    LinearExpr(LinearExprElement(variable,1) +: elements)
  }

  lazy val coeficients = elements.map(_.coef).toArray
  lazy val variables = elements.map(_.variable).toArray

  override def postUnderlyingConstraint(other: Int, op: String, notop: String)
                                       (implicit poster: ConstraintPoster, solver:Solver)= {
    postUnderlyingConstraint(VariableFactory.fixed(other,solver),op,notop)
  }

  override def postUnderlyingConstraint(other: IntVarType, op: String, notop: String)
                                       (implicit poster: ConstraintPoster,solver:Solver) = {
    // compute bounds for new variable
    val possibleMin = (variables zip coeficients).toList.foldLeft(0){
      case (acc,(v,c)) => acc + v.getLB * c
    }
    val possibleMax = (variables zip coeficients).toList.foldLeft(0){
      case (acc,(v,c)) => acc + v.getUB * c
    }

    val newvariable = createAdditionalVariable(other,op,possibleMin,possibleMax)

    val c = IntConstraintFactory.scalar(variables, coeficients,newvariable)
    poster += c
    InversibleConstraint(c, () => {
      val notnewvariable = createAdditionalVariable(other,notop,possibleMin,possibleMax)
      IntConstraintFactory.scalar(variables, coeficients,notnewvariable)
    })
  }

  private def createAdditionalVariable(variable: IntVarType, op: String, possibleMin: Int, possibleMax: Int)
                                      (implicit poster: ConstraintPoster,solver: Solver) = {
    op match {
      case "=" => variable
      case "!=" => {
        val n = VariableFactory.bounded("",possibleMin, possibleMax, solver)
        // post
        n =/= variable
        n
      }
      case "<" => {
        val n = VariableFactory.bounded("",possibleMin, variable.getUB - 1, solver)
        // post
        n < variable
        n
      }
      case "<=" => {
        val n = VariableFactory.bounded("",possibleMin, variable.getUB, solver)
        // post
        n <= variable
        n
      }
      case ">" => {
        val n = VariableFactory.bounded("",variable.getLB + 1, possibleMax, solver)
        // post
        n > variable
        n
      }
      case ">=" => {
        val n = VariableFactory.bounded("",variable.getLB, possibleMax, solver)
        // post
        n >= variable
        n
      }
    }
  }
}
