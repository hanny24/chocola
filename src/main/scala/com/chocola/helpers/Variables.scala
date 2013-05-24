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
import solver.variables.VariableFactory
import VariableTypes._
import util.objects.setDataStructures.SetType
import com.chocola.helpers.constraints.{LinearExprElement, LinearExpr}

/**
 * Helpers for CP variables creation.
 */
trait Variables{
  /**
   * Helper object for Int variables.
   */
  object IntVar extends IntVarMatrix with IntVarFactory{
    def apply(iterable: Iterable[Int], name: String)(implicit solver: Solver) = {
      VariableFactory.enumerated(name, iterable.toArray , solver)
    }

    def apply(max: Int, name: String)(implicit solver: Solver) = {
      VariableFactory.bounded(name, 0, max, solver)
    }

    def apply(bounds: (Int, Int), name: String)(implicit solver: Solver) = {
      VariableFactory.bounded(name, bounds._1, bounds._2, solver)
    }
  }
  /**
   * Wrapper class for Int variable.
   * @param variable instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
   */
  implicit class IntVarWrapper(val variable: IntVarType)
  {
    /**
     * Creates a new Int variable whose value is equal to absolute value of this.
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def abs = {
      VariableFactory.abs(variable)
    }

    /**
     * Creates a new Int variable whose value is equal to minus value of this.
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def unary_- = {
      VariableFactory.minus(variable)
    }

    /**
     * Creates a new Int variable whose value is square of value of this.
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def sqr = {
      VariableFactory.sqr(variable)
    }

    /**
     * Create an offset view based on Int variable and given offset.
     * @param offset variable offset
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def + (offset: Int) = {
      VariableFactory.offset(variable, offset)
    }

    /**
     * Create an offset view based on Int variable and given offset.
     * @param offset variable offset
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def - (offset: Int) = {
      VariableFactory.offset(variable, -offset)
    }

    /**
     * Create a scale view based on Int variable and given scalar.
     * Equal to value * variable.
     * @param value scalar value
     * @return
     */
    def *(value: Int) = {
      VariableFactory.scale(variable, value)
    }

    /**
     * Creates new linear expression.
     * @param that
     * @return
     */
    def +(that: IntVarType):LinearExpr = {
      LinearExpr(List(LinearExprElement(variable,1), LinearExprElement(that,1)))
    }

    /**
     * Creates new linear expression.
     * @param that
     * @return
     */
    def -(that: IntVarType):LinearExpr = {
      LinearExpr(List(LinearExprElement(variable,1), LinearExprElement(that,-1)))
    }
  }

  /**
   * Provides additional helpers for Int variable (commutative operations)
   * @param value
   */
  implicit class IntWrapper(val value: Int)
  {
    /**
     * Create an offset view based on Int variable and given offset.
     * @param variable Int variable
     * @return
     */
    def +(variable: IntVarType) = {
      VariableFactory.offset(variable, value)
    }

    /**
     * Create an offset view based on Int variable and given offset.
     * @param variable Int variable
     * @return
     */
    def -(variable: IntVarType) = {
      VariableFactory.offset(-variable, value)
    }

    /**
     * Create a scale view based on Int variable and given scalar.
     * Equal to value * variable.
     * @param variable Int variable to be scaled
     * @return
     */
    def *(variable: IntVarType) = {
      VariableFactory.scale(variable, value)
    }

    /**
     * Create fixed Int variable. Name is equal to value.
     * @param solver Solver to be used
     * @return
     */
    def toIntVar(implicit solver: Solver):IntVarType = {
      toIntVar(value.toString)
    }

    /**
     * Create fixed Int variable.
     * @param name name of new variable
     * @param solver Solver to be used
     * @return
     */
    def toIntVar(name: String)(implicit solver: Solver):IntVarType = {
      VariableFactory.fixed(name, value, solver)
    }
  }

  /**
   * Helper object for Int variables.
   */
  object BoolVar extends BoolVarMatrix with BoolVarFactory{
    def apply(name: String)(implicit solver: Solver) : BoolVarType = {
      VariableFactory.bool(name, solver)
    }
  }

  /**
   * Helper object for Set variables.
   */
  object SetVar {
    def apply(kernel: Set[Int], envelope: Set[Int], setType: SetType, name: String)(implicit solver: Solver): SetVarType = {
      val k = util.objects.setDataStructures.SetFactory.makeSet(setType, kernel.max - 1)
      val e = util.objects.setDataStructures.SetFactory.makeSet(setType, envelope.max - 1)
      kernel.foreach(k.add)
      envelope.foreach(e.add)
      VariableFactory.set(name, e, k, solver)
    }
  }
}