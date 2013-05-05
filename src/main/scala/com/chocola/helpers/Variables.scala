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
import util.objects.setDataStructures.{SetFactory, SetType, ISet}

/**
 * Helpers for CP variables creation.
 */
trait Variables{
  /**
   * Helper object for Int variables.
   */
  object IntVar extends IntVarMatrix with IntVarFactory{
    def apply(iterable: Array[Int], name: String)(implicit solver: Solver) = {
      VariableFactory.enumerated(name, iterable, solver)
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
     * Creates a new Int variable whose value is equal to value of this + offset.
     * @param offset variable offset
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def + (offset: Int) = {
      VariableFactory.offset(variable, offset)
    }

    /**
     * Creates a new Int variable whose value is equal to value of this + offset.
     * @param offset variable offset
     * @return instance of [[com.chocola.helpers.VariableTypes.IntVarType]]
     */
    def - (offset: Int) = {
      VariableFactory.offset(variable, offset)
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
   * Wrapper class for Bool variable.
   * @param variable instance of [[com.chocola.helpers.VariableTypes.BoolVarType]]
   */
  implicit class BoolVarWrapper(val variable: BoolVarType)
  {
    /**
     * Create a view over Bool variable holding the logical negation of Bool.
     * @return instance of [[com.chocola.helpers.VariableTypes.BoolVarType]]
     */
    def unary_! = {
      VariableFactory.not(variable)
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