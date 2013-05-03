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
import solver.variables.{VariableFactory, IntVar, BoolVar}
import solver.variables.delta.IntDelta

/**
 * Helpers for CP variables creation.
 */
trait Variables {
  type IntVarType = IntVar[_ <: IntDelta]
  type BoolVarType = BoolVar[_ <: IntDelta]

  /**
   * Helper object for Int variables.
   */
  object IntVar {

    /**
     * Creates a new Int variable whose domain is a range. Name is empty.
     * @param range domain of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def apply(range: Range)(implicit solver: Solver):IntVarType = apply(range, "")

    /**
     * Creates a new Int variable whose domain is a range.
     * @param range domain of new variable
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def apply(range: Range, name: String)(implicit solver: Solver) = {
      VariableFactory.bounded(name, range.min, range.max, solver):IntVarType
    }

    /**
     * Creates a new Int variable whose domain is a already assigned.
     * Name is empty.
     * @param value value to be assigned
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def apply(value: Int)(implicit solver: Solver):IntVarType = apply(value, "")

    /**
     * Creates a new Int variable whose domain is a already assigned.
     * @param value value to be assigned
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def apply(value: Int, name: String)(implicit solver: Solver):IntVarType = {
      VariableFactory.fixed(name, value, solver)
    }

    /**
     * Creates a new Int variable whose domain is equal to a given set.
     * Name is empty.
     * @param values set of values of domain
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def apply(values: Set[Int])(implicit solver: Solver):IntVarType = apply(values, "")

    /**
     * Creates a new Int variable whose domain is equal to a given set.
     * @param values set of values of domain
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def apply(values: Set[Int], name: String)(implicit solver: Solver):IntVarType = {
      VariableFactory.enumerated(name, values.toArray, solver)
    }

    /**
     * Creates a new Int variables array whose domains are a range.
     * Name is empty.
     * @param range domain of new variables
     * @param n size of an array
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[com.chocola.helpers.Variables.IntVarType]]]
     */
    def apply(range: Range, n: Int)(implicit solver: Solver):Array[IntVarType] = apply(range, n, "")

    /**
     * Creates a new Int variables array whose domains are a range.
     * @param range domain of new variables
     * @param n size of an array
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[com.chocola.helpers.Variables.IntVarType]]]
     */
    def apply(range: Range, n: Int, name: String)(implicit solver: Solver):Array[IntVarType] = {
      VariableFactory.boundedArray(name, n, range.min, range.max, solver)
    }

    /**
     * Creates a new Int variables array whose domains are equal to a given set.
     * Name is empty.
     * @param values domain of new variables
     * @param n size of an array
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[com.chocola.helpers.Variables.IntVarType]]]
     */
    def apply(values: Set[Int], n: Int)(implicit solver: Solver):Array[IntVarType] = apply(values, n, "")

    /**
     * Creates a new Int variables array whose domains are equal to a given set.
     * @param values domain of new variables
     * @param n size of an array
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[com.chocola.helpers.Variables.IntVarType]]]
     */
    def apply(values: Set[Int], n: Int, name: String)(implicit solver: Solver):Array[IntVarType] = {
      VariableFactory.enumeratedArray(name, n, values.toArray, solver)
    }

    /**
     * Creates a new two dimension matrix of Int variables whose domains are a range.
     * Name is empty.
     * @param range domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]
     */
    def apply(range: Range, x: Int, y: Int)(implicit solver: Solver):Array[Array[IntVarType]] = apply(range, x, y, "")

    /**
     * Creates a new two dimension matrix of Int variables whose domains are a range.
     * @param range domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]
     */
    def apply(range: Range, x: Int, y: Int, name: String)(implicit solver: Solver):Array[Array[IntVarType]] = {
      VariableFactory.boundedMatrix(name, x, y, range.min, range.max, solver)
    }

    /**
     * Creates a new two dimension matrix of Int variables whose domains are equal to a given set.
     * Name is empty.
     * @param values domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]
     */
    def apply(values: Set[Int], x: Int, y: Int)(implicit solver: Solver):Array[Array[IntVarType]] = apply(values, x, y, "")

    /**
     * Creates a new two dimension matrix of Int variables whose domains are equal to a given set.
     * @param values domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]
     */
    def apply(values: Set[Int], x: Int, y: Int, name: String)(implicit solver: Solver):Array[Array[IntVarType]] = {
      VariableFactory.enumeratedMatrix(name, x, y, values.toArray, solver)
    }

    /**
     * Creates a new three dimension matrix of Int variables whose domains are a range.
     * Name is empty.
     * @param range domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param z size of z dimension
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]]
     */
    def apply(range: Range, x: Int, y: Int, z: Int)(implicit solver: Solver):Array[Array[Array[IntVarType]]] = apply(range, x, y, z, "")

    /**
     * Creates a new three dimension matrix of Int variables whose domains are a range.
     * @param range domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param z size of z dimension
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]]
     */
    def apply(range: Range, x: Int, y: Int, z: Int, name: String)(implicit solver: Solver):Array[Array[Array[IntVarType]]] = {
      (1 to x).map(i => VariableFactory.boundedMatrix(s"${name}_$i", y, z, range.min, range.max, solver)).toArray
    }

    /**
     * Creates a new three dimension matrix of Int variables whose domains are equal to a given set.
     * Name is empty.
     * @param values domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param z size of z dimension
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]]
     */
    def apply(values: Set[Int], x: Int, y: Int, z: Int)(implicit solver: Solver):Array[Array[Array[IntVarType]]] =
      apply(values, x, y, z, "")

      /**
      * Creates a new three dimension matrix of Int variables whose domains are equal to a given set.
     * @param values domain of new variables
     * @param x size of x dimension
     * @param y size of y dimension
     * @param z size of z dimension
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[scala.Array[com.chocola.helpers.Variables.IntVarType]]]]]
     */
      def apply(values: Set[Int], x: Int, y: Int, z: Int, name: String)(implicit solver: Solver):Array[Array[Array[IntVarType]]] = {
        (1 to x).map(i => VariableFactory.enumeratedMatrix(s"${name}_$i", y, z, values.toArray, solver)).toArray
      }
  }
  /**
   * Wrapper class for Int variable.
   * @param variable instance of [[com.chocola.helpers.Variables.IntVarType]]
   */
  implicit class IntVarWrapper(val variable: IntVarType)
  {
    /**
     * Creates a new Int variable whose value is equal to absolute value of this.
     * @return instance of [[com.chocola.helpers.Variables.IntVarType]]
     */
    def abs = {
      VariableFactory.abs(variable)
    }
  }

  /**
   * Helper object for Int variables.
   */
  object BoolVar {
    /**
     * Creates a new Bool variable. Name is empty.
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.BoolVarType]]
     */
    def apply()(implicit solver: Solver):BoolVarType = apply("")

    /**
     * Creates a new Bool variable.
     * @param name name of variable
     * @param solver Solver where the variable will be used
     * @return instance of [[com.chocola.helpers.Variables.BoolVarType]]
     */
    def apply(name: String)(implicit solver: Solver):BoolVarType = {
      VariableFactory.bool(name, solver)
    }

    /**
     * Creates a new Bool variables array. Name is empty.
     * @param n size of an array
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[com.chocola.helpers.Variables.BoolVarType]]]
     */
    def apply(n: Int)(implicit solver: Solver): Array[BoolVarType] = apply(n, "")

    /**
     * Creates a new Bool variables array.
     * @param n size of an array
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[com.chocola.helpers.Variables.BoolVarType]]]
     */
    def apply(n: Int, name: String)(implicit solver: Solver): Array[BoolVarType] = {
      VariableFactory.boolArray(name, n, solver)
    }

    /**
     * Creates a new two dimension matrix of Bool variables. Name is empty.
     * @param x size of x dimension
     * @param y size of y dimension
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[com.chocola.helpers.Variables.BoolVarType]]]]
     */
    def apply(x: Int, y: Int)(implicit  solver: Solver): Array[Array[BoolVarType]] = apply(x, y, "")

    /**
     * Creates a new two dimension matrix of Bool variables.
     * @param x size of x dimension
     * @param y size of y dimension
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[com.chocola.helpers.Variables.BoolVarType]]]]
     */
    def apply(x: Int, y: Int, name: String)(implicit  solver: Solver): Array[Array[BoolVarType]] = {
      VariableFactory.boolMatrix(name, x, y, solver)
    }

    /**
     * Creates a new three dimension matrix of Bool variables. Name is empty.
     * @param x size of x dimension
     * @param y size of y dimension
     * @param z size of z dimension
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[scala.Array[com.chocola.helpers.Variables.BoolVarType]]]]]
     */
    def apply(x: Int, y: Int, z: Int)(implicit  solver: Solver): Array[Array[Array[BoolVarType]]] = apply(x, y, z, "")

    /**
     * Creates a new three dimension matrix of Bool variables.
     * @param x size of x dimension
     * @param y size of y dimension
     * @param z size of z dimension
     * @param name name of new variable
     * @param solver Solver where the variable will be used
     * @return instance of [[scala.Array[scala.Array[scala.Array[com.chocola.helpers.Variables.BoolVarType]]]]]
     */
    def apply(x: Int, y: Int, z: Int, name: String)(implicit  solver: Solver): Array[Array[Array[BoolVarType]]] = {
      (1 to x).map(i => VariableFactory.boolMatrix(s"${name}_$i", y, z, solver)).toArray
    }
  }
}
