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

import com.chocola.helpers.VariableTypes._
import solver.constraints.IntConstraintFactory
import com.chocola.ConstraintPoster

/**
 * Contains implicit conversion for arrays and Seqs of IntVar.
 */
trait IntVarSeqConstraints {
  implicit class IntVarSeqWrapper(val seq: Seq[IntVarType]) {
    /**
     * Stands for Array[IntVar](IntVar)
     * @param index
     * @return
     */
    def apply(index: IntVarType) = new IntVarElementWrapper(seq.toArray, index)
  }

  implicit class IntVarArrayWrapper(val array: Array[IntVarType]) {
    /**
     * Stands for Seq[IntVar](IntVar)
     * @param index
     * @return
     */
    def apply(index: IntVarType) = new IntVarElementWrapper(array, index)
  }

  /**
   * Used to post element and element-related constraints.
   * @param array
   * @param index
   */
  class IntVarElementWrapper(array: Array[IntVarType], index: IntVarType) {
    /**
     * Posts array(index) == value
     * @param value
     * @param poster
     * @return
     */
    def ===(value: IntVarType)(implicit poster: ConstraintPoster) = {
      poster.postAllAndPush(IntConstraintFactory.element(value, array, index, 0))
    }
  }

  /**
   * Defines consistencies supported by Choco implementation.
   */
  object AllDifferentConsistency extends Enumeration {
    type AllDifferentConsistency = Value
    val Bounds = Value
    val Arc = Value

    def toShortName(consistency: AllDifferentConsistency) =
      if (consistency == Bounds)
        "BC"
      else
        "AC"
  }

  /**
   * Post All different (distinct) constraint. Arc consistency is used.
   * @param seq
   * @param poster
   * @return
   */
  def alldifferent(seq: Seq[IntVarType])(implicit poster: ConstraintPoster) {
    alldifferent(seq.toArray, AllDifferentConsistency.Arc)
  }

  /**
   * Post All different (distinct) constraint. Arc consistency is used.
   * @param array
   * @param poster
   * @return
   */
  def alldifferent(array: Array[IntVarType])(implicit poster: ConstraintPoster) {
    alldifferent(array, AllDifferentConsistency.Arc)
  }

  /**
   * Post All different (distinct) constraint.
   * @param seq
   * @param consistency
   * @param poster
   * @return
   */
  def alldifferent(seq: Seq[IntVarType], consistency: AllDifferentConsistency.AllDifferentConsistency)(implicit poster: ConstraintPoster) {
    alldifferent(seq.toArray, consistency)
  }

  /**
   * Post All different (distinct) constraint.
   * @param array
   * @param consistency
   * @param poster
   * @return
   */
  def alldifferent(array: Array[IntVarType], consistency: AllDifferentConsistency.AllDifferentConsistency)(implicit poster: ConstraintPoster) {
    poster.postAllAndPush(IntConstraintFactory.alldifferent(array, AllDifferentConsistency.toShortName(consistency)))
  }
}
