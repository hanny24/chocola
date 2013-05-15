package com.chocola.helpers.constraints

import scala.language.existentials
import com.chocola.helpers.VariableTypes._
import com.chocola.ConstraintPoster
import solver.constraints.IntConstraintFactory

/**
 * Provides arithmetic constraints helpers
 */
trait IntVarArithConstraints {
  implicit class IntVarArithmWrapper(val variable: IntVarType)
  {
    def === (other: IntVarType)(implicit poster: ConstraintPoster) =
      postArithm(other, "=")

    def =/= (other: IntVarType)(implicit poster: ConstraintPoster) =
      postArithm(other, "!=")

    def < (other: IntVarType)(implicit poster: ConstraintPoster) =
      postArithm(other, "<")

    def > (other: IntVarType)(implicit poster: ConstraintPoster) =
      postArithm(other, ">")

    def <= (other: IntVarType)(implicit poster: ConstraintPoster) =
      postArithm(other, "<=")

    def >= (other: IntVarType)(implicit poster: ConstraintPoster) =
      postArithm(other, "=")

    private def postArithm(other: IntVarType, op: String)(implicit poster: ConstraintPoster) = {
      poster += IntConstraintFactory.arithm(variable, op, other)
    }
  }
}
