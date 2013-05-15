package com.chocola.helpers.constraints

import scala.language.existentials
import com.chocola.helpers.VariableTypes._
import com.chocola.ConstraintTypes._
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

    def ==> (cons: ConstraintType)(implicit poster: ConstraintPoster) = {
      poster -= cons
      poster += IntConstraintFactory.implies(variable, cons)
    }
  }
}
