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
import VariableTypes._

/**
 * Provides factory matrix methods for Int variable.
 */
trait IntVarMatrix {
  self: IntVarFactory =>

  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable. Name is empty.
  * @param iterable domain of new variables
  * @param size1 size of dimension
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int)(implicit solver: Solver) : Array[IntVarType] = 
        matrix(iterable, size1 : Int, "")
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable. Name is empty.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int)(implicit solver: Solver) : Array[Array[IntVarType]] = 
        matrix(iterable, size1 : Int, size2 : Int, "")
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable. Name is empty.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension 
  * @param size3 size of dimension
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, size3 : Int)(implicit solver: Solver) : Array[Array[Array[IntVarType]]] = 
        matrix(iterable, size1 : Int, size2 : Int, size3 : Int, "")
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable. Name is empty.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension 
  * @param size3 size of dimension 
  * @param size4 size of dimension
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, size3 : Int, size4 : Int)(implicit solver: Solver) : Array[Array[Array[Array[IntVarType]]]] = 
        matrix(iterable, size1 : Int, size2 : Int, size3 : Int, size4 : Int, "")
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable. Name is empty.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension 
  * @param size3 size of dimension 
  * @param size4 size of dimension 
  * @param size5 size of dimension
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int)(implicit solver: Solver) : Array[Array[Array[Array[Array[IntVarType]]]]] = 
        matrix(iterable, size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int, "")

  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable.
  * @param iterable domain of new variables
  * @param size1 size of dimension
  * @param name name of new variable
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, name: String)(implicit solver: Solver) = {
    val array = iterable.toArray
    Array.tabulate(size1){
        case (x1) => apply(array, s"${name}_${x1}")
    }
  }
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension
  * @param name name of new variable
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, name: String)(implicit solver: Solver) = {
    val array = iterable.toArray
    Array.tabulate(size1, size2){
        case (x1, x2) => apply(array, s"${name}_${x1}_${x2}")
    }
  }
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension 
  * @param size3 size of dimension
  * @param name name of new variable
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, size3 : Int, name: String)(implicit solver: Solver) = {
    val array = iterable.toArray
    Array.tabulate(size1, size2, size3){
        case (x1, x2, x3) => apply(array, s"${name}_${x1}_${x2}_${x3}")
    }
  }
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension 
  * @param size3 size of dimension 
  * @param size4 size of dimension
  * @param name name of new variable
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, size3 : Int, size4 : Int, name: String)(implicit solver: Solver) = {
    val array = iterable.toArray
    Array.tabulate(size1, size2, size3, size4){
        case (x1, x2, x3, x4) => apply(array, s"${name}_${x1}_${x2}_${x3}_${x4}")
    }
  }
  
  /**
  * Creates a new matrix of Int variables whose domains
  * are equal to a given iterable.
  * @param iterable domain of new variables
  * @param size1 size of dimension 
  * @param size2 size of dimension 
  * @param size3 size of dimension 
  * @param size4 size of dimension 
  * @param size5 size of dimension
  * @param name name of new variable
  * @param solver Solver where the variable will be used
  * @return instance of [[scala.Array]]
  */
  def matrix(iterable: Iterable[Int], size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int, name: String)(implicit solver: Solver) = {
    val array = iterable.toArray
    Array.tabulate(size1, size2, size3, size4, size5){
        case (x1, x2, x3, x4, x5) => apply(array, s"${name}_${x1}_${x2}_${x3}_${x4}_${x5}")
    }
  }

    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value. Name is empty.
    * @param max maximum of domain
    * @param size1 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int)(implicit solver: Solver) : Array[IntVarType] = 
          matrix(max, size1 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value. Name is empty.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int)(implicit solver: Solver) : Array[Array[IntVarType]] = 
          matrix(max, size1 : Int, size2 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value. Name is empty.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, size3 : Int)(implicit solver: Solver) : Array[Array[Array[IntVarType]]] = 
          matrix(max, size1 : Int, size2 : Int, size3 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value. Name is empty.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, size3 : Int, size4 : Int)(implicit solver: Solver) : Array[Array[Array[Array[IntVarType]]]] = 
          matrix(max, size1 : Int, size2 : Int, size3 : Int, size4 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value. Name is empty.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension 
    * @param size5 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int)(implicit solver: Solver) : Array[Array[Array[Array[Array[IntVarType]]]]] = 
          matrix(max, size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int, "")

    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value.
    * @param max maximum of domain
    * @param size1 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1){
          case (x1) => apply(max, s"${name}_${x1}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2){
          case (x1, x2) => apply(max, s"${name}_${x1}_${x2}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, size3 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2, size3){
          case (x1, x2, x3) => apply(max, s"${name}_${x1}_${x2}_${x3}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, size3 : Int, size4 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2, size3, size4){
          case (x1, x2, x3, x4) => apply(max, s"${name}_${x1}_${x2}_${x3}_${x4}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between 0 and given value.
    * @param max maximum of domain
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension 
    * @param size5 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(max: Int, size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2, size3, size4, size5){
          case (x1, x2, x3, x4, x5) => apply(max, s"${name}_${x1}_${x2}_${x3}_${x4}_${x5}")
      }
    }

    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values. Name is empty.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int)(implicit solver: Solver) : Array[IntVarType] = 
          matrix(bounds, size1 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values. Name is empty.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int)(implicit solver: Solver) : Array[Array[IntVarType]] = 
          matrix(bounds, size1 : Int, size2 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values. Name is empty.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, size3 : Int)(implicit solver: Solver) : Array[Array[Array[IntVarType]]] = 
          matrix(bounds, size1 : Int, size2 : Int, size3 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values. Name is empty.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, size3 : Int, size4 : Int)(implicit solver: Solver) : Array[Array[Array[Array[IntVarType]]]] = 
          matrix(bounds, size1 : Int, size2 : Int, size3 : Int, size4 : Int, "")
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values. Name is empty.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension 
    * @param size5 size of dimension
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int)(implicit solver: Solver) : Array[Array[Array[Array[Array[IntVarType]]]]] = 
          matrix(bounds, size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int, "")

    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1){
          case (x1) => apply(bounds, s"${name}_${x1}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2){
          case (x1, x2) => apply(bounds, s"${name}_${x1}_${x2}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, size3 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2, size3){
          case (x1, x2, x3) => apply(bounds, s"${name}_${x1}_${x2}_${x3}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, size3 : Int, size4 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2, size3, size4){
          case (x1, x2, x3, x4) => apply(bounds, s"${name}_${x1}_${x2}_${x3}_${x4}")
      }
    }
    
    /**
    * Creates a new matrix of Int variables whose domains
    * are between two given values.
    * @param bounds tuple of minimal and maximal values
    * @param size1 size of dimension 
    * @param size2 size of dimension 
    * @param size3 size of dimension 
    * @param size4 size of dimension 
    * @param size5 size of dimension
    * @param name name of new variable
    * @param solver Solver where the variable will be used
    * @return instance of [[scala.Array]]
    */
    def matrix(bounds: (Int, Int), size1 : Int, size2 : Int, size3 : Int, size4 : Int, size5 : Int, name: String)(implicit solver: Solver) = {
      Array.tabulate(size1, size2, size3, size4, size5){
          case (x1, x2, x3, x4, x5) => apply(bounds, s"${name}_${x1}_${x2}_${x3}_${x4}_${x5}")
      }
    }
}