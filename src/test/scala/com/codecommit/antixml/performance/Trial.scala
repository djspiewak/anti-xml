/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.antixml
package performance

import scala.collection.immutable.Set

abstract class Trial(val id: Symbol, val description: String) { self =>
  val warmUps = 20
  val runs = 10
  val runLevel = 0

  val classifiers: Set[Symbol] = Set()

  def create: TrialInstance[_]
  
  /** Contains the trial's executable code.  Input data structures should
    * be kept on this class rather than the `Trial` itself to ensure that they can
    * be garbage collected when the trial is finished executing.
    */
  abstract class TrialInstance[R] {
    type Result = R
    
    type AnyImpl = Implementation[_,R]
    
    var implementations: Seq[AnyImpl] = Seq()
    
    /** A summary description of the input data to the trial */
    def testDataDescription: String
    
    /** A summary description of the trial output */
    def resultDescription(value: Result): String
      
    object implemented {
      def by(desc: String) = new {
        def preload[A](a: =>A) = new {
          def in(impl: A => R): Implementation[A,R] = {
            val result = new Implementation[A,R] {
              val description = desc
              def trialInstance = TrialInstance.this
              def preload() = a
              def run(a: A) = {
                impl(a)
              }
            }
            implementations = implementations :+ result
            result
          }
        }
        def in(impl: =>R): Implementation[Unit,R] = {
          val result = new Implementation[Unit,R] {
            val description = desc
            def trialInstance = TrialInstance.this
            def preload() = ()
            def run(a: Unit) = {
              impl
            }
          }
          implementations = implementations :+ result
          result
        }
      }
    }
    
    /** May be overridden to reduce the warmUps for slow implementations */
    def warmUps(impl: AnyImpl): Int = self.warmUps
    /** May be overridden to reduce the runs for slow implementations */
    def runs(impl: AnyImpl): Int = self.runs
    
    /** May be overridden to increase the run level of particular implementations */
    def runLevel(impl: AnyImpl): Int = self.runLevel
  
  }
  
  trait Implementation[A,R] {
    type Value = A
    type Result = R
    val description: String
    
    def trialInstance: TrialInstance[R]
    
    def preload(): Value
    def run(a: Value): Result
    
    def runs = trialInstance.runs(this)
    def warmUps = trialInstance.warmUps(this)
    def runLevel = trialInstance.runLevel(this)
    def classifiers = self.classifiers
    def id = self.id
  }
  
}

