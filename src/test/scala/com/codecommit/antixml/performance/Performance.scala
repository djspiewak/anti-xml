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

object Performance {
  
  def main(args: Array[String]) {
    new PerformanceRunner(trials).run(args)
  }

  val trials = List(
      LoadingXmlSmall,
      LoadingXmlLarge,
      
      ShallowSelectionSmall,
      DeepSelectionSmall,
      ShallowSelectionSmallCold,
      DeepSelectionSmallCold,
      DeepSelectionOnceSmallCold,
      ShallowSelectionLarge,
      DeepSelectionLarge,
      
      ShallowModifyFewSmall,
      ShallowModifyFewSmallSelectionTime,
      ShallowModifyManySmall,
      ShallowModifyManySmallSelectionTime,
      DeepModifyFewSmall,
      DeepModifyFewSmallSelectionTime,
      DeepModifyManySmall,
      DeepModifyManySmallSelectionTime,

      ShallowZipperOps,
      DeepZipperOps,
      HugeZipperOps,
      HugeDeadZipperOps
      )
  
  /* === Load trials === */
  
  object LoadingXmlSmall extends Trial('loadSmall, "Loading a 7 MB XML file") with LoadTrial {
    override val sizeDescription = "7 MB"
    override val classifiers = Set('load, 'small)
    override def xmlResource = getClass.getResource("/spending.xml")
  }

  object LoadingXmlLarge extends Trial('loadLarge, "Loading a 30 MB XML file") with LoadTrial {
    override val warmUps = 5
    override val sizeDescription = "30 MB"
    override val runLevel = 2
    override val classifiers = Set('load, 'large)
    override def xmlResource = getClass.getResource("/discogs_20110201_labels.xml")
  }

  /* === Selection trials === */
  
  object ShallowSelectionSmall extends Trial('shallowSelectSmall, "Shallow selection in a 7 MB tree") with WarmShallowSelectionTrial {
    override val classifiers = Set('select, 'small, 'shallow)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List(List("result","doc","MajorFundingAgency2"), List("foo","bar", "MajorFundingAgency2"))
  }
  

  object DeepSelectionSmall extends Trial('deepSelectSmall, "Deep selection in a 7 MB tree") with WarmDeepSelectionTrial {
    override val classifiers = Set('select, 'small, 'deep)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List("MajorFundingAgency2","fubar")
  }

  object ShallowSelectionSmallCold extends Trial('shallowSelectSmallCold, "Shallow selection in a cold 7 MB tree") with ColdShallowSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('select, 'small, 'shallow, 'cold)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List(List("result","doc","MajorFundingAgency2"), List("foo","bar", "MajorFundingAgency2"))
  }

  object DeepSelectionSmallCold extends Trial('deepSelectSmallCold, "Deep selection in a cold 7 MB tree") with ColdDeepSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('select, 'small, 'deep, 'cold)

    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List("MajorFundingAgency2","fubar")

  }
  
  /** This is similar to `DeepSelectionSmallCold` except it only does a single deep select, which is useful for calculating the one-time cost of the bloom filter. */
  object DeepSelectionOnceSmallCold extends Trial('deepSelectOnceSmallCold, "One-time deep selection in a cold 7MB tree") with ColdDeepSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('select, 'small, 'deep, 'cold)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List("MajorFundingAgency2")
  }

  object ShallowSelectionLarge extends Trial('shallowSelectLarge, "Shallow selection in a 30 MB tree") with WarmShallowSelectionTrial {
    override val runLevel = 2
    override val classifiers = Set('select, 'shallow, 'large)

    override val xmlResource = getClass.getResource("/discogs_20110201_labels.xml")
    override val selections = List(List("label","sublabels","label"), List("foo","bar", "label"))    
  }

  object DeepSelectionLarge extends Trial('deepSelectLarge, "Deep selection in a 30 MB tree") with WarmDeepSelectionTrial {
    override val runLevel = 2
    override val classifiers = Set('select, 'deep, 'large)

    override val xmlResource = getClass.getResource("/discogs_20110201_labels.xml")
    override val selections = List("sublabels","fubar")
  }
  
  /* === Modify Trials === */
  
  object ShallowModifyFewSmall extends Trial('shallowModifyFewSmall, "shallow modify a few elements of a 7 MB tree") with ShallowModifyTrial {
    override val classifiers = Set('modify, 'small, 'shallow)

    val xmlResource = getClass.getResource("/spending.xml")
    val selection = List("result","doc","MajorFundingAgency2")
    val attributeToSet = ("WasModified","true")
  }
  
  object ShallowModifyFewSmallSelectionTime extends Trial('shallowModifyFewSmallSel, "selection-only times for [shallowModifyFewSmall]") with WarmShallowSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('modify, 'small, 'shallow)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List(List("result","doc","MajorFundingAgency2"))
  }
  
  object ShallowModifyManySmall extends Trial('shallowModifyManySmall, "shallow modify many elements of a 7 MB tree") with ShallowModifyTrial {
    override val classifiers = Set('modify, 'small, 'shallow)

    val xmlResource = getClass.getResource("/spending.xml")
    val selection = List("result","doc","FiscalYear")
    val attributeToSet = ("WasModified","true")
  }

  object ShallowModifyManySmallSelectionTime extends Trial('shallowModifyManySmallSel, "selection-only times for [shallowModifyManySmall]") with WarmShallowSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('modify, 'small, 'shallow)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List(List("result","doc","FiscalYear"))
  }


  object DeepModifyFewSmall extends Trial('deepModifyFewSmall, "deep modify a few elements of a 7 MB tree") with DeepModifyTrial {
    override val classifiers = Set('modify, 'small, 'deep)

    val xmlResource = getClass.getResource("/spending.xml")
    val selection = "MajorFundingAgency2"
    val attributeToSet = ("WasModified","true")
  }
    
  /** Used to measure the selection-only time for the nodes modified DeepModifySmall */
  object DeepModifyFewSmallSelectionTime extends Trial('deepModifyFewSmallSel, "Selection-only times for [deepModifyFewSmall]") with WarmDeepSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('modify, 'small, 'deep)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List("MajorFundingAgency2")
  }
  
  object DeepModifyManySmall extends Trial('deepModifyManySmall, "deep modification of many elements of a 7 MB tree") with DeepModifyTrial {
    override val classifiers = Set('modify, 'small, 'deep)

    val xmlResource = getClass.getResource("/spending.xml")
    val selection = "FiscalYear"
    val attributeToSet = ("WasModified","true")

    //Hack to force javaXml to use only one warmUp and one run because it's sooooooo slow.     
    override def create = new ModifyInstance {
      override def warmUps(impl: AnyImpl) = if (impl == javaXml) 1 else super.warmUps(impl)
      override def runs(impl: AnyImpl) = if (impl == javaXml) 1 else super.runs(impl)
    }
  }
    
  /** Used to measure the selection-only time for the nodes modified DeepModifySmall */
  object DeepModifyManySmallSelectionTime extends Trial('deepModifyManySmallSel, "Selection-only times for [deepModifyManySmall]") with WarmDeepSelectionTrial {
    override val runLevel = 1
    override val classifiers = Set('modify, 'small, 'deep)
    
    override val xmlResource = getClass.getResource("/spending.xml")
    override val selections = List("FiscalYear")
  }
  
  /* === Zipper Method Trials === */
  
  object ShallowZipperOps extends Trial('zipperShallow, "operations on zipper from a 7 MB tree, shallow selected") with ZipperOpsTrial {
    override val classifiers = Set('zipperOps, 'shallow, 'small)
    override def xmlResource = getClass.getResource("/spending.xml")
    override def createZipper(xml: Elem) = xml \ "result" \ "doc" \ "AgencyID"
    override def unselectCount = 3    
    
    override def nodePred(n: Node): Boolean = (n.asInstanceOf[Elem].children(0).asInstanceOf[Text].text.length & 1) == 0
  }
  
  object DeepZipperOps extends Trial('zipperDeep, "operations on zipper from a 7 MB tree, deep selected") with ZipperOpsTrial {
    override val classifiers = Set('zipperOps, 'shallow, 'small)
    override def xmlResource = getClass.getResource("/spending.xml")
    override def createZipper(xml: Elem) = xml \\ "AgencyID"
    override def unselectCount = 1
    
    override def nodePred(n: Node): Boolean = (n.asInstanceOf[Elem].children(0).asInstanceOf[Text].text.length & 1) == 0
  }

  object HugeZipperOps extends Trial('zipperHuge, "operations on a BIG zipper from a 7 MB tree") with ZipperOpsTrial {
    override val classifiers = Set('zipperOps, 'shallow, 'small)
    override def xmlResource = getClass.getResource("/spending.xml")
    override def createZipper(xml: Elem) = xml \\ anyElem
    override def unselectCount = 1
  }
  
  object HugeDeadZipperOps extends Trial('unselectedZipperHuge, "operations on a BIG dead zipper formed via unselect") with ZipperOpsTrial {
    override val classifiers = Set('zipperOps, 'shallow, 'small)
    override def xmlResource = getClass.getResource("/spending.xml")
    override def createZipper(xml: Elem) = (xml \\ anyElem).stripZipper.select(*).unselect 
    override def unselectCount = 0
  }
  
}
