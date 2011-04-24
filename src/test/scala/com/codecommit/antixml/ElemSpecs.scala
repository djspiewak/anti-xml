package com.codecommit.antixml

import org.specs._

object ElemSpecs extends Specification {
  "XML elements" should {
    "serialize empty elements correctly" in {
      <br/>.anti.toString mustEqual "<br/>"
    }
  }
}
