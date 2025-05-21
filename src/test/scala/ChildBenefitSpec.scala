package ChildBenefit

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ChildBenefitSpec extends AnyWordSpec with Matchers {  
  "isChildEligible" should {    
    "return true" when {      
      "child is younger than 16"in {   
        val child = ChildInFamily(age=15, inEducation = true, isDisabled = false)        
        val result = ChildBenefit.isChildEligible(child)        
        val expectedResult = true        
        result shouldBe expectedResult      
      }    
    }  
  }}