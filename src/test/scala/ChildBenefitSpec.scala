package ChildBenefit

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ChildBenefitSpec extends AnyWordSpec with Matchers {

  "isChildEligible" should {
    "return true" when {
      "child is younger than 16" in {
        val child = ChildInFamily(age = 15, inEducation = true, isDisabled = false)
        val result = ChildBenefit.isChildEligible(child)
        val expectedResult = true
        result shouldBe expectedResult
      }
    }
  }

  "isChildEligible" should {
    "return true" when {
      "child age is between 16 and 19 and enrolled in approved education" in {
        val child = ChildInFamily(age = 17, inEducation = true, isDisabled = false)
        val result = ChildBenefit.isChildEligible(child)
        val expectedResult = true
        result shouldBe expectedResult
      }
    }
  }

  "isChildEligible" should {
    "return false" when {
      "child age is between 16 and 19 and not enrolled in approved education" in {
        val child = ChildInFamily(age = 16, inEducation = false, isDisabled = false)
        val result = ChildBenefit.isChildEligible(child)
        val expectedResult = false
        result shouldBe expectedResult
      }
    }
  }

  "isChildEligible" should {
    "return false" when {
      "child is oder than 19" in {
        val child = ChildInFamily(age = 20, inEducation = true, isDisabled = false)
        val result = ChildBenefit.isChildEligible(child)
        val expectedResult = false
        result shouldBe expectedResult
      }
    }
  }



  
  "calculateWeeklyAmount" should {
    "return full benefit (eldest child rate)" when {
      "Family has one child under 16 and an annual income less than £50,001" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = false))
        val income: Int = 40000
        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(26.05)
        result shouldBe expectedResult
      }
    }
  }

  "calculateWeeklyAmount" should {
    "return full benefit (eldest child rate and additional children rate)" when {
      "Family has two children under 16 and an annual income less than £50,001" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = false),
          ChildInFamily(age = 7, inEducation = true, isDisabled = false))
        val income: Int = 35000
        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(43.30)
        result shouldBe expectedResult
      }
    }
  }

  "calculateWeeklyAmount" should {
    "return full benefit (eldest child rate and additional children rate)" when {
      "Family has three children under 16 and an annual income less than £50,001" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = false),
          ChildInFamily(age = 7, inEducation = true, isDisabled = false),
          ChildInFamily(age = 10, inEducation = true, isDisabled = false))
        val income: Int = 50000
        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(60.55)
        result shouldBe expectedResult
      }
    }
  }

  "calculateWeeklyAmount" should {
    "return full benefit (eldest child rate and additional children rate), and not return benefit for child older than 16 " +
      "and in not approved education" when {
      "Family has two children under 16, one child aged between 16 and 19 not in approved education and an annual income less than £50,001" in {
        val children = List(ChildInFamily(age = 17, inEducation = false, isDisabled = false),
          ChildInFamily(age = 7, inEducation = true, isDisabled = false),
          ChildInFamily(age = 10, inEducation = true, isDisabled = false))
        val income: Int = 46700

        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(43.30)
        result shouldBe expectedResult
      }
    }
  }

  "calculateWeeklyAmount" should {
    "return reduced benefit" when {
      "Family has one child under 16 and an annual income between £50,001 and £100,000" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = false))
        val income: Int = 52000
        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(2.88)
        result shouldBe expectedResult
      }
    }
  }

  "calculateWeeklyAmount" should {
    "return reduced benefit" when {
      "Family has two children under 16 and an annual income between £50,001 and £100,000" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = false),
          ChildInFamily(age = 10, inEducation = true, isDisabled = false))
        val income: Int = 60000
        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(11.54)
        result shouldBe expectedResult
      }
    }
  }

  "calculateWeeklyAmount" should {
    "no return benefit" when {
      "Family income is more than £100000" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = true),
          ChildInFamily(age = 10, inEducation = true, isDisabled = false))
        val income: Int = 120000
        val result = ChildBenefit.calculateWeeklyAmount(children, income)
        val expectedResult: BigDecimal = BigDecimal(0)
        result shouldBe expectedResult
      }
    }
  }



  "finalTotalValue" should {
    "return full benefit and special circumstances rate" when {
      "Family has one child under 16 with special circumstances and an annual income less than £50,001" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = true))
        val income: Int = 40000
        val result = ChildBenefit.finalTotalValue(children, income)
        val expectedResult: BigDecimal = BigDecimal(29.90)
        result shouldBe expectedResult
      }
    }
  }

  "finalTotalValue" should {
    "return full benefit and special circumstances rate" when {
      "Family has two children under 16 with special circumstances and an annual income less than £50,001" in {
        val children = List(ChildInFamily(age = 13, inEducation = true, isDisabled = true),
          ChildInFamily(age = 10, inEducation = true, isDisabled = false))
        val income: Int = 40000
        val result = ChildBenefit.finalTotalValue(children, income)
        val expectedResult: BigDecimal = BigDecimal(47.15)
        result shouldBe expectedResult
      }
    }
  }

  "finalTotalValue" should {
    "return reduced benefit and special circumstances rate" when {
      "Family has one child under 16 with special circumstances and an annual income between £50,001 and £100,000" in {
        val children = List(ChildInFamily(age = 7, inEducation = true, isDisabled = true))
        val income: Int = 57000
        val result = ChildBenefit.finalTotalValue(children, income)
        val expectedResult: BigDecimal = BigDecimal(6.73)
        result shouldBe expectedResult
      }
    }
  }

  "finalTotalValue" should {
    "return reduced benefit and special circumstances rate" when {
      "Family has two children under 16 with special circumstances and an annual income between £50,001 and £100,000" in {
        val children = List(ChildInFamily(age = 7, inEducation = true, isDisabled = true),
          ChildInFamily(age = 14, inEducation = true, isDisabled = false))
        val income: Int = 63000
        val result = ChildBenefit.finalTotalValue(children, income)
        val expectedResult: BigDecimal = BigDecimal(15.39)
        result shouldBe expectedResult
      }
    }
  }

  "finalTotalValue" should {
    "return no benefits" when {
      "Family has two children under 16 with special circumstances and an annual income over £100,000" in {
        val children = List(ChildInFamily(age = 7, inEducation = true, isDisabled = true),
          ChildInFamily(age = 14, inEducation = true, isDisabled = false))
        val income: Int = 100023
        val result = ChildBenefit.finalTotalValue(children, income)
        val expectedResult: BigDecimal = BigDecimal(0)
        result shouldBe expectedResult
      }
    }
  }




  "additionalDisabledBenefitRate" should {
    "return 0.0" when {
      "there are no disabled children in the list" in {
        val children = List(
          ChildInFamily(age = 5, inEducation = true, isDisabled = false),
          ChildInFamily(age = 10, inEducation = true, isDisabled = false)
        )
        val result = ChildBenefit.additionalDisabledBenefitRate(children)
        val expectedResult: Double = 0.00
        result shouldBe expectedResult
      }
    }
  }

  "additionalDisabledBenefitRate" should {
    "return the correct total additional benefit" when {
      "there are disabled children in the list" in {
        val children = List(
          ChildInFamily(age = 7, inEducation = true, isDisabled = true),
          ChildInFamily(age = 14, inEducation = true, isDisabled = false),
          ChildInFamily(age = 3, inEducation = false, isDisabled = true)
        )
        val result = ChildBenefit.additionalDisabledBenefitRate(children)
        val expectedResult = 2 * 200
        result shouldBe expectedResult
      }
    }
  }



  
  "calculateYearlyAmountEldest" should {
    "return yearly amount eldest child rate" when {
      "Family has one child under 16 and an annual income less than £50,001" in {
        val expectedResult = 1354.60
        val result = ChildBenefit.calculateYearlyAmountEldest()
        result shouldBe expectedResult
      }
    }
  }

  "calculateYearlyAmountFurtherChild" should {
    "return yearly amount further child rate" when {
      "Family has two children under 16 and an annual income less than £50,001" in {
        val expectedResult = 897.00
        val result = ChildBenefit.calculateYearlyAmountFurtherChild()
        result shouldBe expectedResult
      }
    }
  }


  "calculateChildBenefitCharge" should {
    "return 0" when {
      "Family income is less than £50,001" in {
        val expectedResult = 0.0
        val result = ChildBenefit.calculateChildBenefitCharge(50000, 1345)
        result shouldBe expectedResult
      }
    }
  }

  "calculateChildBenefitCharge" should {
    "return partial charge" when {
    "Family income between £50,000 and £60,000" in {
      val income = 55000
      val weeklyBenefit = 26.05
      val expectedResult = 677.30
      val result = ChildBenefit.calculateChildBenefitCharge(income, weeklyBenefit)
      result shouldBe expectedResult
    }
  }
}

  "calculateChildBenefitCharge" should {
    "return full charge (no childBenefit)" when {
      "Family income over £60,000" in {
        val income = 61000
        val weeklyBenefit = 26.05
        val expectedResult = 1354.60
        val result = ChildBenefit.calculateChildBenefitCharge(income, weeklyBenefit)
        result shouldBe expectedResult
      }
    }
  }
}

