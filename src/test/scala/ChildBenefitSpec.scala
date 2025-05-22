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
        val expectedResult = true
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






}
