package ChildBenefit

object ChildBenefit extends App {
  val EldestChildRate = 26.05 //per week
  val FurtherChildRate = 17.25 //per week
  // val fullChildBenefit = 500.0 //per year ??? 500
  // val reducedRateTwoOrMoreBenefit = 300.0 //per year  ??? 300
  // val reducedRateLessThanTwoBenefit = 150.0 //per year ??? 150
  val additionalDisabledRateBenefit = 200.0 //per year

  def isChildEligible(childInFamily: ChildInFamily): Boolean = {
    if (childInFamily.age < 16) true
    else if (childInFamily.age < 20 && childInFamily.inEducation) true
    else false
  }

  def calculateWeeklyAmount(children: List[ChildInFamily]): BigDecimal = {
    val eligible = children.filter(isChildEligible) //filters elig children
    if (eligible.isEmpty)
      BigDecimal(0)
    else
      BigDecimal(EldestChildRate) + (eligible.length - 1) * BigDecimal(FurtherChildRate)
  }

  /** disabled child rate * */
  def additionalDisabledBenefitRate(children: List[ChildInFamily]): Double = {
    children.count(_.isDisabled) * additionalDisabledRateBenefit
  }
//  def calculateYearlyAmountEldest (): Double = {
//     ??? * ??? //we are expecting here EldestChildRate * 52
//      }
//
//  def calculateYearlyAmountFurtherChild (): Double = {
// ??? * ???
// we are expecting here FurtherChildRate * 52
//  }
}
