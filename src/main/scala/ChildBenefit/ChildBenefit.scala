package ChildBenefit

object ChildBenefit extends App {
  val EldestChildRate = 26.05 //per week
  val FurtherChildRate = 17.25 //per week
  val reducedRateOneChild = 2.88 //per week
  val reducedRateTwoOrMore = 5.77 //per week per child
  val additionalDisabledRateBenefit = 200.0 //per year

  def isChildEligible(childInFamily: ChildInFamily): Boolean = {
    if (childInFamily.age < 16) true
    else if (childInFamily.age < 20 && childInFamily.inEducation) true
    else false
  }

  //  /** disabled child rate * */
    def additionalDisabledBenefitRate(children: List[ChildInFamily], income: Int): BigDecimal = {
    val countChildrenWithDisability = children.filter(_.isDisabled == true)
    if (countChildrenWithDisability.nonEmpty && income <= 100000)
      countChildrenWithDisability.length * 3.85
    else
      BigDecimal(0)
  }

  def calculateWeeklyAmount(children: List[ChildInFamily], income: Int): BigDecimal = {
    val eligible = children.filter(isChildEligible)

    eligible match {
      case Nil => BigDecimal(0) // Case when there are no eligible children
      case _ if income <= 50000 =>
        BigDecimal(EldestChildRate) + (eligible.length - 1) * BigDecimal(FurtherChildRate)
      case _ if income >= 50001 && income <= 100000 && eligible.length == 1 =>
        BigDecimal(reducedRateOneChild)
      case _ if income >= 50001 && income <= 100000 && eligible.length >= 2 =>
        BigDecimal(reducedRateTwoOrMore) * eligible.length
      case _ => BigDecimal(0) // Default case
    }
  }

  def finalTotalValue(children: List[ChildInFamily], income: Int) = {
    calculateWeeklyAmount(children, income) + additionalDisabledBenefitRate(children, income)
  }

  /** disabled child rate * */
  def additionalDisabledBenefitRate(children: List[ChildInFamily]): Double = {
    children.count(_.isDisabled) * additionalDisabledRateBenefit
  }

  //EXT
  def calculateYearlyAmountEldest(): Double = {
    val rate = EldestChildRate * 52
    (rate * 100).toInt / 100.0
  }

  def calculateYearlyAmountFurtherChild(): Double = {
    val rate = FurtherChildRate * 52
    (rate * 100).toInt / 100.0
  }

  def calculateChildBenefitCharge(income: Int, childBenefitWeekly: Double): Double = {
    if (income <= 50000) 0.0
    else {
      val annualBenefit = childBenefitWeekly * 52
      val incomeDifference = income - 50000
      val chargePercentage = math.min(incomeDifference / 10000.0, 1.0) // 100 for the percentage and / 100 for the formula
      val rawCharge = annualBenefit * chargePercentage
      (rawCharge * 100).toInt / 100.0
    }
  }


}
