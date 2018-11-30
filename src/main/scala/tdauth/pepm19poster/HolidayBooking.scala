package tdauth.pepm19poster

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * We want to find the best location in Switzerland for holidays but we have a limited budget in EUR.
  * Unfortunately, the price is only written in CHR, so we have to check for the currency rating using another server.
  * If Switzerland is too expensive, we will go to the USA with the same check.
  * If both locations are too expensive, we stay at home and write our family that we need more money.
  *
  * Future results emulate the requests to external servers.
  * Just imagine we had two different servers for holiday locations in Switzerland and the USA.
  * Besides, there is a server which calculates the currency rating.
  */
object HolidayBooking extends App {
  val budgetEUR = 600
  val x1 = holidayLocationSwitzerland()
    .flatMap(currencyRating)
    .filter(budgetIsSufficient)
  val x2 = holidayLocationUSA()
    .flatMap(currencyRating)
    .filter(budgetIsSufficient)
  val x3 = x1.fallbackTo(x2) // L1
  val x4 = x3.map(bookHoliday).recover(dontBookAnything) // L2
  x4.foreach(letterToFamily)
  x4.foreach(letterToFriends) // L3
  Await.ready(x4, Duration.Inf);

  // Make the price higher for the USA, to get Switzerland as result.
  def holidayLocationSwitzerland() = Future { HolidayLocation(600, "Switzerland", "CHF") }
  // Make both prices higher to don't book anything.
  def holidayLocationUSA() = Future { HolidayLocation(600, "the USA", "USD") }

  def currencyRating(location: HolidayLocation) = location match {
    case HolidayLocation(_, _, "CHF") => Future { HolidayLocationAndRating(location, 1.13) }
    case HolidayLocation(_, _, "USD") => Future { HolidayLocationAndRating(location, 1.14) }
    case _                            => Future.failed(new RuntimeException("Unknown currency!"))
  }

  def budgetIsSufficient(locationAndRating: HolidayLocationAndRating) = locationAndRating.rating * budgetEUR >= locationAndRating.location.price

  def bookHoliday(locationAndRating: HolidayLocationAndRating) =
    locationAndRating.location
  def dontBookAnything: PartialFunction[Throwable, HolidayLocation] = {
    case _ => AtHome
  }
  def letterToFamily(location: HolidayLocation) { println(location.familyLetter) }
  def letterToFriends(location: HolidayLocation) { println(location.friendsLetter) }

  case class HolidayLocation(price: Double, name: String, currency: String) {
    def familyLetter = s"Dear family, I am going to $name."
    def friendsLetter = s"Lets book $name for $price $currency. Join me in my holidays!"
  }
  object AtHome extends HolidayLocation(0.0, "at home", "EUR") {
    override def familyLetter = "Dear family, please send me more money."
    override def friendsLetter = "Don't book anything, I am staying at home."
  }

  case class HolidayLocationAndRating(location: HolidayLocation, rating: Double)
}
