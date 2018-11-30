package tdauth.pepm19poster

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * We want to find the best location in the Swiss for holidays but we have a limited budget in EUR.
  * Unfortunately, the price is only written in CHR, so we have to check for the currency rating using another server.
  * If the Swiss is too expensive, we will go to the USA with the same check.
  * If both locations are too expensive, we stay at home and write our family that we need more money.
  *
  * Future results emulate the requests to external servers.
  * Just imagine we had two different servers for holiday locations in the Swiss and USA.
  * Besides, there is a server which calculates the currency rating.
  */
object HolidayBooking extends App {
  val budgetEUR = 600
  val x1 = getSwissHolidayLocation()
    .flatMap(getCurrencyRating)
    .filter(budgetIsSufficient)
  val x2 = getUSAHolidayLocation()
    .flatMap(getCurrencyRating)
    .filter(budgetIsSufficient)
  val x3 = x1.fallbackTo(x2) // L1
  val x4 = x3.map(bookHoliday).recover(dontBookAnything) // L2
  x4.foreach(informMyFamily) // L3
  val copy1 = x4
  val copy2 = x4 // L3

  println(s"Copy 1: $copy1")
  println(s"Copy 2: $copy2")

  def getSwissHolidayLocation() = Future { HolidayLocation(800, "Swiss", "CHF") } // Make both prices higher to don't book anything.
  def getUSAHolidayLocation() = Future { HolidayLocation(600, "USA", "USD") } // Make the price higher for the USA, to get Swiss as result.

  def getCurrencyRating(location: HolidayLocation) = location match {
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
  def informMyFamily(location: HolidayLocation) { println(location.familyMsg) }

  case class HolidayLocation(price: Double, name: String, currency: String) {
    def bookMsg = s"Lets book $name for $price $currency."
    def familyMsg = s"Dear family, I am going to $name."
    override def toString: String = bookMsg
  }
  object AtHome extends HolidayLocation(0.0, "at home", "EUR") {
    override def bookMsg = "Don't book anything."
    override def familyMsg = "Dear family, please send me more money."
  }

  case class HolidayLocationAndRating(location: HolidayLocation, rating: Double)
}
