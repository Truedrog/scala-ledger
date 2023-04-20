package sledger.data

import parsley.Parsley
import parsley.character.satisfy

import java.time.LocalDate

object Dates {
  val isDateSepChar: Char => Boolean = {
    case c if c == '-' || c == '/' || c == '.' => true
  }
  val datesepchar: Parsley[Char] = satisfy(isDateSepChar)
  def nulldate = LocalDate.of(0, 1, 1)
}
