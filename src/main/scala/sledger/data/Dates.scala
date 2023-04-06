package sledger.data

import parsley.Parsley
import parsley.character.satisfy

object Dates {
  val isDateSepChar: Char => Boolean = {
    case c if c == '-' || c == '/' || c == '.' => true
  }
  val datesepchar: Parsley[Char] = satisfy(isDateSepChar)
}
