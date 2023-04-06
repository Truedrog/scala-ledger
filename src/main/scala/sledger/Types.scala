package sledger

object Types {
  case class Journal()
  case class Transaction()
  case class Posting()
  
  val isDecimalMark: Char => Boolean = {
    case '.' => true
    case ',' => true
    case _   => false 
  }
}



