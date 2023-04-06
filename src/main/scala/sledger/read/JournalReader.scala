package sledger.read

import parsley.Parsley.lookAhead
import parsley.character.newline
import parsley.combinator._
import parsley.implicits.zipped._
import parsley.debug._
import parsley.errors.combinator._

import java.time.LocalDate
import sledger.read.Common.{accountnamep, datep, descriptionp, emptyorcommentlinep, multilinecommentp}
import utils.Parse.{skipNonNewlineSpaces, skipNonNewlineSpaces1, spacenonewline}


object JournalReader {
//  val postingp = (year: Int) => {
//    (skipNonNewlineSpaces1, skipNonNewlineSpaces, accountnamep, skipNonNewlineSpaces).zipped {(_, _, accountname, _) => 
//      optional(amountp)
//    }
//  }
  
  val postingsp = (date:LocalDate) => {
//    many(postingp(date.getYear).label("postings"))
  }
  val transactionp = {
    (datep.label("transaction"), lookAhead((spacenonewline <|> newline).label("whitespace or newline")).void, descriptionp)
      .zipped { (date, _, des) => {}}
  }
  val addJournalItemP = {
    choice(transactionp, emptyorcommentlinep.void, multilinecommentp.void)
  }
  val parser = {
    many(addJournalItemP) <* eof.debug("end of file")
  }
}
