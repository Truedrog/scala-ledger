package sledger.read

import parsley.Parsley
import parsley.Parsley.lookAhead
import parsley.character.newline
import parsley.combinator._
import parsley.implicits.zipped._
import parsley.debug._
import parsley.errors.combinator._
import parsley.position.pos

import sledger.data.Amounts._
import sledger.data.Postings._
import sledger.data.Transactions.{Transaction, txnTieKnot}
import sledger.read.Common.{accountnamep, amountp, codep, datep, descriptionp, emptyorcommentlinep, followingcommentp, multilinecommentp, statusp, transactioncommentp}
import utils.Parse.{skipNonNewlineSpaces, skipNonNewlineSpaces1, spacenonewline}

object JournalReader {
  val postingp: Parsley[Posting] = {
    (skipNonNewlineSpaces1,
      skipNonNewlineSpaces,
      accountnamep,
      skipNonNewlineSpaces,
      option(amountp),
      skipNonNewlineSpaces,
//      option(balanceassertionp),
      skipNonNewlineSpaces,
      followingcommentp
    ).zipped { (_, _, accountname, _, amount, _, _, comment) => 
      posting.copy(account = accountname, 
        amount = amount.fold(missingmixedamt)(a => mixedAmount(a)), 
//        balanceAssertion = massertion,
        comment = comment)
    }
  }
  
  val postingsp: Parsley[List[Posting]] = {
    many(postingp.label("postings"))// todo add year postings 
  }
  
  val transactionp: Parsley[Transaction] = {
    (
      pos,
      datep.label("transaction"),
      lookAhead((spacenonewline <|> newline).label("whitespace or newline")).void,
      descriptionp,
      transactioncommentp,
      statusp,
      codep,
      postingsp,
      pos,
    ).zipped { (startPos, date, _, desc, comment, status, code, postings, endPos) => {
      val sourcePos = (startPos, endPos)
      txnTieKnot(Transaction(0, 
        "", 
        sourcepos = sourcePos, 
        date = date, date2 = None, 
        status = status, 
        code = code, 
        description = desc, 
        comment = comment, 
        postings = postings))
    }}
  }
  
  val addJournalItemP = {
    choice(transactionp, emptyorcommentlinep, multilinecommentp)
  }
  val parser = {
    many(addJournalItemP) <* eof.debug("end of file")
  }
}
