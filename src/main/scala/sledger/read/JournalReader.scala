package sledger.read

import parsley.Parsley
import parsley.Parsley.lookAhead
import parsley.character.newline
import parsley.combinator._
import parsley.implicits.zipped._
import parsley.debug._
import parsley.errors.combinator._
import parsley.position.pos
import parsley.registers.Reg
import sledger.data.Amounts._
import sledger.data.Journals._
import sledger.data.Postings._
import sledger.data.Transactions.{Transaction, txnTieKnot}
import sledger.read.Common.{accountnamep, amountp, codep, datep, descriptionp, emptyorcommentlinep, followingcommentp, multilinecommentp, statusp, transactioncommentp}
import utils.Parse.{skipNonNewlineSpaces, skipNonNewlineSpaces1, spacenonewline}

object JournalReader {
  val postingp: Parsley[Posting] = {
    (skipNonNewlineSpaces1,
      skipNonNewlineSpaces,
      accountnamep.debug("account name"),
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
  }.debug("posting")
  
  val postingsp: Parsley[List[Posting]] = {
    many(postingp.label("postings"))// todo add year postings 
  }
  
  val transactionp: Parsley[Transaction] = {
    (
      pos,
      datep.label("transaction"),
      lookAhead((spacenonewline <|> newline).label("whitespace or newline")).void,
      descriptionp.debug("description"),
      transactioncommentp.debug("transactioncommentp"),
      statusp.debug("statusp"),
      codep.debug("codep"),
      postingsp.debug("postingsp"),
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
  val r = Reg.make[Journal]

  val addJournalItemP = {
    choice(transactionp.debug("transactionp").flatMap(t => r.modify(j => j.copy(transactions = t :: j.transactions))), 
      emptyorcommentlinep.void.debug("emptyorcommentlinep"), 
      multilinecommentp.void.debug("multilinecommentp"))
  }
  val parser = r.put(nulljournal) *> (many(addJournalItemP).debug("many add journalItem") *> eof *> r.get)
}
