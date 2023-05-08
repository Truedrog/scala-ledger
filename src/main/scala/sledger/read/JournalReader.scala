package sledger.read

import cats.data._
import cats.effect._
import parsley.Parsley
import parsley.Parsley.lookAhead
import parsley.character.newline
import parsley.combinator._
import parsley.debug._
import parsley.errors.combinator._
import parsley.implicits.zipped._
import parsley.position.pos
import parsley.registers.Reg
import sledger.data.Amounts._
import sledger.data.InputOptions.defInputOpts
import sledger.data.Journals._
import sledger.data.Journals.JournalOps._
import sledger.data.Postings._
import sledger.data.Transactions.{Transaction, txnTieKnot}
import sledger.read.Common.{accountnamep, amountp, codep, datep, descriptionp, emptyorcommentlinep, followingcommentp, multilinecommentp, statusp, transactioncommentp}
import sledger.utils.Parse.{skipNonNewlineSpaces, skipNonNewlineSpaces1, spacenonewline}

import java.time.LocalDateTime

object JournalReader {
  private val r = Reg.make[Journal]
  
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
        amount = amount.fold(missingMixedAmt)(a => mixedAmount(a)),
        //        balanceAssertion = massertion,
        comment = comment)
    }
  }

  val postingsp: Parsley[List[Posting]] = {
    many(postingp.label("postings")) // todo add year postings 
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
    ).zipped { (startPos, date, _, desc, comment, status, code, postings, endPos) =>
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
    }
  }
  
  private val journalp = {
    val addJournalItemP = {
      choice(transactionp.flatMap(t => r.modify(_.addTransaction(t))),
        emptyorcommentlinep.void,
        multilinecommentp.void)
    }
    many(addJournalItemP) *> eof *> r.get
  }
  
  case class ParseError(parseError: String) extends Exception {
    override def getMessage: String = parseError
  }

  def finalizeJournal[F[_]:Sync](file: String, content: String, pj: Journal): EitherT[F, String, Journal] = {
    val definopts = defInputOpts
    import sledger.data.Journals.JournalOps._
    val t = Sync[F].delay(LocalDateTime.now())
    for {
      time <- EitherT.liftF(t)
      j <- EitherT(Sync[F].delay {
        pj.withLastReadTime(time)
          .withFile(file, content)
          .reverse
          .withAccountTypes
          .applyCommodityStyles
          .flatMap(_.balanceTransactions)
      })
    } yield j
  }
  
  def initialiseAndParseJournal[F[_] : Sync](r: Reg[Journal])
                                            (parser: Parsley[Journal], file: String = "", content: String = ""): EitherT[F, String, Journal] = {
    val initJournal = nulljournal.copy()
    val p = r.put(initJournal) *> parser
    for {
      parsedJournal <- EitherT {
        Sync[F].pure(p.parse(content).toEither)
      }
      j <- finalizeJournal(file, content, parsedJournal)
    } yield j
  }


  def readJournal[F[_] : Sync]: (CommoditySymbol, CommoditySymbol) => EitherT[F, String, Journal] = 
    initialiseAndParseJournal(r)(journalp, _, _)
}
