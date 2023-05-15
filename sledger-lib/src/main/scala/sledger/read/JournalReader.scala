package sledger.read

import cats.data._
import cats.effect._
import parsley.Parsley
import parsley.Parsley.{attempt, lookAhead}
import parsley.debug._
import parsley.character.newline
import parsley.combinator._
import parsley.errors.combinator._
import parsley.implicits.zipped._
import parsley.position.pos
import parsley.registers.Reg
import sledger.data.Amounts._
import sledger.data.InputOptions.{InputOpts, defInputOpts}
import sledger.data.Journals.JournalOps._
import sledger.data.Journals._
import sledger.data.Postings._
import sledger.data.Transactions.{Transaction, txnTieKnot}
import sledger.read.Common.{accountnamep, amountp, codep, datep, descriptionp, emptyorcommentlinep, followingcommentp, multilinecommentp, statusp, transactioncommentp}
import sledger.utils.Parse.{skipNonNewlineSpaces, skipNonNewlineSpaces1, spacenonewline}

import java.time.LocalDateTime

object JournalReader {
  private val r = Reg.make[Journal]

  val postingp: Parsley[Posting] = {
    println("posting parse")
    val a = for {
      _ <- skipNonNewlineSpaces.debug("more spaces")
      status <- statusp.debug("statusp")
      _ <- skipNonNewlineSpaces.debug("more spaces")
      acount <- accountnamep.debug("account")
    } yield (status, acount)
    (
      skipNonNewlineSpaces1.debug("first skipNonNewlineSpaces1"),
      attempt(a),
      skipNonNewlineSpaces.debug("spaces after account"),
      option(amountp).debug("amount"),
      skipNonNewlineSpaces.debug("spaces after amount"),
      //      option(balanceassertionp),
      skipNonNewlineSpaces.debug("more spaces?"),
      followingcommentp.debug("followingcommentp")
    ).zipped { (_, b, _, amount, _, _, comment) =>
      posting.copy(
        status = b._1,
        account = b._2,
        amount = amount.fold(missingMixedAmt)(a => mixedAmount(a)),
        //        balanceAssertion = massertion,
        comment = comment)
    }.debug("postingggg")
  }

  val postingsp: Parsley[List[Posting]] = {
    many(postingp.label("postings")) // todo add year postings 
  }.debug("posting")

  val transactionp: Parsley[Transaction] = {
    (
      pos.debug("pos"),
      datep.debug("transaction"),
      lookAhead((spacenonewline <|> newline).label("whitespace or newline")).void,
      descriptionp,
      transactioncommentp,
      statusp,
      codep,
      postingsp.debug("posting"),
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
  }.debug("transactoin parse")

  private val journalp = {
    val addJournalItemP = {
      choice(transactionp.flatMap(t => r.modify(_.addTransaction(t))),
        emptyorcommentlinep.void.debug("empty line"),
        multilinecommentp.void.debug("multilinecommentp")).debug("journal item")
    }.debug("addJournalItemP")
    many(addJournalItemP) *> eof.debug("end of line") *> r.get
  }

  case class ParseError(parseError: String) extends Exception {
    override def getMessage: String = parseError
  }

  def finalizeJournal[F[_] : Sync](file: String, content: String, pj: Journal): EitherT[F, String, Journal] = {
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
    val initJournal = nulljournal
    val p = r.put(initJournal) *> parser
    for {
      parsedJournal <- EitherT {
        Sync[F].pure(p.parse(content).toEither)
      }
      j <- finalizeJournal(file, content, parsedJournal)
    } yield j
  }

  def reader[F[_] : Sync](inputOpts: InputOpts, mbpath: Option[String], content: String): EitherT[F, CommoditySymbol, Journal] = {
    initialiseAndParseJournal(r)(journalp, mbpath.getOrElse("(string)"), content)
  }
}
