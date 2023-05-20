package sledger.cli.commands

import cats.effect._
import cats.effect.std.Console
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder
import parsley.character.{char, item}
import parsley.combinator.{eof, many, option}
import parsley.implicits.zipped._
import sledger.Unmarked
import sledger.cli.CliOptions.{CliOpts, defcliopts}
import sledger.cli.commands.Add.AddingStage._
import sledger.data.Amounts._
import sledger.data.Balancing.{balanceTransaction, defBalancingOptions}
import sledger.data.Dates.{EFDay, Exact, fromEFDay, nulldate}
import sledger.data.Journals.{Journal, nulljournal}
import sledger.data.Postings.{Posting, nullposting, posting, sumPostings}
import sledger.data.Transactions.{Transaction, nulltransaction, showTransaction}
import sledger.read.Common.{accountnamep, amountp, codep, datep}
import sledger.utils.Parse.skipNonNewlineSpaces

import java.nio.file.{Files, StandardOpenOption}
import java.time.LocalDate
import scala.Function.const

object Add {
  private class RestartTransactionException() extends Exception

  private case class EntryState(
                                 opts: CliOpts,
                                 args: List[String],
                                 today: LocalDate,
                                 defDate: LocalDate,
                                 journal: Journal,
                                 postings: List[Posting]
                               )

  private def defEntryState: EntryState = EntryState(
    defcliopts,
    args = List.empty, today = nulldate, defDate = nulldate, journal = nulljournal, postings = List.empty
  )

  case class Params(date: LocalDate, code: String, description: String, comment: String)

  sealed trait AddingStage

  object AddingStage {
    case object EnterDateAndCode extends AddingStage

    case class EnterDescAndComment(date: LocalDate, comment: String) extends AddingStage

    case class EnterAccount(params: Params) extends AddingStage

    case class EnterAmountAndComment(params: Params, comment: String) extends AddingStage

    case class EndStage(t: Transaction) extends AddingStage

    case class EnterNewPosting(params: Params, p: Option[Posting]) extends AddingStage
  }


  private case class PrevInput(
                                prevDateAndCode: Option[String],
                                prevDescAndComment: Option[String],
                                prevAccount: List[String],
                                prevAmountAndComment: List[String]
                              )

  def add(cliOps: CliOpts, j: Journal): IO[Unit] =
    Resource
      .make {
        val reader = for {
          terminal <- IO(TerminalBuilder.builder()
            .system(true)
            .dumb(false)
            .jna(true)
            .build())
          lineReader <- IO(LineReaderBuilder.builder().terminal(terminal).build())
        } yield lineReader
        reader
      }(_ => IO(())) // ignore
      .use { reader =>
        val es = defEntryState.copy(
          opts = cliOps,
          today = LocalDate.now(),
          defDate = LocalDate.now(),
          journal = j
        )
        for {
          _ <- Console[IO].println(
            s"Adding transactions to journal file ${j.files.headOption.fold("(unknown)")(_._1)}\n" +
              s"Any command line arguments will be used as defaults.\n" +
              s"An optional ; COMMENT may follow descriptions or amounts.\n" +
              s"If you make a mistake, enter < at any prompt to go one step backward.\n" +
              s"To end a transaction, enter . when prompted.\n" +
              s"To quit, enter . at a date prompt or press control-d or control-c.")
          _ <- getAndAdd(reader, es).recover {
            case _: UserInterruptException => IO(()) // just exit
            case _: EndOfFileException => IO(()) // just exit
          }
        } yield ()
      }

  private def getAndAdd(reader: LineReader, es: EntryState): IO[Unit] = {
    val defaultPrevInput = PrevInput(
      prevDateAndCode = None,
      prevDescAndComment = None,
      prevAccount = List.empty,
      prevAmountAndComment = List.empty
    )

    confirmedTransactionWizard(reader, defaultPrevInput, es, List.empty).flatMap {
      case None => IO.raiseError(new RuntimeException("Could not interpret the input, restarting"))
      case Some(t) =>
        journalAddTransaction(es.journal, t).flatMap { j =>
          IO.println("Saved.") >>
            IO.println("Starting the next transaction (. or ctrl-D/ctrl-C to quit)") >>
            getAndAdd(reader, es.copy(journal = j, defDate = t.date))
        }
    }.recoverWith {
      case _: RestartTransactionException => IO.println("Restarting this transaction.") >> getAndAdd(reader, es)
    }
  }

  private def runPrompt(reader: LineReader, prompt: String, default: String = ""): IO[String] =
    IO(reader.readLine(prompt: String, null: Character, default))

  private def dateAndCodePrompt(reader: LineReader,
                                prevInput: PrevInput, entryState: EntryState): IO[Option[(EFDay, String)]] = {

    val defaultDate = entryState.defDate.toString

    def go(prevInput: PrevInput, entryState: EntryState): IO[Option[(EFDay, String)]] = {

      runPrompt(
        reader = reader,
        prompt = s"Date${showDefault(defaultDate)}: ", default = prevInput.prevDateAndCode.getOrElse("")).flatMap { s =>
        if (s == ".") IO.raiseError(new EndOfFileException()) else {
          val parser = (datep, option(codep), skipNonNewlineSpaces, eof).zipped { (date, mCode, _, _) =>
            (date, mCode.getOrElse(""))
          }
          val defDate = if (s.isEmpty && defaultDate.nonEmpty) defaultDate else s
          val r = (parser <* eof).parse(defDate.toLowerCase).toEither
          r match {
            case Left(value) => IO.println(s"A valid sledger smart date is required. Eg: 2022-08-30. $value") >>
              go(prevInput, entryState)
            case Right((d, c)) => IO.pure(Some(Exact(d), c))
          }
        }
      }
    }

    go(prevInput, entryState)
  }

  private def descAndCommentString(reader: LineReader,
                                   prevInput: PrevInput,
                                   entryState: EntryState): IO[Option[(String, String)]] = {
    val default = entryState.args.headOption.getOrElse("")
    val prevDescAndComment = prevInput.prevDescAndComment.getOrElse("")
    runPrompt(reader, s"Description${showDefault(default)}: ", prevDescAndComment).flatMap { s =>
      if (s == "<") IO.pure(None) else {
        val descAndComment = if (s.isEmpty) default else s
        val (desc, comment) = {
          val (a, b) = descAndComment.span(_ != ';')
          (a.trim, b.dropWhile(_ == ';').trim)
        }
        IO.pure(Some(desc, comment))
      }
    }
  }

  private def postingsBalanced(postings: List[Posting]): Boolean =
    balanceTransaction(defBalancingOptions, nulltransaction.copy(postings = postings)).isRight

  private def accountPrompt(reader: LineReader, prevInput: PrevInput, entryState: EntryState): IO[Option[String]] = {

    def parseAccountOrDotOrNull(canFinish: Boolean, s: String): Option[Option[String]] = {
      (canFinish, s) match {
        case (_, "<") => Some(None) // go back
        case (_, ".") => Some(Some(".")) // end transaction
        case (true, "") => Some(Some("")) // no default, txn is balanced, end transaction
        case (_, s) =>
          val parser = accountnamep <* eof
          parser.parse(s).toEither.fold(const(None), s => Some(s)).map(Some(_))
      }
    }

    val pNum = entryState.postings.length + 1
    val canFinish = entryState.postings.nonEmpty && postingsBalanced(entryState.postings)

    val endMessage = if (canFinish) " or . or enter to finish this transaction" else ""

    val prevAccount = prevInput.prevAccount
    val prevAccountPrompt = prevAccount.lift(entryState.postings.length).getOrElse("")

    def go(canFinish: Boolean, prevInput: PrevInput, entryState: EntryState): IO[Option[String]] = {

      runPrompt(reader,
        s"Account $pNum$endMessage%s: ",
        prevAccountPrompt
      ).flatMap(s => parseAccountOrDotOrNull(canFinish, s) match {
        case Some(value) => IO.pure(value)
        case None => IO.println("A valid sledger account name is required. Eg: assets:cash, expenses:food:eating out.") >>
          go(canFinish, prevInput, entryState)
      })
    }

    go(canFinish, prevInput, entryState)
  }

  private def amountPrompt(reader: LineReader, prevInput: PrevInput,
                           entryState: EntryState): IO[Option[(Amount, String)]] = {

    def parseAmountAndComment(s: String): Option[Option[(Amount, String)]] = {
      if (s == "<") Some(None) else {
        val parser = (amountp, skipNonNewlineSpaces, option(char(';') *> many(item))).zipped { (a, _, mChars) =>
          (a, mChars.fold("")(chars => chars.mkString))
        }
        (parser <* eof).parse(s).toEither.fold(const(None), s => Some(s)).map(Some(_))
      }
    }

    val balancingAmt = maNegate(sumPostings(entryState.postings))
    val balancingAmtFirstCommodity = mixed(amounts(balancingAmt).take(1))

    def showAmt(mixedAmount: MixedAmount): String =
      showMixedAmountB(noColour, mixedAmountSetPrecision(NaturalPrecision, mixedAmount)).builder.result()
    
    val pNum = entryState.postings.length + 1
    val prevAmountAndComment = prevInput.prevAmountAndComment
    val prevAmount = prevAmountAndComment.lift(entryState.postings.length).getOrElse("")
    val default = if(pNum > 1 && !mixedAmountLooksZero(balancingAmt)) showAmt(balancingAmtFirstCommodity) else ""

    def go(prevInput: PrevInput, entryState: EntryState): IO[Option[(Amount, String)]] = {
      runPrompt(reader,
        s"Amount $pNum${showDefault(default)}: ",
        prevAmount
      ).flatMap(s => {
        val amount = if(s.isEmpty && default.length > 1) default else s 
        parseAmountAndComment(amount) match {
          case Some(value) => IO.pure(value)
          case None => IO.println("A valid hledger amount is required. Eg: 1, $2, 3 EUR.") >>
            go(prevInput, entryState)
        }
      })
    }

    go(prevInput, entryState)
  }

  private def saveTransactionPrompt(reader: LineReader, transaction: Transaction): IO[Option[Char]] = {
    def parseSavePrompt(s: String): Option[Option[Char]] = {
      if (s.isEmpty) None else s.strip().map(_.toLower).headOption.map(c => if (c == '<') None else Some(c))
    }

    def go(): IO[Option[Char]] = {
      runPrompt(reader, "Save this transaction to the journal ? y: ").flatMap { s =>
        parseSavePrompt(s) match {
          case Some(value) => IO.pure(value)
          case None => IO.println("Please enter y or n.") >> go()
        }
      }
    }

    IO.println(showTransaction(transaction)) >> go()
  }

  private def confirmedTransactionWizard(reader: LineReader,
                                         prevInput: PrevInput,
                                         es: EntryState,
                                         addingStage: List[AddingStage]): IO[Option[Transaction]] = {
    def replaceNthOrAppend[B](n: Int, newElem: B, xs: List[B]): List[B] = {
      if (n >= 0 && n < xs.length) {
        xs.patch(n, Seq(newElem), 1)
      } else {
        xs :+ newElem
      }
    }

    (prevInput, es, addingStage) match {
      case (prev, es, Nil) => confirmedTransactionWizard(reader, prev, es, List(EnterDateAndCode))

      case (prev, es, stack@currentStage :: _) => currentStage match {
        case EnterDateAndCode => dateAndCodePrompt(reader, prev, es).flatMap {
          case Some((d, code)) =>
            val date = fromEFDay(d)
            val entryState = es.copy(defDate = date, args = es.args.drop(1))
            confirmedTransactionWizard(
              reader,
              prevInput.copy(prevDateAndCode = Some(date.toString)),
              entryState,
              EnterDescAndComment(date, code) :: stack
            )
          case None => confirmedTransactionWizard(reader, prevInput, es, stack)
        }

        case EnterDescAndComment(date, code) => descAndCommentString(reader, prev, es).flatMap {
          case Some((desc, comment)) =>
            val entryState = es.copy(
              args = es.args.drop(1),
              postings = List.empty
            )
            val descAndComment = desc + (if (comment.isEmpty) "" else "  ; " + comment)
            val prev = prevInput.copy(prevDescAndComment = Some(descAndComment))

            confirmedTransactionWizard(
              reader,
              prev,
              entryState,
              addingStage = EnterNewPosting(Params(date, code, desc, comment), None) :: stack
            )
          case None => confirmedTransactionWizard(reader, prev, es, stack.drop(1))
        }

        case EnterNewPosting(params@Params(date, code, description, comment), p) => (es.postings, p) match {
          case (Nil, None) => confirmedTransactionWizard(reader, prev, es, EnterAccount(params) :: stack)
          case (_, Some(_)) => confirmedTransactionWizard(reader, prev, es, EnterAccount(params) :: stack)
          case (_, None) =>
            val t = nulltransaction.copy(
              date = date,
              status = Unmarked,
              code = code,
              description = description,
              comment = comment,
              postings = es.postings
            )
            balanceTransaction(defBalancingOptions, t) match {
              case Right(transaction) => confirmedTransactionWizard(reader, prev, es, EndStage(transaction) :: stack)
              case Left(err) => Console[IO].errorln(s"\n ${err.toUpperCase}please re-enter.") >>
                confirmedTransactionWizard(
                  reader,
                  prev,
                  es.copy(postings = List.empty),
                  stack.dropWhile {
                    case EnterNewPosting(_, None) => false
                    case _ => true
                  }
                )
            }
        }
        case EnterAccount(params) => accountPrompt(reader, prevInput, es).flatMap {
          case Some(account) if List(".", "").contains(account) => (es.postings, postingsBalanced(es.postings)) match {
            case (Nil, _) => Console[IO].errorln("Please enter some postings first.") >>
              confirmedTransactionWizard(reader, prevInput, es, stack)
            case (_, false) => Console[IO].errorln("Please enter more postings to balance the transaction.") >>
              confirmedTransactionWizard(reader, prevInput, es, stack)
            case (_, true) => confirmedTransactionWizard(reader, prevInput, es, EnterNewPosting(params, None) :: stack)
          }
          case Some(account) =>
            val prevAccount = replaceNthOrAppend(es.postings.length, account, prevInput.prevAccount)

            confirmedTransactionWizard(
              reader,
              prevInput.copy(prevAccount = prevAccount),
              es.copy(args = es.args.drop(1)),
              EnterAmountAndComment(params, account) :: stack
            )
          case None =>
            val notPrevAmountAndNotEnterDesc: AddingStage => Boolean = {
              case EnterAmountAndComment(_, _) => false
              case EnterDescAndComment(_, _) => false
              case _ => true
            }

            confirmedTransactionWizard(
              reader,
              prevInput,
              es.copy(postings = if (es.postings.nonEmpty) es.postings.init else List.empty),
              stack.dropWhile(notPrevAmountAndNotEnterDesc)
            )
        }

        case EnterAmountAndComment(params, account) => amountPrompt(reader, prevInput, es).flatMap {
          case Some((amount, comment)) =>
            val p = nullposting.copy(
              account = account.reverse.dropWhile(c => c == ')' || c == ']' || c == '(' || c == '[').reverse,
              amount = mixedAmount(amount)
            )
            val amountAndCommentString = showAmount(amount) + (if (comment.isEmpty) "" else "  ;" + comment)
            val prevAmountAndComment = replaceNthOrAppend(
              es.postings.length,
              amountAndCommentString,
              prevInput.prevAmountAndComment
            )
            val entryState = es.copy(args = es.args.drop(2), postings = es.postings ++ List(p))
            confirmedTransactionWizard(
              reader,
              prevInput.copy(prevAmountAndComment = prevAmountAndComment),
              entryState,
              EnterNewPosting(params, Some(posting)) :: stack
            )
          case None => confirmedTransactionWizard(reader, prevInput, es, stack.drop(1))
        }

        case EndStage(t) => saveTransactionPrompt(reader, t).flatMap {
          case Some('y') => IO.pure(Some(t))
          case Some(_) => throw new RestartTransactionException()
          case None => confirmedTransactionWizard(reader, prevInput, es, stack.drop(2))
        }
      }
    }
  }

  private def appendToFileOrPrintToStdout(filePath: String, str: String): IO[Unit] = {
    val content = str.replaceAll("\r\n", "\n")

    val ioStream = if (filePath == "-") {
      IO(println(content))
    } else {
      Resource.fromAutoCloseable(
        IO(
          Files.newOutputStream(
            java.nio.file.Path.of(filePath),
            StandardOpenOption.CREATE,
            StandardOpenOption.APPEND)
        )
      ).use(outputStream => IO(outputStream.write(content.getBytes("UTF-8"))))
    }

    ioStream
  }

  private def journalAddTransaction(journal: Journal, transaction: Transaction): IO[Journal] = {
    val f = journal.files.headOption.map(_._1).getOrElse("(unknown)")
    for {
      _ <- appendToFileOrPrintToStdout(f, showTransaction(transaction))
    } yield journal.copy(transactions = journal.transactions ++ List(transaction))
  }

  private def showDefault(s: String): String = {
    if (s.isEmpty) ""
    else " [" + s + "]"
  }
}

