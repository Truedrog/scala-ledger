package sledger
import cats._
import cats.syntax.all._
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import parsley.{Failure, Success}
import sledger.data.Journals.Journal
import sledger.read.JournalReader

import scala.io.Source

object Main extends IOApp.Simple {
  def sourceIO: IO[Source] = IO(Source.fromFile(".hledger.journal", "utf-8"))
  def readLines(source: Source): IO[String] = IO(source.mkString)
  def closeFile(source: Source): IO[Unit] = IO(source.close())

  val bracketRead: IO[String] =
    sourceIO.bracket(src => readLines(src))(src => closeFile(src))
  def run: IO[Unit] = {
    
    val res = for {
      f <- bracketRead
//      _ = println(f)
    } yield JournalReader.parser.parse(f)
    for {
      a <- res
      b = a.toEither
      _ <- IO.println(b.fold(e => s"parse failed $e", j => j.transactions.take(1)))
    } yield ()
  }
}
