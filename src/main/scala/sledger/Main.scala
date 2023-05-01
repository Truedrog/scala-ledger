package sledger

import cats._
  import cats.syntax.all._
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import sledger.read.JournalReader

import scala.io.Source

object Main extends IOApp.Simple {
  def sourceIO: IO[Source] = IO(Source.fromFile(".hledger.journal", "utf-8"))

  def readLines(source: Source): IO[String] = IO(source.mkString)

  def closeFile(source: Source): IO[Unit] = IO(source.close())

  val bracketRead: IO[String] =
    sourceIO.bracket(src => readLines(src))(src => closeFile(src))

  def run: IO[Unit] = {
    (for {
      f <- bracketRead
      ef <- JournalReader.readJournal[IO].apply("file", f).value
      result <- ef match {
        case Left(error) => IO.raiseError(JournalReader.ParseError(s"Error: ${error}"))
        case Right(journal) => IO.println(journal)
      }
    } yield result).handleErrorWith { error =>
      Console[IO].errorln(s"sledger: ${error.getMessage}")
    }
  }
}
