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
    
    val res = for {
      f <- bracketRead // todo use resource
    } yield JournalReader.readJournal("file", f)
    for {
      a <- res
      _ <- IO.println(a.toEither)
    } yield ()
  }
}
