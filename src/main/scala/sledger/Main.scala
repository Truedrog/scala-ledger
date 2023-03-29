package sledger

import cats.effect.{IO, IOApp}

import scala.io.Source

object Main extends IOApp.Simple {
  def sourceIO: IO[Source] = IO(Source.fromFile(".hledger.journal", "utf-8"))
  def readLines(source: Source): IO[String] = IO(source.mkString)
  def closeFile(source: Source): IO[Unit] = IO(source.close())

  val bracketRead: IO[String] =
    sourceIO.bracket(src => readLines(src))(src => closeFile(src))
  def run: IO[Unit] = {
    IO.pure(())
    val res = for {
      f <- bracketRead
      _ = println(f)
    } yield JournalReader.parser.parse(f)
    for {
      a <- res
      _ <- IO.println(a)
    } yield ()
  }
}
