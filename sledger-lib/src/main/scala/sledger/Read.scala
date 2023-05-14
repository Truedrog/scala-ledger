package sledger

import cats.data._
import cats.effect._
import cats.syntax.all._
import sledger.data.InputOptions.InputOpts
import sledger.data.Journals.Journal
import sledger.read.JournalReader.reader
import sledger.utils.IO.readFileOrStdin

import java.nio.file.{Files, Path, Paths}
import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.util.control.NonFatal

object Read {

  val journalEnvVar = "LEDGER_FILE"
  val journalEnvVar2 = "LEDGER"
  val journalDefaultFilename = ".sledger.journal"

  private def handleIO[F[_]: Sync](f: F[String]): F[String] = {
    f.handleErrorWith {
      case NonFatal(_) => Sync[F].pure("")
    }
  }
  
  def defaultJournalPath[F[_] : Sync]: F[String] = {
    for {
      s <- envJournalPath[F]
      path <- if (s.isEmpty) defPath[F] else s.pure[F]
    } yield path
  }

  private def envJournalPath[F[_] : Sync]: F[String] = {
    def catchIOException[A](io: F[A]): F[Option[A]] =
      io.attempt
        .map(_.toOption)
        .handleError(_ => None)

    val getEnvIO: String => F[String] = key =>
      Sync[F].delay(System.getenv(key)).flatMap {
        case null => Sync[F].raiseError(new RuntimeException(s"Environment variable not found: $key"))
        case value => Sync[F].pure(value)
      }

    handleIO(
      catchIOException(getEnvIO(journalEnvVar)).flatMap {
        case None => catchIOException(getEnvIO(journalEnvVar2)).flatMap {
          case None => Sync[F].pure("")
          case Some(value) => Sync[F].pure(value)
        }
        case Some(value) => Sync[F].pure(value)
      }
    )
  }

  private def defPath[F[_] : Sync]: F[String] = {
    val getHomeDirectoryIO: F[Path] = Sync[F].delay(Paths.get(System.getProperty("user.home")))

    handleIO(
      getHomeDirectoryIO.flatMap { home =>
        Sync[F].delay(Files.createDirectories(home))
          .as(journalDefaultFilename)
          .map(home.resolve)
          .map(_.toString)
      }
    )
  }

  def newJournalContent[F[_] : Sync]: F[String] = {

    def getCurrentDay: F[LocalDate] = {
      Sync[F].delay(ZonedDateTime.now(ZoneId.systemDefault()).toLocalDate)
    }

    val currentDate: F[LocalDate] = getCurrentDay

    currentDate.map { d =>
      s"; journal created ${d.toString} by sledger\n"
    }
  }

  def splitReaderPrefix(f: String): (Option[String], String) = {
    val readerNames = List("journal")

    val prefix = readerNames
      .find(r => f.startsWith(r + ":"))
      .map(r => (Some(r), f.drop(r.length + 1)))
      .getOrElse((None, f))

    prefix
  }

  def readJournal[F[_] : Sync](inputOpts: InputOpts,
                               filepath: Option[String],
                               content: String): EitherT[F, String, Journal] = {
    reader(inputOpts, filepath, content)
  }

  def readJournalFile[F[_] : Sync](inputOptions: InputOpts, filePath: String): EitherT[F, String, Journal] = {
    for {
      _ <- EitherT.liftF(requireJournalFileExists(filePath))
      t <- EitherT.liftF(readFileOrStdin(filePath))
      j <- readJournal(inputOptions, Some(filePath), t)
    } yield j
  }

  def requireJournalFileExists[F[_] : Sync](filePath: String): F[Unit] = {
    if (filePath == "-") {
      Sync[F].unit
    } else {
      for {
        exists <- doesFileExist[F](filePath)
        _ <- if (!exists) {
          val errorMessage = s"The journal file '$filePath' was not found.\n" +
            "Please create it first, e.g., with 'add' or a text editor.\n"
          Sync[F].raiseError(new Exception(errorMessage))
        } else {
          Sync[F].unit
        }
      } yield ()
    }
  }

  def ensureJournalFileExists[F[_] : Sync](filePath: String): F[Unit] = {
    if (filePath == "-") {
      Sync[F].unit
    } else {
      for {
        exists <- doesFileExist[F](filePath)
        _ <- if (!exists) {
          val errorMessage = s"Creating hledger journal file '$filePath'.\n"
          Sync[F].delay(Console.err.println(errorMessage)) *> Sync[F].raiseError(new Exception(errorMessage))
        } else {
          Sync[F].unit
        }
      } yield ()
    }
  }

  def doesFileExist[F[_] : Sync](filePath: String): F[Boolean] = {
    Sync[F].delay(Files.exists(Paths.get(filePath)))
  }
}
