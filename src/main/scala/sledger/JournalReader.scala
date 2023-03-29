package sledger
import cats.effect.{IO, IOApp}
import parsley.Parsley, Parsley.attempt
import parsley.character.{endOfLine, char, item, newline, satisfy, string, whitespace}
import parsley.combinator.{choice, many, manyUntil, optional, skip, whileP, eof}
import parsley.debug._

object JournalReader {
  
  private[this] def takeWhileP(p: Char => Boolean) = many(satisfy(p))
  
  val parser: Parsley[Unit] = {
    val isNewLine: Char => Boolean = {
      case '\n' => true
      case _ => false
    }
    val isLineCommentStart: Char => Boolean = {
      case '#' => true
      case '*' => true
      case ';' => true
      case _ => false
    }
    val isNonNewlineSpace: Char => Boolean = (c: Char) => {
      !isNewLine(c) && c.isWhitespace
    }
    
    val skipNonNewlineSpaces: Parsley[Unit] = takeWhileP(isNonNewlineSpace).void
    val emptyorcommentlinep: Parsley[Unit] = {
      val skiplinecommentp: Parsley[Unit] = {
        satisfy(isLineCommentStart) *> takeWhileP(_ != '\n') *> optional(endOfLine)
      }
      skipNonNewlineSpaces *> skiplinecommentp <|> endOfLine.void
    }
    
    val multilinecommentp: Parsley[Unit] = {
      val trailingSpaces = skipNonNewlineSpaces <* newline
      val startComment = attempt(string("comment")) *> trailingSpaces
      val endComment = eof <|> attempt(string("end comment")) *> trailingSpaces
      val anyLine = takeWhileP(_ != '\n') *> newline.void
      startComment *> manyUntil(anyLine.debug("any line"), endComment).void
    }

    val addJournalItemP: Parsley[Unit] = {
      choice(emptyorcommentlinep.void, multilinecommentp.void)
    }
    
    many(addJournalItemP) *> eof.debug("end of file").void
  }
}
