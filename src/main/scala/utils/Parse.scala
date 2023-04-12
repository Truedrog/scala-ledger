package utils
import cats._
import cats.data._
import cats.syntax.all._
import parsley.Parsley
import parsley.character.{satisfy, stringOfMany, stringOfSome, newline}
import parsley.combinator.{eof}


object Parse {
  def takeWhileP(p: Char => Boolean): Parsley[String] = stringOfMany(satisfy(p))
  def takeWhileP1(p: Char => Boolean): Parsley[String] = stringOfSome(satisfy(p))
  
  val isNewline: Char => Boolean = {
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
    !isNewline(c) && c.isWhitespace
  }

  val skipNonNewlineSpaces: Parsley[Unit] = takeWhileP(isNonNewlineSpace).void
  val skipNonNewlineSpaces1: Parsley[Unit] = takeWhileP1(isNonNewlineSpace).void
  val skipNonNewlineSpacesb: Parsley[Boolean] = (skipNonNewlineSpaces1 #> true) <|> Parsley.pure(false) 
  val spacenonewline: Parsley[Char] = satisfy(isNonNewlineSpace)
  val eolof = newline.void <|> eof
}
