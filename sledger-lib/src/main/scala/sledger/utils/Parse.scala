package sledger.utils
import cats.syntax.all._
import parsley.Parsley
import parsley.character.{newline, satisfy, stringOfMany, stringOfSome}
import parsley.combinator.eof
import sledger.SourcePos


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
    !isNewline(c) && c.isSpaceChar
  }

  val skipNonNewlineSpaces: Parsley[Unit] = takeWhileP(isNonNewlineSpace).void
  val skipNonNewlineSpaces1: Parsley[Unit] = takeWhileP1(isNonNewlineSpace).void
  val skipNonNewlineSpacesb: Parsley[Boolean] = (skipNonNewlineSpaces1 #> true) <|> Parsley.pure(false) 
  val spacenonewline: Parsley[Char] = satisfy(isNonNewlineSpace)
  val eolof: Parsley[Unit] = newline.void <|> eof
  val sourcePosPretty: ((SourcePos, SourcePos)) => String = {
    case (pos1, pos2) =>
      val l2 = if (pos2._2 == 1) pos2._1 - 1 else pos2._2
      show"${pos1._1}-${l2}"
  }
}
