package sledger.text.tabular
import cats._
import cats.data._
import cats.syntax.all._

object Tabular {
  sealed trait Properties

  case object NoLine extends Properties

  case object SingleLine extends Properties

  case object DoubleLine extends Properties

  sealed trait TableHeader[+A]

  final case class Header[A](a: A) extends TableHeader[A]

  final case class Group[A](properties: Properties, headers: List[TableHeader[A]]) extends TableHeader[A]

  object TableHeader {
    implicit val functorTH: Functor[TableHeader] = new Functor[TableHeader] {
      override def map[A, B](fa: TableHeader[A])(f: A => B): TableHeader[B] = fa match {
        case Header(a) => Header(f(a))
        case Group(properties, headers) => Group(properties, headers.map(h => map(h)(f)))
      }
    }
  }

  def headerContents[A](h: TableHeader[A]): List[A] = {
    h match {
      case Header(a) => List(a)
      case Group(_, headers) => headers.flatMap(headerContents)
    }
  }

  def zipHeader[H, A](e: H, ss: List[H], h: TableHeader[A]): TableHeader[(H, A)] = {
    def runner(header: TableHeader[A]): State[List[H], TableHeader[(H, A)]] = {
      State { initial => {
        header match {
          case Header(a) => initial match {
            case s :: ss => (ss, Header(s, a))
            case Nil => (initial, Header(e, a))
          }
          case Group(properties, headers) =>
            headers.traverse(runner).map(a => Group(properties, a)).run(initial).value
        }
      }
      }
    }

    runner(h).runA(ss).value
  }

  def flattenHeader[A](h: TableHeader[A]): List[Either[Properties, A]] = {
    h match {
      case Header(a) => List(Right(a))
      case Group(properties, headers) =>
        val hs: List[List[Either[Properties, A]]] = headers.map(flattenHeader[A])
        val pxs = List(Left(properties))
        Foldable[List].intercalate(hs, pxs)
    }
  }

  final case class Table[R, C, A](rh: TableHeader[R], ch: TableHeader[C], cells: List[List[A]])
  def empty = Table(Group(NoLine, List.empty), Group(NoLine, List.empty), List.empty)
  
}
