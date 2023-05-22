package sledger

import cats.syntax.all._
import sledger.Queries.Query._
import sledger.data.AccountNames.{AccountName, AccountType, accountNameLevel, isAccountSubtypeOf}
import sledger.data.Dates._
import sledger.data.Postings.{Posting, postingDate, postingDate2}

import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

object Queries {
  sealed trait Query

  object Query {
    case class Not(query: Query) extends Query

    case class And(queries: List[Query]) extends Query

    case class Or(queries: List[Query]) extends Query

    case object Any extends Query

    case object Nothing extends Query

    case class Date(dateSpan: DateSpan) extends Query

    case class Date2(dateSpan: DateSpan) extends Query

    case class Depth(level: Int) extends Query

    case class Acct(r: Regex) extends Query

    case class Type(at: List[AccountType]) extends Query

  }

  trait QueryOpt // todo implement

  val defaultprefix = "acct"

  def partitionEithers[A, B](eithers: List[Either[A, B]]): (List[A], List[B]) = {
    eithers.foldRight((List.empty[A], List.empty[B])) {
      case (Left(a), (lefts, rights)) => (a :: lefts, rights)
      case (Right(b), (lefts, rights)) => (lefts, b :: rights)
    }
  }

  def parseQueryList(queryString: List[String]): Either[String, (Query, List[QueryOpt])] = {
    for {
      terms <- queryString.traverse(a => parseQueryTerm(a))
      (pats, opts) = partitionEithers(terms)
      (accPats, _) = pats.partition(queryIsAcct)
      q = simplifyQuery(And(List(Or(accPats))))
    } yield (q, opts)
  }

  def toRegexCI(pattern: String): Regex = {
    val regexPattern = pattern.replace("\\", "\\\\").replace(".", "\\.")
    new Regex(regexPattern, "i")
  }

  @tailrec
  def parseQueryTerm(term: String): Either[String, Either[Query, QueryOpt]] = {
    term match {
      case s if s.startsWith("acct:") =>
        Try(toRegexCI(s.stripPrefix("acct:"))).toEither.leftMap(_.getMessage).map(s => Left(Acct(s)))
      case s => parseQueryTerm(defaultprefix + ":" + s)
      case _ => Left("query not implemented")
    }
  }

  def simplifyQuery(query: Query): Query = {

    def simplify(query: Query): Query = query match {
      case And(Nil) => Any
      case And(List(q)) => simplify(q)
      case And(qs) =>
        if (qs.forall(==)) simplify(qs.head) else if (qs.contains(Nothing)) {
          Nothing
        } else if (qs.forall(queryIsDate)) {
          Date(spansIntersect(qs.flatMap(queryTermDateSpan)))
        } else {
          val qss = qs.filter(q => q != Any)
          val (datesq, othersq) = qss.partition(queryIsDate)
          And(datesq.map(simplify) ++ othersq.map(simplify))
        }
      case Or(Nil) => Any
      case Or(List(q)) => simplifyQuery(q)
      case Or(qs) =>
        if (qs.forall(==)) simplify(qs.head) else if (qs.contains(Any)) {
          Any
        } else {
          Or(qs.filter(q => q != Nothing).map(simplify))
        }
      case Date(DateSpan(None, None)) => Any
      case Date2(DateSpan(None, None)) => Any
      case q => q
    }

    val q1 = simplify(query)
    if (q1 == query) query else simplifyQuery(q1)
  }

  def queryIsDate(query: Query): Boolean = {
    query match {
      case Date(_) => true
      case _ => false
    }
  }

  def queryIsDate2(query: Query): Boolean = {
    query match {
      case Date2(_) => true
      case _ => false
    }
  }

  def queryIsDateOrDate2(query: Query): Boolean = {
    query match {
      case Date(_) => true
      case Date2(_) => true
      case _ => false
    }
  }

  def queryIsAcct(query: Query): Boolean = query match {
    case Acct(_) => true
    case _ => false
  }

  def queryIsDepth(query: Query): Boolean = {
    query match {
      case Depth(_) => true
      case _ => false
    }
  }
  def queryDateSpan(secondary: Boolean, query: Query): DateSpan = (secondary, query) match {
    case (secondary, Or(queries)) => spansUnion(queries.map(queryDateSpan_))
    case (secondary, And(queries)) => spansIntersect(queries.map(queryDateSpan_))
    case (_, Date(dateSpan)) => dateSpan
    case (true, Date2(dateSpan)) => dateSpan
    case (_, _) => nulldatespan
  }
  def queryDateSpan_(query: Query): DateSpan = query match {
    case Or(queries) => spansUnion(queries.map(queryDateSpan_))
    case And(queries) => spansIntersect(queries.map(queryDateSpan_))
    case Date(dateSpan) => dateSpan
    case Date2(dateSpan) => dateSpan
    case _ => nulldatespan
  }

  def queryTermDateSpan(query: Query): Option[DateSpan] = {
    query match {
      case Date(spn) => Some(spn)
      case _ => None
    }
  }

  def queryDepth(query: Query): Option[Int] = {
    def go(query: Query): List[Int] = {
      query match {
        case Depth(level) => List(level)
        case Or(queries) => queries.flatMap(q => go(q))
        case And(queries) => queries.flatMap(q => go(q))
        case _ => List()
      }
    }

    go(query).minOption
  }

  def matchesAccount(query: Query, account: AccountName): Boolean = {
    (query, account) match {
      case (Nothing, _) => false
      case (Not(m), a) => !matchesAccount(m, a)
      case (Or(ms), a) => ms.exists(q => matchesAccount(q, a))
      case (And(ms), a) => ms.forall(q => matchesAccount(q, a))
      case (Acct(r), a) => {
        println(r, a, r.findFirstIn(a))
        r.findFirstIn(a).isDefined
      }
      case (Depth(d), a) => accountNameLevel(a) <= d
      case (_, _) => true
    }
  }

  def matchesPosting(q: Query, posting: Posting): Boolean = {

    (q, posting) match {
      case (Not(query), p) => matchesPosting(query, p)
      case (Any, _) => true
      case (Nothing, _) => false
      case (Or(qs), p) => qs.exists(q => matchesPosting(q, p))
      case (And(qs), p) => qs.forall(q => matchesPosting(q, p))
      case (Date(spn), p) => spanContainsDate(spn, postingDate(p))
      case (Acct(r), p) =>
        r.findFirstIn(p.account).isDefined || p.original.fold(false)(p => r.findFirstIn(p.account).isDefined)
      case (Date2(spn), p) => spanContainsDate(spn, postingDate2(p))
      case (q@Depth(_), p) => matchesAccount(q, p.account)
      case (Type(_), _) => false
    }
  }

  def matchesPostingExtra(atype: AccountName => Option[AccountType], query: Query, posting: Posting): Boolean = {
    (atype, query, posting) match {
      case (atype, Not(q), p) => !matchesPostingExtra(atype, q, p)
      case (atype, Or(qs), p) => qs.exists(q => matchesPostingExtra(atype, q, p))
      case (atype, And(qs), p) => qs.forall(q => matchesPostingExtra(atype, q, p))
      case (atype, Type(ts), p) => atype(p.account).fold(false)(t => ts.exists(t1 => isAccountSubtypeOf(t, t1)))
      case (_, q, p) => matchesPosting(q, p)
    }
  }

  def filterQuery(p: Query => Boolean, query: Query): Query = simplifyQuery(filterQuery1(p, query))

  def filterQuery1(p: Query => Boolean, query: Query): Query = {
    (p, query) match {
      case (p, And(qs)) => And(qs.map(q => filterQuery(p, q)))
      case (p, Or(qs)) => Or(qs.map(q => filterQuery(p, q)))
      case (p, q) => if (p(q)) q else Any
    }
  }
}
