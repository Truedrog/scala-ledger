package sledger

import sledger.Queries.Query._
import sledger.data.AccountNames.{AccountName, AccountType}
import sledger.data.Dates.{DateSpan, spanContainsDate, spansIntersect}
import sledger.data.Postings.{Posting, postingDate, postingDate2}

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

  def queryIsDepth(query: Query): Boolean = {
    query match {
      case Depth(_) => true
      case _ => false
    }
  }

  def queryTermDateSpan(query: Query): Option[DateSpan] = {
    query match {
      case Date(spn) => Some(spn)
      case _ => None
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
      case (Date2(spn), p) => spanContainsDate(spn, postingDate2(p))
      case (Depth(level), _) => true // todo add account match
    }
  }

  def matchesPostingExtra(atype: AccountName => Option[AccountType], query: Query, posting: Posting): Boolean = {
    (atype, query, posting) match {
      case (atype, Not(q), p) => !matchesPostingExtra(atype, q, p)
      case (atype, Or(qs), p) => qs.exists(q => matchesPostingExtra(atype, q, p))
      case (atype, And(qs), p) => qs.forall(q => matchesPostingExtra(atype, q, p))
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
