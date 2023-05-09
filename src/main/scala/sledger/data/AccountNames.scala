package sledger.data
import cats._
import cats.data.NonEmptyList
import cats.derived
import cats.derived.auto
import sledger.data.AccountNames.AccountName
import sledger.utils.RoseTree._

import scala.util.matching.Regex
object AccountNames {
  type AccountName = String

  sealed trait AccountType

  object AccountType {
    case object Asset extends AccountType

    case object Liability extends AccountType

    case object Equity extends AccountType

    case object Revenue extends AccountType

    case object Expense extends AccountType

    case object Cash extends AccountType

    case object Conversion extends AccountType

    implicit val accountTypeEq: Eq[AccountType] = Eq.fromUniversalEquals
    implicit val accountTypeOrder: auto.order.type = derived.auto.order
    implicit val accountTypeShow: Show[AccountType] = new Show[AccountType]{
      override def show(t: AccountType): String = t match {
        case Asset      => "A"
        case Liability  => "L"
        case Equity     => "E"
        case Revenue    => "R"
        case Expense    => "X"
        case Cash       => "C"
        case Conversion => "V"
      }
    }
  }
  
  val acctSepChar: Char = ':'

  val acctsep: String = acctSepChar.toString
  val accountNameComponents: AccountName => List[AccountName] = 
    accname => accname.split(acctsep).toList
  val accountNameFromComponents: List[AccountName] => AccountName = 
    components => Foldable[List].intercalate(components, acctsep)
  val accountLeafName: AccountName => String = 
    accountName => accountNameComponents(accountName).last
    
  val accountSummarisedName: AccountName => String = (name: AccountName) => {
    val cs = accountNameComponents(name)
    val a = accountLeafName(name)
    if (cs.length > 1) {
      (Foldable[List].intercalate(cs.init.map(_.take(2)), ":") :+ a).mkString(":")
    } else {
      a
    }
  }
  
  val assetsAccount: Regex     = "^assets?(:|$)".r
  val cashAccount: Regex       = "^assets?(:.+)?:(cash|bank|che(ck|que?)(ing)?|savings?|current)(:|$)".r
  val liabilityAccount: Regex  = "^(debts?|liabilit(y|ies))(:|$)".r
  val equityAccount: Regex     = "^equity(:|$)".r
  val conversionAccount: Regex = "^equity:(trad(e|ing)|conversion)s?(:|$)".r
  val revenueAccount: Regex    = "^(income|revenue)s?(:|$)".r
  val expenseAccount: Regex    = "^expenses?(:|$)".r
  
  def accountNameInferType(a: String): Option[AccountType] = {
    // Match the account name against each account type regex
    if (cashAccount.pattern.matcher(a).matches) Some(AccountType.Cash)
    else if (assetsAccount.pattern.matcher(a).matches) Some(AccountType.Asset)
    else if (liabilityAccount.pattern.matcher(a).matches) Some(AccountType.Liability)
    else if (conversionAccount.pattern.matcher(a).matches) Some(AccountType.Conversion)
    else if (equityAccount.pattern.matcher(a).matches) Some(AccountType.Equity)
    else if (revenueAccount.pattern.matcher(a).matches) Some(AccountType.Revenue)
    else if (expenseAccount.pattern.matcher(a).matches) Some(AccountType.Expense)
    else None
  }
  
  def accountNameType(atypes: Map[AccountName,AccountType], a: AccountName): Option[AccountType] = {
    val anames = a+:parentAccountNames(a)
    Alternative[Option].combineAllK(anames.map(an => atypes.get(an))).orElse(accountNameInferType(a))  
  }
  
  def isBalanceSheetAccountType(t: AccountType): Boolean = t match {
    case AccountType.Asset | AccountType.Liability | AccountType.Equity | AccountType.Cash | AccountType.Conversion => true
    case _ => false
  }

  def isIncomeStatementAccountType(t: AccountType): Boolean = t match {
    case AccountType.Revenue | AccountType.Expense => true
    case _ => false
  }

  def isAccountSubtypeOf(t1: AccountType, t2: AccountType): Boolean = {
    import sledger.data.AccountNames.AccountType._
    (t1, t2) match {

      case (Asset, Asset) | (Liability, Liability) | (Equity, Equity) | (Revenue, Revenue) | (Expense, Expense)
           | (Cash, Cash) | (Cash, Asset) | (Conversion, Conversion) | (Conversion, Equity) => true
      case _ => false
    }
  }

  def accountNameLevel(a: String): Int = {
    if (a.isEmpty) 0
    else a.count(c => c == acctSepChar) + 1
  }
  
  def expandAccountNames(as: List[AccountName]): List[AccountName] = {
    Foldable[List].foldMap(as)(a => expandAccountName(a).toSet).toList
  }

  // Define a function to expand an account name
  def expandAccountName(a: AccountName): List[AccountName] = {
    NonEmptyList
      .fromListUnsafe(accountNameComponents(a).tails.toList)
      .tail
      .map(accountNameFromComponents)
  }
  
  def topAccountNames(as: List[AccountName]): List[AccountName] = {
    expandAccountNames(as).filter(a => accountNameLevel(a) == 1)
  }
  
  def parentAccountName(a: AccountName): AccountName = {
    accountNameFromComponents(accountNameComponents(a).init)
  }
  
  def parentAccountNames(a: AccountName): List[AccountName] = {
    def go(a: AccountName): List[AccountName] = a match {
      case "" => List.empty
      case a2 => a2 +: go(parentAccountName(a2))
    }
    go(parentAccountName(a))
  }
  
  def isAccountNamePrefixOf(a: AccountName, b: AccountName): Boolean = (a + acctsep).startsWith(b)

  def isSubAccountNameOf(s: AccountName, p: AccountName): Boolean = {
    isAccountNamePrefixOf(s, p) && accountNameLevel(s) == accountNameLevel(p) + 1
  }
  
  def subAccountNamesFrom(accs: List[AccountName], a: AccountName) = {
    accs.filter(a1 => isSubAccountNameOf(a, a1))
  }

  def accountNameTreeFrom(accs: List[AccountName]): Node[AccountName] = {
    def subs: AccountName => List[AccountName] = subAccountNamesFrom(expandAccountNames(accs), _)
    def go(as: List[AccountName]): List[Node[AccountName]] = {
      as match {
        case ::(head, next) => for {
          a <- as
        } yield Node(a, go(subs(a)))
        case Nil => List.empty
      }
    } 
    Node("root", go(topAccountNames(accs)))
  }
}
