package sledger.data

import cats.syntax.all._
import sledger.data.AccountNames.{AccountName, expandAccountName}
import sledger.data.Accounts.FastTree.accountTree
import sledger.data.Amounts._
import sledger.data.Journals.Journal
import sledger.data.Postings.Posting
import sledger.{NormalSign, NormallyNegative, NormallyPositive}

import scala.collection.immutable.{HashMap, HashSet}

object Accounts {

  case class Account(
                      name: AccountName,
                      subs: List[Account],
                      parent: Option[Account],
                      boring: Boolean,
                      numposting: Int,
                      ebalance: MixedAmount,
                      ibalance: MixedAmount
                    ) {


    override def toString: AccountName = {
      val name = this.name
      val boring = if(this.boring) "y" else "n"
      val numposting = this.numposting
      val ebalance = showMixedAmountB(noColour, this.ebalance).builder.result()
      val ibalance = showMixedAmountB(noColour, this.ibalance).builder.result()
      f"Account ${name}%s (boring:${boring}%s, postings:${numposting}%d, ebalance:${ebalance}%s, ibalance:${ibalance}%s)\n"
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case a: Account => a.name == this.name
      }
    }
  }

  def showAccountDebug(a: Account) = {
    val name = a.name
    val boring = if (a.boring) "b" else " "
    val ebalance = showMixedAmountB(noColour, a.ebalance).builder.result()
    val ibalance = showMixedAmountB(noColour, a.ibalance).builder.result()
    f"${name}%-25s ${ebalance}%4s ${ibalance}%4s ${boring}%s"
  }
  
  def showAccounts(a: Account): String = {
    "\n" + flattenAccounts(a).map(showAccountDebug).mkString("","\n", "\n")
  }
    
  val nullact: Account = Account(
    name = "",
    subs = List.empty,
    parent = None,
    numposting = 0,
    boring = false,
    ebalance = nullMixedAmount,
    ibalance = nullMixedAmount
  )

  def mapAccounts(f: Account => Account)(a: Account): Account =
    f(a.copy(subs = a.subs.map(mapAccounts(f))))

  def sortAccountTreeByAmount(normalSign: NormalSign, account: Account): Account = {
    val sortSubs: List[Account] => List[Account] = xs => normalSign match {
      case NormallyPositive => xs.sortWith((a, b) => (a.ibalance, a.name) > (b.ibalance, b.name))
      case NormallyNegative => xs.sortWith((a, b) => (a.ibalance, a.name) < (b.ibalance, b.name))
    }
    mapAccounts(a => a.copy(subs = sortSubs(a.subs)))(account)
  }

  def sortAccountTreeByDeclaration(account: Account): Account = {
    if (account.subs.isEmpty) {
      account
    } else {
      account.copy(subs = account.subs.map(sortAccountTreeByDeclaration))
    }
  }


  def sortAccountNamesByDeclaration(journal: Journal, keepParents: Boolean, as: List[AccountName]): List[AccountName] = {
    def filter(as: List[AccountName]): List[AccountName] =
      if (keepParents) identity(as) else {
        as.filter(k => HashSet.from(as).contains(k))
      }
     
    filter(flattenAccounts(sortAccountTreeByDeclaration(accountTree("root", as))).drop(1).map(_.name))
  }

  def accountsFromPostings(ps: List[Posting]): List[Account] = {
    def addAndIncrement(a: (Int, MixedAmount), b: (Int, MixedAmount)): (Int, MixedAmount) = {
      val (n, a1) = a
      val (m, b1) = b
      (n + m, maPlus(a1, b1))
    }
    val summed = ps.foldRight(HashMap.empty[AccountName, (Int, MixedAmount)])((p, acc) =>
      acc.updatedWith(p.account) {
        case Some(value) => Some(addAndIncrement(value, (1, p.amount)))
        case None => Some((1, p.amount))
      }
    )

    def setnumpsebalance(a: Account): Account = {
      val (numps, total) = summed.getOrElse(a.name, (0, nullMixedAmount))
      a.copy(numposting = numps, ebalance = total)
    }

    val acctstree = accountTree("root", summed.keys.toList)
    val acctswithebals = mapAccounts(setnumpsebalance)(acctstree)
    
    val acctswithibals = sumAccounts(acctswithebals)
    
    val acctswithparents = tieAccountParents(acctswithibals)
    flattenAccounts(acctswithparents)
  }

  def sumAccounts(account: Account): Account = {
    if (account.subs.isEmpty) {
      account.copy(ibalance = account.ebalance)
    } else {
      val subs = account.subs.map(sumAccounts)
      val ibal = maSum(account.ebalance +: subs.map(_.ibalance))
      account.copy(
        ibalance = ibal,
        subs = subs
      )
    }
  }

  def tieAccountParents(a: Account): Account = {
    def tie(parent: Option[Account], a: Account): Account = {
      val subs = a.subs.map(sub => tie(Some(a), sub))
      a.copy(parent = parent, subs = subs)
    }

    tie(None, a)
  }

  def flattenAccounts(a: Account): List[Account] = {
    def squish(a: Account, as: List[Account]): List[Account] = {
      val subs = a.subs.foldRight(as)((sub, acc) => squish(sub, acc))
      a :: subs
    }

    squish(a, Nil)
  }
  
  case class FastTree[A](treeMap: Map[A, FastTree[A]])

  object FastTree {

    def accountTree(rootname: AccountName, as: List[AccountName]): Account = {
      val fastTree = treeFromPaths(as.map(expandAccountName))
      nullact.copy(
        name = rootname,
        subs = fastTree.treeMap.toList.sortBy(_._1).map { case (k, v) => accountTree1(k, FastTree(v.treeMap)) }
      )
    }

    private def accountTree1(a: AccountName, m: FastTree[AccountName]): Account = {
      nullact.copy(
        name = a,
        subs = m.treeMap.toList.sortBy(_._1).map { case (k, v) => accountTree1(k, FastTree(v.treeMap)) }
      )
    }

    def mergeTrees[A: Ordering](tree1: FastTree[A], tree2: FastTree[A]): FastTree[A] =
      FastTree(tree1.treeMap ++ tree2.treeMap.map { case (k, v) =>
        k -> tree1.treeMap.get(k).fold(v)(subTree1 => mergeTrees(subTree1, v))
      })
      
    def treeFromPath[A: Ordering](path: List[A]): FastTree[A] =
      path match {
        case Nil => FastTree[A](Map.empty)
        case x :: xs => FastTree(Map(x -> treeFromPath(xs)))
      }
    
    def treeFromPaths[A: Ordering](paths: List[List[A]]): FastTree[A] = {
      val emptyTree = FastTree(Map.empty[A, FastTree[A]])
      paths.foldLeft(emptyTree)((tree, path) => mergeTrees(tree, treeFromPath(path)))
    }
  }

}
