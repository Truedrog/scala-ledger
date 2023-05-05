package sledger.data

import munit.CatsEffectSuite
import sledger.Types.Unmarked
import sledger.data.Balancing.{balanceTransaction, defBalancingOptions, isTransactionBalanced, transactionInferBalancingAmount}
import sledger.data.Transactions._
import sledger.data.Amounts._
import sledger.data.Postings.{nullsourcepos, post, posting}

import java.time.LocalDate

class BalancingTest extends CatsEffectSuite {
  test("transactionInferBalancingAmount") {
    var actual = transactionInferBalancingAmount(Map.empty, nulltransaction).map(a => a._1)
    var expected = Right(nulltransaction)
    assertEquals(actual, expected)
    
    expected = Right(nulltransaction.copy(postings = List(
      post("a", usd(-5)),
      post("b", usd(5))
    )))
    actual = transactionInferBalancingAmount(Map.empty, nulltransaction.copy(postings = List(
      post("a", usd(-5)),
      post("b", missingamt)
    ))).map(a => a._1)
    assertEquals(actual, expected)

    actual = transactionInferBalancingAmount(Map.empty, nulltransaction.copy(postings = List(
      post("a", usd(-5)),
      post("b", usd(4)),
      post("c", missingamt),
    ))).map(a => a._1)
    assertEquals(actual, Right(nulltransaction.copy(postings = List(
      post("a", usd(-5)),
      post("b", usd(4)),
      post("c", usd(1)),
    ))))
  }
  test("balanceTransaction") {
    var assertLeft = balanceTransaction(defBalancingOptions,
      Transaction(
        0,
        "",
        nullsourcepos,
        LocalDate.of(2023, 1, 1),
        None,
        Unmarked,
        "",
        "test",
        "",
        List(posting.copy(account = "a", amount = mixedAmount(usd(1))), posting.copy(account = "b", amount = mixedAmount(usd(1))))
      )
    )
    assert(clue(assertLeft.isLeft), "detect unbalanced entry, sign error")
    
    assertLeft = balanceTransaction(defBalancingOptions,
      Transaction(
        0,
        "",
        nullsourcepos,
        LocalDate.of(2023, 1, 1),
        None,
        Unmarked,
        "",
        "test",
        "",
        List(
          posting.copy(account = "a", amount = missingMixedAmt), 
          posting.copy(account = "b", amount = missingMixedAmt)
        )
      )
    )
    assert(clue(assertLeft.isLeft), "detect unbalanced entry, multiple missing amounts")
    
    var actual = balanceTransaction(defBalancingOptions,
      Transaction(
        0,
        "",
        nullsourcepos,
        LocalDate.of(2023, 1, 1),
        None,
        Unmarked,
        "",
        "test",
        "",
        List(
          posting.copy(account = "a", amount = mixedAmount(usd(1))),
          posting.copy(account = "b", amount = missingMixedAmt)
        )
      )
    ).map(_.postings.last.amount)
    var expected = Right(mixedAmount(usd(-1)))
    println(actual)
    assertEquals(actual, expected)
  }
  test("isTransactionBalanced") {
    val balanced = isTransactionBalanced(defBalancingOptions,
      Transaction(
        0,
        "",
        nullsourcepos,
        LocalDate.of(2023, 1, 1),
        None,
        Unmarked,
        "",
        "test",
        "",
        List(
          posting.copy(account = "b", amount = mixedAmount(usd(1))), 
          posting.copy(account = "c", amount = mixedAmount(usd(-1)))
        )
      )
    )
    assert(balanced, "detect balanced")
    
    var unbalanced = !isTransactionBalanced(defBalancingOptions,
      Transaction(
        0,
        "",
        nullsourcepos,
        LocalDate.of(2023, 1, 1),
        None,
        Unmarked,
        "",
        "test",
        "",
        List(
          posting.copy(account = "b", amount = mixedAmount(usd(1))),
          posting.copy(account = "c", amount = mixedAmount(usd(-1.01)))
        )
      )
    )
    assert(unbalanced, "detect unbalanced")

    unbalanced = !isTransactionBalanced(defBalancingOptions,
      Transaction(
        0,
        "",
        nullsourcepos,
        LocalDate.of(2023, 1, 1),
        None,
        Unmarked,
        "",
        "test",
        "",
        List(
          posting.copy(account = "b", amount = mixedAmount(usd(1))),
        )
      )
    )
    assert(unbalanced, "detect unbalanced, one posting")
  }
}
