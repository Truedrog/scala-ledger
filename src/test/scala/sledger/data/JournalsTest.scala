package sledger.data

import munit.FunSuite
import sledger.data.Dates.{DateSpan, Exact}
import sledger.data.Journals.{journalDateSpan, nulljournal}
import sledger.data.Postings.posting
import sledger.data.Transactions.nulltransaction

import java.time.LocalDate

class JournalsTest extends FunSuite {

  test("journalDateSpan") {
    
    val a = journalDateSpan(true, nulljournal.copy(
      transactions = List(
        nulltransaction.copy(date = LocalDate.of(2014,2,1),
          postings = List(
            posting.copy(date1 = Some(LocalDate.of(2014,1, 10)))
          )
        ),
        nulltransaction.copy(date = LocalDate.of(2014, 9, 1),
          postings = List(
            posting.copy(date2 = Some(LocalDate.of(2014, 10, 10)))
          ) 
        )
      ) 
    ))
    val expected = DateSpan(Some(Exact(LocalDate.of(2014, 1, 10))),
      Some(Exact(LocalDate.of(2014, 10,11)))
    )
    assertEquals(a, expected)
  }
}
