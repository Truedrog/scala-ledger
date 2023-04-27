package sledger.data

import sledger.data.AccountNames.AccountName
import sledger.data.Amounts.{AmountStyle, CommoditySymbol}
import sledger.data.Transactions.Transaction

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, Year, ZoneId, ZonedDateTime}

object Journals {

  case class Journal(
                      parsedefaultyear: Option[Year],
                      parseparentaccounts: List[AccountName],
                      parseddefaultcommodity: Option[(CommoditySymbol, AmountStyle)],
                      //                      inferredcommodities: Map[CommoditySymbol, AmountStyle], // commodities and formats inferred from journal amounts
                      files: List[(String, String)],
                      //                      parsedefaultyear: Option[Year],
                      transactions: List[Transaction],
                      //                      finalcommentlines: String,
                      lastReadTime: LocalDateTime
                    ) { 

    def reverse = {
      this.copy(files = this.files.reverse, transactions = this.transactions.reverse)
    }
    
    def addTransaction(t: Transaction): Journal = {
      this.copy(transactions = t +: this.transactions)
    }
    
    def setLastReadTime(time: LocalDateTime): Journal = {
      this.copy(lastReadTime = time)
    }
    
    def addFiles(f:(String, String)): Journal =
      this.copy(files = this.files ++ List(f))
  }


  val nulljournal = Journal(
    parsedefaultyear = None,
    parseddefaultcommodity = None,
    //    inferredcommodities = Map.empty,
    files = List.empty,
    //    parsedefaultyear = None,
    transactions = List.empty,
    lastReadTime = LocalDateTime.MIN,
    parseparentaccounts = List.empty
    //    finalcommentlines = ""
  )
}