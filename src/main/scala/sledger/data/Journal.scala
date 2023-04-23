package sledger.data

import sledger.data.Amounts.{AmountStyle, CommoditySymbol}
import sledger.data.Transactions.Transaction

import java.time.Year


object Journals {

  case class Journal(
//                      inferredcommodities: Map[CommoditySymbol, AmountStyle], // commodities and formats inferred from journal amounts
//                      files: List[(String, String)],
//                      parsedefaultyear: Option[Year],
                      transactions: List[Transaction],
//                      finalcommentlines: String
                    )


  val nulljournal = Journal(
//    inferredcommodities = Map.empty,
//    files = List.empty,
//    parsedefaultyear = None,
    transactions = List.empty
//    finalcommentlines = ""
  )
}