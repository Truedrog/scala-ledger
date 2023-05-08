package sledger.data

sealed trait StringFormat
case class OneLine(components: List[StringFormatComponent]) extends StringFormat
case class TopAligned(components: List[StringFormatComponent]) extends StringFormat
case class BottomAligned(components: List[StringFormatComponent]) extends StringFormat

sealed trait ReportItemField
case object AccountField extends ReportItemField
case object DefaultDateField extends ReportItemField
case object DescriptionField extends ReportItemField
case object TotalField extends ReportItemField
case object DepthSpacerField extends ReportItemField
case class FieldNo(n: Int) extends ReportItemField

sealed trait StringFormatComponent
case class FormatLiteral(text: String) extends StringFormatComponent
case class FormatField(leftJustify: Boolean, minWidth: Option[Int], maxWidth: Option[Int], field: ReportItemField) extends StringFormatComponent
object StringFormat {
  def defaultBalanceLineFormat: StringFormat = BottomAligned(List(
    FormatField(false, Some(20), None, TotalField),
    FormatLiteral("  "),
    FormatField(true, Some(2), None, DepthSpacerField),
    FormatField(true, None, None, AccountField)
  ))
}
