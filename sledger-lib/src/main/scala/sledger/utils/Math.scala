package sledger.utils

object Math {
  def numDigits(n:Int): Int = if (n==0) 1 else math.log10(math.abs(n)).toInt + 1;
}
