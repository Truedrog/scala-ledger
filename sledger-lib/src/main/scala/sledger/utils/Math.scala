package sledger.utils

object Math {
  /*
  Find the number of digits of an 'Int'.
   */
  def numDigits(n:Int): Int = if (n==0) 1 else math.log10(math.abs(n)).toInt + 1;
}
