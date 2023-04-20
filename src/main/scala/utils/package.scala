package object utils {
  def lastDef[A](default: A, xs: Seq[A]): A =
    xs.lastOption.getOrElse(default)

  def maximumBound[A : Ordering](x: A)(xs: List[A]): A = {
    val ev = implicitly[Ordering[A]]
    (x :: xs).max(ev)
  }
}
