/*
  Calculate number of correct bracket structures of length 2*N.
  w(1) = 1, i.e. ()
  w(2) = 2, i.e. ()w1, (w1) => ()(), (())
  w(3) = 5, i.e. w1(w2), (w2)w1, (w1w2), (w2w1), w1(w2)

  https://en.wikipedia.org/wiki/Catalan_number
*/

object CatalanNumbers extends App {

  def catalan(n: Int): Int =
    if (n <= 1) 1
    else (1 until n).map(k => catalan(k) * catalan(n - k)).sum

  // test
  for (i <- 0 to 10) {
    println(i, catalan(i + 1))
  }
}
