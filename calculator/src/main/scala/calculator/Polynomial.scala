package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b()
      bVal * bVal - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val deltaVal = delta()
      val deltaSqrtVal = math.sqrt(deltaVal)
      val twiceA = 2 * a()
      val negB = -b()
      if (deltaVal >= 0) Set(
        (negB + deltaSqrtVal) / twiceA,
        (negB - deltaSqrtVal) / twiceA
      )
      else Set()
    }
  }
}
