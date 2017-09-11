package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    Math.pow(b.apply(), 2) - 4*a.apply()*c.apply()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    Set ((-b.apply().toDouble - Math.sqrt(computeDelta(a, b, c).apply()))/(2*a.apply()), (-b.apply().toDouble + Math.sqrt(computeDelta(a, b, c).apply()))/(2*a.apply()))
  }
}
