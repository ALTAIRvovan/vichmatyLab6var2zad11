/**
  * Created by altair on 13.04.16.
  */
class VariableTask(K: Double => Double, Q: Double => Double, F: Double => Double, N: Int)
    extends DifferencialModelTask(K, Q, F, N) {

    override def c(l: Int): Double = k1((l - 0.5) * h)

    override def b(l: Int): Double = -(a(l) + c(l) + q1(l * h) * h * h)

    override def a(l: Int): Double = k1((l + 0.5) * h)

    def q1(x:Double): Double = Q(x)

    def k1(x: Double): Double = K(x)
}
