/**
  * Created by altair on 12.04.16.
  */
class ModelTask(K : Double => Double, Q : Double => Double, F : Double => Double, Mu : Double, Nu : Double) extends AbstractTask(K, Q, F ) {

    def q:Double = Q(0)
    def k:Double = K(0)
    def f(x:Double):Double = F(x)

    override def solution: Array[Double] = {
        def func(x : Double) = Mu / q + Nu / (4 * Math.PI * Math.PI * k + q) * F(x)

        val res = new Array[Double](11)
        for(i <- 0 until 11) {
            res(i) = func(i * 0.1)
        }
        return res
    }
}
