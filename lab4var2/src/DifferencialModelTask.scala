import scala.collection.mutable

/**
  * Created by altair on 12.04.16.
  */
class DifferencialModelTask(K: Double => Double, Q: Double => Double, F: Double => Double, N: Int)
    extends ModelTask(K, Q, F, 0 , 0) {

    val h: Double = 1.0 / N

    val eps = 1e-8

    def a(l:Int = 0):Double = k
    def b(l:Int = 0):Double = -2 * k - q * h * h
    def c(l:Int = 0):Double = k
    def d1(l:Int = 0):Double = - f(l.toDouble * h) * h * h
    def d2(l:Int = 0):Double = 0

    def isRound(l: Int) : Boolean = {
        val x: Double = l.toDouble * h * 10.toDouble
        if(Math.abs(Math.round(x) - x) < h)
            true
        else
            false
    }

    def IS_ZERO(x: Double) : Boolean = math.abs(x) < eps

    private def doSweet(d: (Int) => Double, a0: Double, b0: Double, d0: Double, bL: Double, cL: Double, dL: Double): Tuple3[Array[Double], Double, Double] = {
        var alfa:Double = - a0 / b0
        var beta:Double =   d0 / b0
        var res = new mutable.MutableList[Double]
        for(l <- 1 until N) {
            beta = (d(l) - c(l) * beta) / (b(l) + c(l) * alfa)
            alfa = - a(l) / (b(l) + c(l) * alfa)
        }
        var u: Double = (dL - cL * beta) / (bL + cL * alfa)
        res += u
        u = alfa * u + beta
        if(isRound(N - 1)) {
            res += u
        }
        val U_N_1: Double = if( math.abs(u) < eps) 0 else u
        var U_1: Double = 0
        for(l <- N - 1 to 1 by -1) {
            alfa = - (a(l) + b(l) * alfa) / (c(l) * alfa)
            beta = - (beta * (b(l) + c(l) * alfa) - d(l)) / c(l)
            u = alfa * u + beta
            if(isRound(l - 1)) {
                res += u
            }
            if(l == 2) {
                U_1 = if( math.abs(u) < eps) 0 else u
            }
        }
        Tuple3(res.reverse.toArray, U_1, U_N_1)
    }

    override def solution: Array[Double] = {
        val V = doSweet(d1, 0, 1, 0, 1, 0, 0)
        val W = doSweet(d2, 0, 1, 1, 1, 0, 1)
        println("denominator: " + (a(0) + c(N) + q * h * h - a(0) * W._2 - c(N) * W._3))
        println("numenator: " + (a(0) * V._2 + c(N) * V._3 + f(0) * h * h ) )
        val u0:Double = (a(0) * V._2 + c(N) * V._3 - d1(0)) / (-b(0) - a(0) * W._2 - c(N) * W._3)
        println("U0:" + u0)
        val res = new Array[Double](11)
        for(i <- V._1.indices) {
            res(i) = V._1(i) + u0 * W._1(i)
        }
        res
    }
}
