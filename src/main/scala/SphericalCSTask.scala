import types.NodeFunction1

/**
  * Created by altair on 02.05.16.
  */
class SphericalCSTask(val Mu: Double, val Ct: Double, val L: Int, val M: Int, val N: Int, val delta: Double = 1e-7){
    val hr:Double = 1.0 / L
    val ht:Double = Math.PI / M / 2
    val tau:Double = math.min(1.0, Ct * Mu / (2 * (Mu + 2))) / N// / N

    def rl(l:Double):Double = hr * l
    def tetam(m: Double):Double = ht * m

    def a1(m: Int, Un: (Int, Int) => Double, Uk: (Int, Int) => Double)(l: Int): Double = {
        -rl(l + 0.5) * rl(l + 0.5) * (math.pow(Uk(l + 1, m), Mu) + math.pow(Uk(l, m), Mu)) * tau / (2 * rl(l) * rl(l) * hr * hr)
    }

    def c1(m: Int, Un: (Int, Int) => Double, Uk: (Int, Int) => Double)(l: Int): Double = {
        -rl(l - 0.5) * rl(l - 0.5) * (math.pow(Uk(l, m), Mu) + math.pow(Uk(l - 1, m), Mu)) * tau / (2 * rl(l) * rl(l) * hr * hr)
    }

    def b1(m: Int, Un: (Int, Int) => Double, Uk: (Int, Int) => Double)(l: Int): Double = {
        1 - a1(m, Un, Uk)(l) - c1(m, Un, Uk)(l)
    }

    def d1(m: Int, Un: (Int, Int) => Double, Uk: (Int, Int) => Double)(l: Int): Double = Un(l, m)

    def a2(l: Int, Uk: (Int, Int) => Double, Ut: (Int, Int) => Double)(m: Int): Double = {
        - math.sin(tetam(m + 1/2)) * (math.pow(Uk(l, m + 1), Mu) + math.pow(Uk(l, m), Mu)) * tau / (2 * rl(l) * rl(l) * math.sin(tetam(m)) * ht * ht)
    }

    def c2(l: Int, Uk: (Int, Int) => Double, Ut: (Int, Int) => Double)(m: Int): Double = {
        - math.sin(tetam(m - 1/2)) * (math.pow(Uk(l, m), Mu) + math.pow(Uk(l, m - 1), Mu)) * tau / (2 * rl(l) * rl(l) * math.sin(tetam(m)) * ht * ht)
    }

    def b2(l: Int, Uk: (Int, Int) => Double, Ut: (Int, Int) => Double)(m: Int): Double = {
        1 - a2(l, Uk, Ut)(m) - c2(l, Uk, Ut)(m)
    }

    def d2(l: Int, Uk: (Int, Int) => Double, Ut: (Int, Int) => Double)(m: Int): Double = Ut(l, m)

    def lBorder(n: Int, l: Int, m: Int):Double = 0
    def rBorder(n: Int, l: Int, m: Int):Double = {
        math.pow(math.cos(tetam(m)), 2.0 / Mu) * math.pow(Ct - 2 * (Mu + 2) * tau * n / Mu, -1.0 / Mu)
    }

    def bBorder(n: Int, l: Int, m: Int): Double = {
        math.pow(rl(l), 2.0 / Mu) * math.pow(Ct - 2.0 * (Mu + 2) * tau * n / Mu, -1.0 / Mu)
    }

    def tBorder(n: Int, l: Int, m: Int): Double = 0

    def solve(): CompulationGrid = {
        val grid = new CompulationGrid(L, M, N)
        val layer0 = new CompulationGrid.Layer(L, M)
        for(l <- 0 to L; m <- 0 to M)
            layer0.set( l, m,
                math.pow(rl(l) * math.cos(tetam(m)), 2.0 / Mu) * math.pow(Ct, -1.0 / Mu)
            )
        grid.set(0, layer0)
        grid(0).setSteps(hr, ht, tau)
        val tridiagonal = new TridiagonalMatrixAlgorithm2D(L, M, a1, b1, c1, d1, a2, b2, c2, d2, lBorder, rBorder, tBorder, bBorder)
        for(n <- 0 until N) {
            grid.set(n + 1, tridiagonal solve(n + 1, grid(n)))
            grid(n + 1).setSteps(hr, ht, tau)
        }
        grid
    }

    def model(): CompulationGrid = {
        val grid = new CompulationGrid(L, M, N)
        for( n <- 0 to N; l <- 0 to L; m <- 0 to M) {
            grid.set(n, l, m, math.pow(math.pow(rl(l) * math.cos(tetam(m)), 2) / (Ct - 2 * (Mu + 2) * tau * n / Mu), 1.0 / Mu))
            grid(n).setSteps(hr, ht, tau)
        }
        grid
    }
}
