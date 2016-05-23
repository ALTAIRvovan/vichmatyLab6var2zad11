import CompulationGrid.Layer

/**
  * Created by altair on 02.05.16.
  */
class CompulationGrid(val L: Int, val M: Int, val N: Int) {
    val grid = new Array[Layer](N + 1)
    /*for(i <- 0 to N)
        grid(i) = new Layer(L + 1, M + 1)
     */

    def apply(l: Int, m: Int, n: Int):Double = {
        assert(n <= N, "n = " + n)
        assert(l <= L, "l = " + l)
        assert(m <= M, "m = " + M)
        //assert(grid(n) != null, "null n= " + n )
        if(grid(n) == null) {
            grid(n) = new Layer(L, M)
        }
        grid(n)(l, m)
    }
    def apply(n: Int): Layer = grid(n)
    def set(n: Int, layer: Layer) = {
        if(N > 1000) {
            clearOld(n)
        }
        grid(n) = layer
    }

    def set(n: Int, l: Int, m: Int, value: Double) = {
        if(grid(n) == null) {
            grid(n) = new Layer(L, M)
        }
        grid(n).set(l, m, value)
    }

    def clearOld(n : Int): Unit = {
        if( n > 3) {
            var n1:Int = n - 2
            while(n1 > 0 && grid(n1) != null) {
                grid(n1) = null
                n1 -= 1
            }
        }
    }
}

object CompulationGrid {
    class Layer(val L:Int, val M: Int) {
        val grid = Array.ofDim[Double](L + 1, M + 1)
        var hr:Double = 0
        var ht: Double = 0
        var tau: Double = 0

        def apply(l: Int, m: Int): Double = {
            assert(l <= L, "l = " + l)
            assert(m <= M, "m = " + M)
            grid(l)(m)
        }

        def set(l: Int, m: Int, value: Double) = {
            grid(l)(m) = value
        }

        def setSteps(hr1: Double, ht1: Double, tau1: Double) = {
            hr = hr1
            ht = ht1
            tau = tau1
        }

        override def toString: String = {
            if(hr > 0 && ht > 0) {
                someToString
            } else
                allToString
        }

        def allToString: String = {
            var buf = new StringBuilder()
            if(hr > 0) {
                buf ++= f"           "
                for(j <- 0 to L) {
                    buf ++= f"${hr * j}%4.8f "
                }
                buf ++= "\n"
            }
            for(i <- 0 to M) {
                if(ht > 0) {
                    buf ++= f"${ht * i}%4.8f "
                }
                for(j <- 0 to L) {
                    buf ++= f"${apply(j, i)}%4.8f "
                }
                buf ++= "\n"
            }
            buf.toString
        }

        private def isRoundL(l: Int, h: Double): Boolean = {
            val r = math.round(l * h * 10)
            if(math.abs(r - l * h * 10) < h / 2 && r % 2 == 0)
                true
            else
                false
        }

        private def isRoundM(m: Int, h: Double): Boolean = {
            for(i <- 0 to 5) {
                if(Math.abs(i * Math.PI / 10 - m * h) < h / 2)
                    return true
            }
            false
        }

        def someToString: String = {
            require(hr > 0)
            require(ht > 0)
            var buf = new StringBuilder()
            if(hr > 0) {
                buf ++= f"           "
                for(j <- 0 to L if isRoundL(j, hr)) {
                    buf ++= f"${hr * j}%4.8f "
                }
                buf ++= "\n"
            }
            for(i <- 0 to M if isRoundM(i, ht)) {
                buf ++= f"${ht * i}%4.8f "
                for(j <- 0 to L if isRoundL(j, hr)) {
                    buf ++= f"${apply(j, i)}%4.8f "
                }
                buf ++= "\n"
            }
            buf.toString
        }

        def delta(layer: Layer): Double = {
            require(hr > 0 && ht > 0)
            var max = math.abs(grid(0)(0) - layer(0, 0))
            for(i <- 0 to L if isRoundL(i, hr); j <- 0 to M if isRoundM(j, ht)) {
                val v = math.abs(grid(i)(j) - layer(i, j))
                if(v > max)
                    max = v
            }
            max
        }

        def subAbs(layer: Layer): Layer = {
            require(hr > 0 && ht > 0)
            val res = new Layer(L, M)
            for(i <- 0 to L; j <- 0 to M) {
                res.set(i, j, math.abs(apply(i, j) - layer(i, j)))
            }
            res.setSteps(hr, ht, tau)
            res
        }

        def average(layer: Layer): Unit = {
            require(L == layer.L && M == layer.M)
            for(l <- 0 to L; m <- 0 to M) {
                set(l, m, (apply(l, m) + layer(l, m)) / 2)
            }
        }

        def average(layer: Layer, layer2: Layer, layer3: Layer): Unit = {
            require(L == layer.L  && M == layer.M )
            require(L == layer2.L && M == layer2.M)
            require(L == layer3.L && M == layer3.M)
            for(l <- 0 to L; m <- 0 to M) {
                set(l, m, (apply(l, m) + layer(l, m) + layer2(l, m) + layer3(l, m)) / 4)
            }
        }
    }
}