import types._

/**
  * Created by altair on 02.05.16.
  */


class TridiagonalMatrixAlgorithm2D(L: Int, M:Int,
                                   a1: CoefficientFunction1,
                                   b1: CoefficientFunction1,
                                   c1: CoefficientFunction1,
                                   d1: CoefficientFunction1,
                                   a2: CoefficientFunction1,
                                   b2: CoefficientFunction1,
                                   c2: CoefficientFunction1,
                                   d2: CoefficientFunction1,
                                   lBorder: NodeFunction1,
                                   rBorder: NodeFunction1,
                                   tBorder: NodeFunction1,
                                   bBorder: NodeFunction1) {

    val eps = 1e-8

    private def calcCoefficients(L: Int,
                                 a: IntToDouble,
                                 b: IntToDouble,
                                 c: IntToDouble,
                                 d: IntToDouble,
                                 U0: Double): Array[Array[Double]] = {
        val coefficients = Array.ofDim[Double](L, 2)
        coefficients(1)(0) = -a(1) / b(1)
        coefficients(1)(1) = (d(1) - c(1) * U0) / b(1)
        for(i <- 2 until L) {
            coefficients(i)(0) = - a(i) / (b(i) + c(i) * coefficients(i - 1)(0))
            coefficients(i)(1) = (d(i) - c(i) * coefficients(i - 1)(1)) / (b(i) + c(i) * coefficients(i - 1)(0))
        }
        coefficients
    }

    private def calcCoefficients1(L: Int,
                                  a: IntToDouble,
                                  b: IntToDouble,
                                  c: IntToDouble,
                                  d: IntToDouble,
                                  UL: Double): Array[Array[Double]] = {
        val coefficients = Array.ofDim[Double](L, 2)
        coefficients(L - 1)(0) = -c(L - 1) / b(L - 1)
        coefficients(L - 1)(1) = (d(L - 1) - c(L - 1) * UL) / b( L - 1)
        for(i <- L - 2 to 1 by -1) {
            coefficients(i)(0) = - c(i) / (b(i) + a(i) * coefficients(i + 1)(0))
            coefficients(i)(1) = (d(i) - a(i) * coefficients(i + 1)(1)) / (b(i) + a(i) * coefficients(i + 1)(0))
        }
        coefficients
    }

    def getLayerWithBorderCondition(n: Int): CompulationGrid.Layer = {
        val layer = new CompulationGrid.Layer(L, M)
        for(m <- 0 to M) {
            layer.set(0, m, lBorder(n, 0, m))
            layer.set(L, m, rBorder(n, L, m))
        }
        for(l <- 1 until L) {
            layer.set(l, 0, bBorder(n, l, 0))
            layer.set(l, M, tBorder(n, l, M))
        }
        layer
    }

    def check(layerk_1: CompulationGrid.Layer, layerk: CompulationGrid.Layer): Boolean = {
        var max = math.abs((layerk_1(1, 1) - layerk(1, 1)) / layerk_1(1, 1))
        for(l <- 0 to L; m <- 0 to M) {
            val v = math.abs((layerk_1(l, m) - layerk(l, m)) / layerk_1(l, m))
            if(v > max)
                max = v
            if(max > eps) {
                return false
            }
        }
        if(max > eps)
            false
        else
            true
    }

    def solve(n:Int, alayer: CompulationGrid.Layer) = {
        val bufLayer = getLayerWithBorderCondition(n)
        var layer = alayer
        var oldlayer: CompulationGrid.Layer = null
        do {
            oldlayer = layer
            layer = alg1(n, bufLayer, alayer, oldlayer)
            //val layer2 = alg2(n, bufLayer2, alayer, oldlayer)
            //val layer3 = alg3(n, bufLayer, alayer, oldlayer)
            //val layer4 = alg4(n, bufLayer, alayer, oldlayer)

            //layer average(layer2, layer2, layer)
        } while(!check(layer, oldlayer))
        layer
    }


    private def alg1(n: Int, bufLayer: CompulationGrid.Layer, alayer: CompulationGrid.Layer, oldlayer: CompulationGrid.Layer): CompulationGrid.Layer = {
        for(m <- 1 until M) {
            val coefficients = calcCoefficients1(L,
                a1(m, alayer.apply, oldlayer.apply),
                b1(m, alayer.apply, oldlayer.apply),
                c1(m, alayer.apply, oldlayer.apply),
                d1(m, alayer.apply, oldlayer.apply),
                bufLayer(L, m)
            )
            for (l <- 1 until L) {
                bufLayer.set(l, m, coefficients(l)(0) * bufLayer(l - 1, m) + coefficients(l)(1))
            }
        }
        val layer = getLayerWithBorderCondition(n)
        for(l <- 1 until L) {
            val coefficients = calcCoefficients1(M,
                a2(l, oldlayer.apply, bufLayer.apply),
                b2(l, oldlayer.apply, bufLayer.apply),
                c2(l, oldlayer.apply, bufLayer.apply),
                d2(l, oldlayer.apply, bufLayer.apply),
                layer(l, M)
            )
            for (m <- 1 until M) {
                layer.set(l, m, coefficients(m)(0) * layer(l, m - 1) + coefficients(m)(1))
            }
        }
        layer
    }

    private def alg2(n: Int, bufLayer: CompulationGrid.Layer, alayer: CompulationGrid.Layer, oldlayer: CompulationGrid.Layer): CompulationGrid.Layer = {
        for(m <- 1 until M) {
            val coefficients = calcCoefficients(L,
                a1(m, alayer.apply, oldlayer.apply),
                b1(m, alayer.apply, oldlayer.apply),
                c1(m, alayer.apply, oldlayer.apply),
                d1(m, alayer.apply, oldlayer.apply),
                bufLayer(0, m)
            )
            for (l <- L - 1 to 1 by - 1) {
                bufLayer.set(l, m, coefficients(l)(0) * bufLayer(l + 1, m) + coefficients(l)(1))
            }
        }
        val layer = getLayerWithBorderCondition(n)
        for(l <- 1 until L) {
            val coefficients = calcCoefficients1(M,
                a2(l, oldlayer.apply, bufLayer.apply),
                b2(l, oldlayer.apply, bufLayer.apply),
                c2(l, oldlayer.apply, bufLayer.apply),
                d2(l, oldlayer.apply, bufLayer.apply),
                layer(l, M)
            )
            for (m <- 1 until M) {
                layer.set(l, m, coefficients(m)(0) * layer(l, m - 1) + coefficients(m)(1))
            }
        }
        layer
    }

    private def alg3(n: Int, bufLayer: CompulationGrid.Layer, alayer: CompulationGrid.Layer, oldlayer: CompulationGrid.Layer): CompulationGrid.Layer = {
        for(m <- 1 until M) {
            val coefficients = calcCoefficients1(L,
                a1(m, alayer.apply, oldlayer.apply),
                b1(m, alayer.apply, oldlayer.apply),
                c1(m, alayer.apply, oldlayer.apply),
                d1(m, alayer.apply, oldlayer.apply),
                bufLayer(L, m)
            )
            for (l <- 1 until L) {
                bufLayer.set(l, m, coefficients(l)(0) * bufLayer(l - 1, m) + coefficients(l)(1))
            }
        }
        val layer = getLayerWithBorderCondition(n)
        for(l <- 1 until L) {
            val coefficients = calcCoefficients(M,
                a2(l, oldlayer.apply, bufLayer.apply),
                b2(l, oldlayer.apply, bufLayer.apply),
                c2(l, oldlayer.apply, bufLayer.apply),
                d2(l, oldlayer.apply, bufLayer.apply),
                layer(l, 0)
            )
            for (m <- M - 1 to 1 by - 1) {
                layer.set(l, m, coefficients(m)(0) * layer(l, m + 1) + coefficients(m)(1))
            }
        }
        layer
    }

    private def alg4(n: Int, bufLayer: CompulationGrid.Layer, alayer: CompulationGrid.Layer, oldlayer: CompulationGrid.Layer): CompulationGrid.Layer = {
        for(m <- 1 until M) {
            val coefficients = calcCoefficients(L,
                a1(m, alayer.apply, oldlayer.apply),
                b1(m, alayer.apply, oldlayer.apply),
                c1(m, alayer.apply, oldlayer.apply),
                d1(m, alayer.apply, oldlayer.apply),
                bufLayer(0, m)
            )
            for (l <- L - 1 to 1 by -1) {
                bufLayer.set(l, m, coefficients(l)(0) * bufLayer(l + 1, m) + coefficients(l)(1))
            }
        }
        val layer = getLayerWithBorderCondition(n)
        for(l <- 1 until L) {
            val coefficients = calcCoefficients(M,
                a2(l, oldlayer.apply, bufLayer.apply),
                b2(l, oldlayer.apply, bufLayer.apply),
                c2(l, oldlayer.apply, bufLayer.apply),
                d2(l, oldlayer.apply, bufLayer.apply),
                layer(l, 0)
            )
            for (m <- M - 1 to 1 by - 1) {
                layer.set(l, m, coefficients(m)(0) * layer(l, m + 1) + coefficients(m)(1))
            }
        }
        layer
    }
}
