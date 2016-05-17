/**
  * Created by altair on 12.04.16.
  */
abstract class AbstractTask(K: (Double) => Double,
                            Q: (Double) => Double,
                            F: (Double) => Double) {
    def solution: Array[Double]

}
