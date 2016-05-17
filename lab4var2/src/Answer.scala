/**
  * Created by altair on 12.04.16.
  */
class Answer(name:String) {
    var col1: Array[Double] = null
    var col2: Array[Double] = null

    def print = {
        println("Answer" + name)
        var max = math.abs(col2(0) - col1(0))
        for(i <- col1.indices) {
            printf("(%3.2f , %+7.8f , %+7.8f , %e)\n", i * 0.1, col1(i), col2(i), col2(i) - col1(i))
            val dx = math.abs(col2(i) - col1(i))
            if(max < dx)
                max = dx
        }
        println("max delta = " + max)
        println("END;")
    }
}
