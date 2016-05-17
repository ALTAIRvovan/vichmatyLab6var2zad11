import java.util.Scanner

/**
  * Created by altair on 12.04.16.
  */
object MainObject {

    def k(x:Double):Double = 1
    def q(x:Double):Double = 1
    val Mu:Double = 3
    val Nu:Double = 1
    def F(x: Double):Double = Math.cos(2 * Math.PI * x)
    def f(x: Double):Double = Mu + Nu * F(x)

    def k1(x: Double): Double = Math.sin(2 * Math.PI * x) + 1.5
    def q1(x: Double): Double = 1
    def f1(x: Double): Double = Math.cos(2 * Math.PI * x)

    def main(args: Array[String]) = {
        var scaner = new Scanner(System.in);
        print("Input N: ")
        val N = scaner.nextInt()
        var answer = new Answer("Analitical model, Variable model");
        answer.col1 = new ModelTask(k, q, F, Mu, Nu).solution
        answer.col2 = new DifferencialModelTask(k, q, f, N).solution
        answer.print
        var answer2 = new Answer("Variable h and h/2")
        answer2.col1 = new VariableTask(k1, q1, f1, N).solution
        answer2.col2 = new VariableTask(k1, q1, f1, 2 * N).solution
        answer2.print
    }
}
