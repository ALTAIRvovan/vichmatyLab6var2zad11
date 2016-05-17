/**
  * Created by altair on 03.05.16.
  */

object types {

    type NextLayer = (Int, Int) => Double
    type CurrentLayer = (Int, Int) => Double
    /*
        @param l Int
        @param m Int
     */
    type NodeFunction1 = (Int, Int, Int) => Double
    type IntToDouble = (Int) => Double
    type CoefficientFunction1 = (Int, CurrentLayer, NextLayer) => IntToDouble


}