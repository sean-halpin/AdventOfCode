//http://adventofcode.com/2017/day/6
//

//val inputs = "0\t2\t7\t0".split('\t').map(_ toInt)toList
val inputs = "2\t8\t8\t5\t4\t2\t3\t1\t5\t5\t1\t2\t15\t13\t5\t14".split('\t').map(_ toInt)toList

val v = scala.collection.immutable.Vector.empty ++ inputs

def distributionCycle(v: Vector[Int]) = {
  def reDistribute(v: Vector[Int], index: Int, remainder : Int) : Vector[Int] = {
    val indexModulus = index % v.length
    remainder match {
      case 0 => v
      case _ => reDistribute(v.updated(indexModulus,v(indexModulus)+1),
        indexModulus+1,
        remainder-1)
    }
  }
  val indexOfMax = v.zipWithIndex.maxBy(_._1)._2
  reDistribute(v.updated(indexOfMax, 0), indexOfMax + 1, v(indexOfMax))
}

def findDistributionRepetition(allSeenV: Vector[Vector[Int]], v: Vector[Int], cycles: Int): Int = {
  allSeenV.contains(v) match {
    case true => cycles
    case false => findDistributionRepetition(allSeenV :+ v, distributionCycle(v), cycles + 1)
  }
}

findDistributionRepetition(Vector(Vector[Int](0)),v,0)