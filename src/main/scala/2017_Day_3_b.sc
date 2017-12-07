//http://adventofcode.com/2017/day/3
//input = 368078

trait Heading {
  def turnLeft(): Heading = this match {
    case North => West
    case South => East
    case East => North
    case West=> South
  }
}

case object North extends Heading
case object South extends Heading
case object East extends Heading
case object West extends Heading

class SpiralState(var currentValue: Int, val Cords: (Int,Int),
                  val stepMic: Int, val stepMicMax: Int,
                  val stepMac: Int, val stepMacMax: Int,
                  val heading: Heading, val previousState: Option[SpiralState]) {
  def AreAdjacent(state1: SpiralState, state2: SpiralState): Boolean = {
    val cordDiff = (state1.Cords._1 - state2.Cords._1,
      state1.Cords._2 - state2.Cords._2)
    cordDiff match {
      case (1,1) => true
      case (1,0) => true
      case (1,-1) => true
      case (-1,1) => true
      case (-1,0) => true
      case (-1,-1) => true
      case (0,-1) => true
      case (0,1) => true
      case _ => false
    }
  }
  def calcCurrentValue = {
      def getPreviousStates(latest: SpiralState, acc: List[SpiralState] = Nil): List[SpiralState] = {
        latest.previousState match {
          case None => acc
          case Some(p) =>  getPreviousStates(p, latest :: acc)
        }
      }
    if(this.previousState.isDefined) {
      val allPreviousAdjacent = getPreviousStates(this.previousState.get).filter(s =>
        AreAdjacent(s, this))
      allPreviousAdjacent.map(_.currentValue).sum
    }
    else 1
  }
  currentValue = calcCurrentValue

  def next(): SpiralState = {
    val nextCords = {
      heading match {
        case North => (Cords._1, Cords._2 + 1)
        case South => (Cords._1, Cords._2 - 1)
        case East => (Cords._1 + 1, Cords._2)
        case West => (Cords._1 - 1, Cords._2)
      }
    }
    val nextStepMic = {
      if (stepMic == stepMicMax) 1
      else stepMic + 1
    }
    val nextStepMicMax = {
      if (stepMic == stepMicMax && stepMac == stepMacMax) stepMicMax + 1
      else stepMicMax
    }
    val nextStepMac = {
      if (stepMic == stepMicMax && stepMac != stepMacMax) stepMac + 1
      else if (stepMic == stepMicMax && stepMac == stepMacMax) 1
      else stepMac
    }
    val nextHeading = stepMic == stepMicMax match {
      case true => heading.turnLeft
      case false => heading
    }
    new SpiralState(-1, nextCords,
      nextStepMic, nextStepMicMax,
      nextStepMac, stepMacMax,
      nextHeading, Some(this))
  }
}
val beginningState = new SpiralState(1, (0,0),
  1, 1,
  1, 2,
  East, None)
val target = 65
val result = (1 until target).foldLeft(beginningState){ (acc : SpiralState, _: Int) =>
  acc.next()
}

result.Cords // res0: (Int, Int) = (-4,4)
result.currentValue // res1: Int = 369601