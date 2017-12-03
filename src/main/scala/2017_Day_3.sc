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

class SpiralState(val currentValue: Int, val Cords: (Int,Int),
                  stepMic: Int, stepMicMax: Int,
                  stepMac: Int, stepMacMax: Int,
                  heading: Heading) {
  def next(): SpiralState = {
    val nextValue = currentValue + 1
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
    val nextCords = {
      heading match {
        case North => (Cords._1, Cords._2 + 1)
        case South => (Cords._1, Cords._2 - 1)
        case East => (Cords._1 + 1, Cords._2)
        case West => (Cords._1 - 1, Cords._2)
      }
    }
    new SpiralState(nextValue, nextCords,
      nextStepMic, nextStepMicMax,
      nextStepMac, stepMacMax,
      nextHeading)
  }
}
val beginningState = new SpiralState(1, (0,0),
  1, 1,
  1, 2,
  East)
val target = 368078
val result = (1 until target).foldLeft(beginningState){ (acc : SpiralState, i: Int) =>
  acc.next()
}

result.Cords // (-68,-303) 68 + 303 = 371