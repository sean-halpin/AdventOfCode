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
  def until(targetValue: Int): SpiralState = {
    val updateValue = currentValue + 1
    val updateStepMic = {
      if (stepMic == stepMicMax) 1
      else stepMic + 1
    }
    val updateStepMicMax = {
      if (stepMic == stepMicMax && stepMac == stepMacMax) stepMicMax + 1
      else stepMicMax
    }
    val updateStepMac = {
      if (stepMic == stepMicMax && stepMac != stepMacMax) stepMac + 1
      else if (stepMic == stepMicMax && stepMac == stepMacMax) 1
      else stepMac
    }
    val updateHeading = stepMic == stepMicMax match {
      case true => heading.turnLeft
      case false => heading
    }
    val updateCords = {
      updateHeading match {
        case North => (Cords._1, Cords._2 + 1)
        case South => (Cords._1, Cords._2 - 1)
        case East => (Cords._1 + 1, Cords._2)
        case West => (Cords._1 - 1, Cords._2)
      }
    }
    updateValue match {
      case `targetValue` => new SpiralState(updateValue, updateCords,
        updateStepMic, updateStepMicMax,
        updateStepMac, 2,
        updateHeading)
      case _ => new SpiralState(updateValue, updateCords,
        updateStepMic, updateStepMicMax,
        updateStepMac, 2,
        updateHeading) until targetValue
    }
  }
}
val beginningState = new SpiralState(1, (0,0),
  1, 1,
  1, 2,
  South)
val result = beginningState until 368
result.Cords