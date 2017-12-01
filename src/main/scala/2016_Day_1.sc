//http://adventofcode.com/2016/day/1
//http://adventofcode.com/2016/day/1/input

val inputs: List[String] =
  "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"split(',') map(_.trim) toList

object Dir extends Enumeration {
  val Left = Value(3)
  val Right = Value(1)
  def dirToEnum(str: String): Dir.Value = str.charAt(0) match {
    case 'L' => Dir.Left
    case 'R' => Dir.Right
  }
}

object Heading extends Enumeration {
  val North = Value(0)
  val East = Value(1)
  val South = Value(2)
  val West = Value(3)
}

class Command(val dir: Dir.Value,val distance: Int){
  override def toString(): String = s"$dir $distance"
}

class State(val heading: Heading.Value,val xCord: Int,val yCord:Int){
  def update(cmd : Command): State = {
    val newHeading = Heading(math.abs(heading.id + cmd.dir.id) % 4)
    val newXCord = newHeading match {
      case Heading.West => xCord + cmd.distance
      case Heading.East => xCord - cmd.distance
      case _ => xCord
    }
    val newYCord = newHeading match {
      case Heading.North => yCord + cmd.distance
      case Heading.South => yCord - cmd.distance
      case _ => yCord
    }
    new State(newHeading,newXCord,newYCord)
  }
  def distanceFromOrigin = math.abs(xCord) + math.abs(yCord)
  override def toString(): String = s"$heading x=$xCord y=$yCord distanceFromOrigin=$distanceFromOrigin"
}

val commands: List[Command] = inputs.map(input =>
  new Command(
    Dir.dirToEnum(input),
    input.substring(1).toInt
  )
)

val finalState : State = commands.foldLeft(new State(Heading.North,0,0)){ (currentState : State, nextCommand : Command) =>
  currentState.update(nextCommand)
}
finalState.toString()