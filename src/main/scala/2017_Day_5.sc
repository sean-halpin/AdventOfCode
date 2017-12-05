//http://adventofcode.com/2017/day/5/

val filePath = "/Users/shalpin/src/adventProblems/AdventOfCode/src/main/scala/"

class jumpInstruction(val initialJumpVal: Int){
  var jumpVar = initialJumpVal;
  def getJumpVar(): Int = {
    jumpVar +=1
    jumpVar -1
  }
}

val fname = filePath + "2017_Day_5_Input.txt"
val f = scala.io.Source.fromFile(fname)
val jumpList = f.getLines().map(l => new jumpInstruction(l.toInt)).toList
f.close()
val jumpListLength = jumpList.length

var programCounter = 0
var programPointer = 0
while(programPointer < jumpListLength){
  programCounter+=1
  programPointer += jumpList(programPointer).getJumpVar();
}
println(programCounter) //373160