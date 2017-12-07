//http://adventofcode.com/2017/day/7/

val filePath = "/Users/shalpin/src/adventProblems/AdventOfCode/src/main/scala/"

val fname = filePath + "2017_Day_7_Input.txt"
val f = scala.io.Source.fromFile(fname)
val jumpList = f.getLines().map(l => l).toList
f.close()