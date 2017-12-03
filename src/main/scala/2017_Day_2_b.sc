//http://adventofcode.com/2017/day/2
//http://adventofcode.com/2017/day/2/input

import cats.effect.{IO, Sync}
import fs2.{io, text}
import java.nio.file.Paths

val filePath = "....../adventOfCode/src/main/scala/"

def checksum(input: String): Int = {
  val values = input.split('\t').map(_ toInt)
  var max = 0
  for (i <- values)
    for (j <- values)
      if(i % j == 0 && i != j && i > max)(max = i/j)
  max
}

def converter[F[_]](implicit F: Sync[F]): F[Unit] =
  io.file.readAll[F](Paths.get(filePath + "2017_Day_2_Input.csv"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(line => checksum(line).toString + "\t")
    .through(text.lines)
    .through(text.utf8Encode)
    .through(io.file.writeAll(Paths.get(filePath + "2017_Day_2_Output.csv")))
    .runSync

// at the end of the universe...
val u: Unit = converter[IO].unsafeRunSync()

val bufferedSource = scala.io.Source.fromFile(filePath + "2017_Day_2_Output.csv")
val checkSums = bufferedSource.getLines().mkString("").split('\t').map(_ toInt)
bufferedSource.close
checkSums.toList.foldLeft(0)(_ + _)