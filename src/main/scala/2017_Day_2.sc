//http://adventofcode.com/2017/day/2
//http://adventofcode.com/2017/day/2/input

import cats.effect.{IO, Sync}
import fs2.{io, text}
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicInteger

val filePath = "/Users/shalpin/src/adventProblems/AdventOfCode/src/main/scala/"

var atomicInt : AtomicInteger = new AtomicInteger(0)

def checksum(input: String): Int = {
  val values = input.split('\t').map(_ toInt)
  val minMax = values.foldLeft(values(0),values(0)){ (acc, nextVal) =>
    if (nextVal <= acc._1)(nextVal, acc._2)
    else if (nextVal >= acc._2)(acc._1, nextVal)
    else acc
  }
  minMax._2 - minMax._1
}

def converter[F[_]](implicit F: Sync[F]): F[Unit] =
  io.file.readAll[F](Paths.get(filePath + "2017_Day_2_Input.tsv"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(line => atomicInt.getAndAdd(checksum(line)))
    .runSync

// at the end of the universe...
val u: Unit = converter[IO].unsafeRunSync()

println(atomicInt.get())