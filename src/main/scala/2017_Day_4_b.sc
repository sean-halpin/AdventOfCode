//http://adventofcode.com/2017/day/4
//http://adventofcode.com/2017/day/4/input

import cats.effect.{IO, Sync}
import fs2.{io, text}
import java.nio.file.Paths
import java.util.concurrent.atomic.{AtomicInteger}

val filePath = "/Users/shalpin/src/adventProblems/AdventOfCode/src/main/scala/"

var atomicInteger = new AtomicInteger(0)

def passPhraseIsGood(input: String): Boolean = {
  val values: Seq[String] = input.split(' ').map(_.sorted).toList
  val distinctValues: Seq[String] = values.distinct

  values.length == distinctValues.length
}

def converter[F[_]](implicit F: Sync[F]): F[Unit] =
  io.file.readAll[F](Paths.get(filePath + "2017_Day_4_Input.txt"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(line => {
      if(passPhraseIsGood(line))
        atomicInteger.getAndAdd(1)
    })
    .runSync

// at the end of the universe...
val u: Unit = converter[IO].unsafeRunSync()

println(atomicInteger.get())