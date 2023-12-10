class Race(val duration: Long, val recordDistance: Long)

// Solves both part 1 and 2 just fine
@main def Day6: Unit =
    val fileName = "../inputs/day6.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val lines = bufferedSource.getLines().toArray

    val durations = lines(0).split(" ").filter(s => s != "").drop(1).map(s => s.toLong)
    val recordDistances = lines(1).split(" ").filter(s => s != "").drop(1).map(s => s.toLong)

    val races = durations.zip(recordDistances).map((duration, record) => Race(duration = duration, recordDistance = record))

    var result = 1

    for (race <- races) {
        val numberOfWays = Range(1, race.duration.toInt)
            .map(n => (race.duration - n) * n)
            .count(dist => dist > race.recordDistance)

        result = result * numberOfWays
    }

    println(s"Result: $result")

    bufferedSource.close()