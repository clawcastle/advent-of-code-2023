class Race(val duration: Int, val recordDistance: Int)

@main def Day6: Unit =
    part1()

def part1(): Unit =
    val fileName = "../inputs/day6.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val lines = bufferedSource.getLines().toArray

    val durations = lines(0).split(" ").filter(s => s != "").drop(1).map(s => s.toInt)
    val recordDistances = lines(1).split(" ").filter(s => s != "").drop(1).map(s => s.toInt)

    val races = durations.zip(recordDistances).map((duration, record) => Race(duration = duration, recordDistance = record))

    var result = 1

    for ((race, i) <- races.zipWithIndex) {
        println(s"Race $i. Duration: ${race.duration}, record: ${race.recordDistance}")
        val numberOfWays = Range(1, race.duration)
            .map(n => (race.duration - n) * n)
            .count(dist => dist > race.recordDistance)

        result = result * numberOfWays
    }

    println(s"Part 1 result: $result")

    bufferedSource.close()