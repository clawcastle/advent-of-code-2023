import scala.collection.mutable.HashMap


@main def Day2: Unit =
    part1()
    part2()

def part1() : Unit =
    val fileName = "../inputs/day2.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val colors = HashMap[String, Int]("red"->12, "green"->13, "blue"->14)

    val sumOfValidGameIds = bufferedSource.getLines()
        .map(l => getGameIdAndRest(l))
        .filter((gameId, rest) => isValidGame(gameId, rest, colors))
        .map((gameId, rest) => gameId)
        .sum

    println(s"Answer to part 1: $sumOfValidGameIds")

    bufferedSource.close()

def part2() : Unit =
    val fileName = "../inputs/day2.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val sumOfProducts = bufferedSource.getLines()
        .map(l => getGameIdAndRest(l))
        .map((gameId, rest) => getMinimumAmountOfEachColor(gameId, rest))
        .map(counts => counts.product)
        .sum

    println(s"Answer to part 2: $sumOfProducts")

    bufferedSource.close()

def getGameIdAndRest(line: String) : (Int, String) =
    val Array(idPart, rest) = line.split(":", 2).map(x => x)

    val gameId = parseNum(idPart.split(' ')(1))

    (gameId, rest)


def parseNum(l: String): Int =
    var n = 0;

    var i = 0

    while i < l.length() && l.charAt(i) >= '0' && l.charAt(i) <= '9' do {
        n = n * 10
        n = n + l.charAt(i).toInt - '0'

        i = i + 1
    }

    return n

def isValidGame(gameId: Int, rest: String, colors: HashMap[String,Int]): Boolean =
    val sections = rest.split(';')

    return sections.map[Array[Boolean]](section => section.split(',').map[Boolean](s => {
            val Array(n, color) = s.trim().split(" ", 2)
            val amount = parseNum(n)
            
            colors.contains(color) && amount <= colors(color)
        })).flatMap(x => x).reduce((a,b) => a && b)


def getMinimumAmountOfEachColor(gameId: Int, rest: String): Array[Int] =
    val sections = rest.split(';')

    // index 0 is red, 1 is green, 2 is blue
    val maxAmounts = Array(0, 0, 0)

    sections.flatMap(section => section.split(',').map(s => {
        val Array(n, color) = s.trim().split(" ", 2)
        val amount = parseNum(n)

        (color, amount)
    })).foreach((color, amount) => {
        val index = color match
            case "red" => 0
            case "green" => 1
            case _ => 2

        if amount > maxAmounts(index) then {
            maxAmounts.update(index, amount)
        }
    })

    maxAmounts