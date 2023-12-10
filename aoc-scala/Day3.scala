import scala.collection.mutable.ListBuffer

class Symbol(val symbol: Char, val x: Int, val y: Int)
class Number(val number: Int, val x1: Int, val x2: Int, val y: Int):
    def isAdjacentTo(positionX: Int, positionY: Int): Boolean =
        return positionX >= x1 - 1 && positionX <= x2 + 1 && positionY >= y - 1 && positionY <= y + 1

@main def Day3(): Unit =
    // part1()
    part2()

def part1(): Unit =
    val fileName = "../inputs/day3.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val lines = bufferedSource.getLines().toArray

    val symbols = lines.zipWithIndex
        .map((line, i) => parseSymbols(line, i))
        .flatMap(s => s)

    val numbers = lines.zipWithIndex
        .map((line, i) => parseNumbers(line, i))
        .flatMap(n => n)
        .toArray

    val result = numbers
        .filter(n => symbols.exists(s => n.isAdjacentTo(s.x, s.y)))
        .map(n => n.number)
        .sum

    println(s"Part 1 result: $result")

    bufferedSource.close()

def part2(): Unit =
    val fileName = "../inputs/day3.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val lines = bufferedSource.getLines().toArray

    val numbers = lines.zipWithIndex
        .map((line, i) => parseNumbers(line, i))
        .flatMap(n => n)
        .toArray

    val gears = lines.zipWithIndex
        .map((line, i) => parseSymbols(line, i))
        .flatMap(s => s)
        .filter(s => s.symbol == '*' && numbers.count(n => n.isAdjacentTo(s.x, s.y)) == 2)

    val result = gears
        .map(gear => numbers.filter(n => n.isAdjacentTo(gear.x, gear.y)).map(n => n.number).product)
        .sum

    println(s"Part 2 result: $result")
    
    bufferedSource.close()


def parseSymbols(line: String, lineNumber: Int): List[Symbol] =
    val symbols = new ListBuffer[Symbol]()

    for ((c, i) <- line.zipWithIndex) {
        if c != '.' && (c < '0' || c > '9') then {
            symbols += new Symbol(symbol = c, x = i, y = lineNumber)
        }
    }

    return symbols.toList

def parseNumbers(line: String, lineNumber: Int): List[Number] =
    val numbers = new ListBuffer[Number]()

    var startOfNumber: Option[Int] = None

    for ((c, i) <- line.zipWithIndex) {
        if c >= '0' && c <= '9' then {
            if startOfNumber == None then {
                startOfNumber = Some(i)
            }
        } else {
            if startOfNumber != None then {
                val start = startOfNumber.get
                val n = line.slice(start, i).toInt

                val number = new Number(number = n, x1 = start, x2 = i - 1, y = lineNumber)

                numbers += number
                startOfNumber = None
            }
        }
    }

    if startOfNumber != None then {
        val start = startOfNumber.get
        val n = line.slice(start, line.length).toInt

        val number = new Number(number = n, x1 = start, x2 = line.length - 1, y = lineNumber)

        numbers += number
    }

    return numbers.toList
