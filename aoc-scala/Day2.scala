import java.util.Stack
import scala.collection.mutable.HashMap

@main def Day2: Unit =
    val colors = HashMap[String, Int]("red"->12, "green"->13, "blue"->14)

    val fileName = "../inputs/day2.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val sumOfValidGameIds = bufferedSource.getLines()
        .map(l => getGameIdAndRest(l))
        .filter((gameId, rest) => isValidGame(gameId, rest, colors))
        .map((gameId, rest) => gameId)
        .sum

    println(sumOfValidGameIds)

    bufferedSource.close()


def getGameIdAndRest(line: String) : (Int, String) =
    val Array(idPart, rest) = line.split(":", 2).map(x => x)

    val gameId = parseNum(idPart.split(' ')(1))

    (gameId, rest)


def parseNum(l: String): Int =
    var n = 0;

    val s = Stack[Int]()

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


    