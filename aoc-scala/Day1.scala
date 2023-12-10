import scala.collection.mutable.HashMap

@main def Day1 : Unit =
    part1()
    part2()

def part1() : Unit =
    val fileName = "../inputs/day1.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val sumOfNumbers = bufferedSource.getLines().map(line => {
        val first = line.find(c => c >= '0' && c <= '9')
        val last = line.reverse.find(c => c >= '0' && c <= '9')

        (first, last) match
            case (_, None) | (None, _) => 0
            case (Some(f), Some(l)) => {
                (f.toInt - '0') * 10 + (l.toInt - '0')
            }
    }).sum

    println(sumOfNumbers)

    bufferedSource.close()


def part2() : Unit =
    val fileName = "../inputs/day1.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val windowSize = 3 // Longest number string

    val x = bufferedSource.getLines().map(line => {
        val firstDigit = line
            .sliding(windowSize)
            .find(window => getDigitFromWindow(window) != None)
            .flatMap(window => getDigitFromWindow(window))
            .getOrElse(0)

        val lastDigit = line
            .sliding(windowSize)
            .filter(window => getDigitFromWindow(window) != None)
            .

        println(s"first: $firstDigit, last: $lastDigit")

        (firstDigit * 10) + lastDigit
    }).sum

    println(x)

    bufferedSource.close()


val numbers = HashMap[String, Int](
    "zero"->0, 
    "one"->1, 
    "two"->2, 
    "three"->3, 
    "four"->4, 
    "five"->5, 
    "six"->6, 
    "seven"->7, 
    "eight"->8, 
    "nine"->9
    )

def getDigitFromWindow(s: String) : Option[Int] =
    if s(0) >= '0' && s(0) <= '9' then {
        return Some(s.charAt(0).toInt - '0')
    }

    for (i <- Range.inclusive(0, 4)) {
        val key = s.slice(0, i + 1)
        if numbers.contains(key) then return Some(numbers(key))
    }

    println(s)

    return None