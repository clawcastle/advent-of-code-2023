@main def Day1 : Unit =
    part1()

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