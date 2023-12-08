@main def Day4: Unit =
    part1()

class Card(val winningNumbers: Array[Int], val cardNumbers: Array[Int])

def part1(): Unit =
    val fileName = "../inputs/day4.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val result = bufferedSource.getLines()
        .map(line => parseCardLine(line))
        .map(card => getCardScore(card))
        .sum

    println(s"Part 1 result: $result")

    bufferedSource.close()

def part2(): Unit =
    val fileName = "../inputs/day4.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val lines = bufferedSource.getLines().toArray

    for (line <- lines) {

    }

    bufferedSource.close()

def parseCardLine(line: String): Card = 
    val nums = line.split(":")(1).split('|').map(parts => parts.split(' ').filter(p => p != "").map(n => n.toInt))

    return Card(nums(0), nums(1))

def getCardScore(card: Card): Int =    
    var score = 1

    val winningCardNumbers = card.cardNumbers.count(n => card.winningNumbers.contains(n))

    score = score << winningCardNumbers
    score = score >> 1

    return score