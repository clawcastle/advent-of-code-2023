import java.util.Stack

@main def Day4: Unit =
    part1()
    part2()

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

    val indicesOfLinesToProcess = Stack[Int]()
    var currentIndex = 0

    var count = 0

    val cards: Array[Option[Card]] = Array.fill(lines.length)(None)

    while currentIndex < lines.length do {
        indicesOfLinesToProcess.push(currentIndex)

         while !indicesOfLinesToProcess.empty() do {
            val i = indicesOfLinesToProcess.pop()
            count = count + 1

            if i < lines.length then {
                if cards(i) == None then {
                    cards.update(i, Some(parseCardLine(lines(i))))
                }
                val card = cards(i).get
                
                val winningCardNumbers = card.cardNumbers.count(n => card.winningNumbers.contains(n))

                val newIndicesToProcess = Range.inclusive(i + 1, i + winningCardNumbers)

                newIndicesToProcess.foreach(n => {
                    if n < lines.length then {
                        indicesOfLinesToProcess.push(n)
                    }
                })
            }
        }

        currentIndex = currentIndex + 1
    }

    println(s"Part 2 result: $count")

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