@main def Day4: Unit =
    val fileName = "../inputs/day4.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val x = bufferedSource.getLines()
        .map(line => line.split(":")(1).split('|').map(parts => parts.split(' ').filter(p => p != "").map(n => n.toInt)))
        .map(nums => getCardScore(nums(0), nums(1)))
        .sum


    println(x)

    bufferedSource.close()

    
def getCardScore(winningNumbers: Array[Int], cardNumbers: Array[Int]): Int =    
    var score = 1

    val winningCardNumbers = cardNumbers.count(n => winningNumbers.contains(n))

    score = score << winningCardNumbers
    score = score >> 1

    return score