import scala.collection.mutable.ListBuffer

@main def Day9: Unit =
    part1()
    part2()

def part1(): Unit =
    val fileName = "../inputs/day9.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val result = bufferedSource.getLines()
        .map(line => parseHistoryLine(line))
        .map(history => extrapolate(history, calculateDifferences(history)))
        .sum

    println(s"Result part 1: $result")
    
    bufferedSource.close()

def part2(): Unit =
    val fileName = "../inputs/day9.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val result = bufferedSource.getLines()
        .map(line => parseHistoryLine(line))
        .map(history => extrapolateBackwards(history, calculateDifferences(history)))
        .sum

    println(s"Result part 2: $result")
    
    bufferedSource.close()

def parseHistoryLine(line: String): Array[Int] =
    return line.split(' ').map(s => s.toInt).toArray

def calculateDifferences(nums: Array[Int]): List[Array[Int]] =
    val diffs = ListBuffer[Array[Int]]()

    calculateDifferences(nums, diffs)

    return diffs.toList

def calculateDifferences(nums: Array[Int], differences: ListBuffer[Array[Int]]): Unit =
    if nums.length <= 1 then {
        return
    }

    val numsDiffs = nums.sliding(2).map(window => window(1) - window(0)).toArray
    differences += numsDiffs

    if numsDiffs.forall(diff => diff == 0) then {
        return
    }

    calculateDifferences(numsDiffs, differences)

def extrapolate(nums: Array[Int], differences: List[Array[Int]]): Long =
    return differences
        .reverse
        .appended(nums)
        .foldLeft(0)((a, b) => a + b.last)

def extrapolateBackwards(nums: Array[Int], differences: List[Array[Int]]): Long =
    return differences
        .reverse
        .appended(nums)
        .foldLeft(0)((a,b) => b(0) - a)
