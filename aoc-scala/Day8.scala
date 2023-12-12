enum Instruction:
    case Left, Right

@main def Day8: Unit =
    println("day 8")

def part1(): Unit =
    val fileName = "../inputs/day3.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)


    val lines = bufferedSource.getLines()

    val instructionsLine = lines.next()
    val nodesLines = lines.drop(1)
    
    bufferedSource.close()