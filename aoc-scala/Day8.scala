enum Instruction:
    case Left, Right

class Node(val id: String, val left: String, val right: String)

@main def Day8: Unit =
    part1()

def part1(): Unit =
    val fileName = "../inputs/day8.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val lines = bufferedSource.getLines()

    val instructionsLine = lines.next()
    val nodesLines = lines.drop(1)

    val instructions = parseInstructions(instructionsLine).toArray

    val nodes = nodesLines
        .map(line => parseNode(line))
        .map(node => (node.id, node))
        .toMap

    val root = nodes("AAA")

    var count = 0
    var instructionIndex = 0
    var current = root

    while current.id != "ZZZ" do {
        count = count + 1
        val instruction = instructions(instructionIndex)

        current = instruction match
            case Instruction.Left => nodes(current.left)
            case Instruction.Right => nodes(current.right)

        instructionIndex = (instructionIndex + 1) % instructions.length
    }

    println(s"Part 1 result: $count")
    
    bufferedSource.close()

def parseInstructions(line: String): Iterable[Instruction] =
    return line.map(c => c match
        case 'L' => Instruction.Left
        case 'R' => Instruction.Right
        case _ => throw new Exception("Invalid value for instruction.")
    )

val nodePattern = "[A-Z]+, [A-Z]+".r

def parseNode(line: String): Node =
    val id = line.slice(0, 3)

    val children = nodePattern.findFirstIn(line)
        .get

    val leftId = children.slice(0, 3)
    val rightId = children.takeRight(3)

    return new Node(id = id, left = leftId, right = rightId)