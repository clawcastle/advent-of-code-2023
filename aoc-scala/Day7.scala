enum Card:
    case Numeric(number: Int)
    case Letter(letter: Char)

    def score(): Int =
        this match
            case Numeric(number) => number
            case Letter(letter) => letter match
                case 'T' => 10
                case 'J' => 11
                case 'Q' => 12
                case 'K' => 13
                case 'A' => 14
                case _ => throw new Exception("Invalid value for card")
        

class Hand(val cards: Array[Card], val bid: Int) extends Ordered[Hand]:
    override def compare(that: Hand): Int =
        return 2

@main def Day7: Unit =
    part1()

def part1(): Unit =
    val fileName = "../inputs/day7.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val hands = bufferedSource.getLines()
        .map(line => parseHand(line))

    for ((hand, i) <- hands.zipWithIndex) {
        println(s"Hand $i:")
        for (card <- hand.cards) {
            println(card.score())
        }
    }

    bufferedSource.close()

def parseHand(line: String): Hand =
    val parts = line.split(' ')

    val cards = parts(0).map(char => char match
        case c: Char if c >= '0' && c <= '9' => Card.Numeric(number = c.toInt - '0')
        case _ => Card.Letter(letter = char)
    ).toArray

    val bid = parts(1).toInt

    return Hand(cards = cards, bid = bid)