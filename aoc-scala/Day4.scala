@main def Day4: Unit =
    val fileName = "../inputs/day4.txt"
    val bufferedSource = scala.io.Source.fromFile(fileName)

    val x = bufferedSource.getLines()
        .map(line => line.split(":")(1).split('|').map(parts => parts.map(n => n.toInt)))
        .map(nums => {
            val winning = nums(0)
            val cardNumbers = nums(1)
            
            
        })

    bufferedSource.close()
