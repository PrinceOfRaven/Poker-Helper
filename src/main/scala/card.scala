


case class card(suit: String,digit: Int ) :

  private val suitValue = suit.toLowerCase match
    case "ч" => 0
    case "б" => 1
    case "т" => 2
    case "п" => 3
    case _ => 0

  private val number: Int = (suitValue * 13) + digit
  
  def getDigit: Int = digit
  
  def getSuit: String = suit