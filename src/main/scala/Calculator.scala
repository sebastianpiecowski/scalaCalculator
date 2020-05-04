import exception.{DivideByZero, InvalidExpression}

object Calculator {

  def evaluate(expression: String): String = {
    var exp = expression
    while (Implicits.ExtendedString(exp).contains("\\(.+\\)")) {
      val validatedExpression = findExpressionInBrackets(exp)
      val calculated = calculateExpression(validatedExpression)
      exp = exp.replace("("+validatedExpression+")", calculated)
    }

    calculateExpression(exp)
  }

  def calculateExpression(validatedExpression: String): String = {
      Implicits.ExtendedString(validatedExpression).getValuesByOp("V").getValuesByOp("^").getValuesByOp("*").getValuesByOp("/")
        .getValuesByOp("-")
        .getValuesByOp("+").getValue()
  }

  def calculateValues(x: String, y: String, op: String): String = {
    op match {
      case "V" => {
        if (!x.isEmpty()) throw new InvalidExpression()
        Implicits.ExtendedDouble(Math.sqrt(Implicits.ExtendedString(y).restoreValue().toDouble)).roundToString()
      }
      case "+" => {
        Implicits.ExtendedDouble(Implicits.ExtendedString(x).restoreValue().toDouble + Implicits.ExtendedString(y).restoreValue().toDouble).roundToString()
      }
      case "-" => {
        Implicits.ExtendedDouble(Implicits.ExtendedString(x).restoreValue().toDouble - Implicits.ExtendedString(y).restoreValue().toDouble).roundToString()
      }
      case "*" => {
        Implicits.ExtendedDouble(Implicits.ExtendedString(x).restoreValue().toDouble * Implicits.ExtendedString(y).restoreValue().toDouble).roundToString()
      }
      case "/" => {
        if(y.toDouble == 0.0) {
          throw new DivideByZero()
        }
        Implicits.ExtendedDouble(Implicits.ExtendedString(x).restoreValue().toDouble / y.toDouble).roundToString()
      }
      case "^" => {
        Implicits.ExtendedDouble(scala.math.pow(Implicits.ExtendedString(x).restoreValue().toDouble, Implicits.ExtendedString(y).restoreValue().toDouble)).roundToString()
      }
      case _ => {
        // op not supported
        throw new InvalidExpression()
      }
    }
  }

  def findExpressionInBrackets(expression: String): String = {
    if (!Implicits.ExtendedString(expression).contains("[()]")) expression else
      findExpressionInBrackets(Implicits.ExtendedString(Implicits.ExtendedString(expression).substringAfter("(")).substringBeforeLast(")"))
  }
}
