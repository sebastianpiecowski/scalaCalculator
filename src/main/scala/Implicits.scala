import Calculator.calculateValues
import exception.InvalidExpression

object Implicits {

  implicit class ExtendedString(value: String) {
    def getValuesByOp(op: String): ExtendedString = {
      var expression = this.value
      while (expression.sanitizeMinusOp().getValue().contains(op)) {

        val firstPart = expression.sanitizeMinusOp().substringBefore(op)
        val startIndexOfFirstValue =
          firstPart.indexOfLast { it =>
            it == '*' || it == '/' || it == '+' || it == '-' || it == '^'
          } + 1
        var firstValue = firstPart.substring(startIndexOfFirstValue)
        val secondPart = expression.sanitizeMinusOp().substringAfter(op)
        val endIndexOfSecondValue =
          secondPart.indexOfFirst { it =>
            it == '*' || it == '/' || it == '+' || it == '-' || it == '^'
          }
        val secondValue =
          secondPart.substring(0, if (endIndexOfSecondValue == -1) secondPart.length else endIndexOfSecondValue)
        val result = calculateValues(firstValue, secondValue, op)
        if (op == "V") expression = expression.replace("V"+secondValue, result)
        else
          expression = expression.replace(firstValue.restoreValue()+op+secondValue.restoreValue(), result)

      }
      expression
    }

    def getValue(): String = this.value

    def sanitizeMinusOp(): ExtendedString = {

      if (this.value.matches("\\-\\-\\d+")) throw new InvalidExpression()
      this.value.replace("+-", "+#")
        .replace("--", "-#")
        .replace("*-", "*#")
        .replace("/-", "/#")
        .replace("^-", "^#")
        .replaceFirst("^-", "#")
    }

    def substringBefore(value: String): String = {
      var index = this.value.indexOf(value)
      if (index == -1) index = value.length
      this.value.substring(0, index)
    }

    def substringBeforeLast(value: String): String = {
      var index = this.value.lastIndexOf(value)
      if (index == -1) index = value.length
      this.value.substring(0, index)
    }


    def indexOfLast(predicate: String => Boolean): Int = {
      this.value.lastIndexOf(predicate, this.value.length)
    }

    def indexOfFirst(predicate: String => Boolean): Int = {
      this.value.indexOf(predicate)
    }

    def substringAfter(value: String): String = {
      var index = this.value.lastIndexOf(value)
      if(index == -1) index = 0 else index = index +1
      this.value.substring(index, this.value.length)
    }

    def restoreValue(): String = {
      this.value.replace("#", "-")
    }

    def contains(regex: String): Boolean = {
      regex.r.findFirstIn(this.value).isDefined
    }
  }

  implicit class ExtendedDouble(value: Double) {
    private def roundToPrecision(precision: Int): Double = { val s = math pow (10, precision); (math round value * s) / s }
    def roundToString(): String = roundToPrecision(4).toString
  }

}
