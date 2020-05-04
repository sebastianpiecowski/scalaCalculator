import exception.{DivideByZero, InvalidExpression}
import javafx.event.ActionEvent
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextArea, TextField}
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.scene.text.Font


//todo create common methods
object CalculatorGUI extends JFXApp {
  stage = new PrimaryStage {

    //setUp
    title.value = "Calculator"
    resizable = false
    width = 280
    height = 250
    centerOnScreen()

    //variables
    val buttonSize = 50
    var bracket = 0

    // NUMBERS
    val numberButtons = Range(0, 10).map(value => {
      new Button(value.toString)
    })

    //OPERATORS
    //SIMPLE MATH OPERATORS
    val mathOperatorSigns = IndexedSeq("/", "*", "+", "-")
    val mathOperators = mathOperatorSigns.map(sign => {
      new Button {
        text = sign; disable = true
      }
    })

    //REST OF OPERATORS
    val dot = new Button {
      text = ".";
      tooltip = "Subtract";
      disable = true
    }
    val startBracket = new Button {
      text = "(";
      tooltip = "Start bracket"
    }
    val endBracket = new Button {
      text = ")";
      tooltip = "Close bracket";
      disable = true
    }
    val eval = new Button {
      text = "=";
      tooltip = "Evaluate";
      disable = true
    }
    val cLast = new Button {
      text = "<-";
      tooltip = "Remove last"
    }
    val clear = new Button {
      text = "C";
      tooltip = "Clear"
    }

    // SET SCENE
    val text = new TextArea
    val pane = new GridPane
    scene = new Scene {
      fill = Color.rgb(242, 241, 240)

      text.prefHeight = 60
      text.prefWidth = 270
      text.layoutX = 5
      text.layoutY = 5
      text.setWrapText(true)
      text.setFont(Font.font("Serif", 20))

      pane.setHgap(5)
      pane.setVgap(5)

      mathOperators.foreach(button => {
        button.prefWidth = buttonSize
      })

      numberButtons.foreach(button => {
        button.prefWidth = buttonSize
      })

      dot.prefWidth = buttonSize
      startBracket.prefWidth = buttonSize
      endBracket.prefWidth = buttonSize
      eval.prefWidth = buttonSize
      cLast.prefWidth = buttonSize
      clear.prefWidth = buttonSize

      clear.onAction = (e: ActionEvent) => {
        text.text = ""
        commonOperations()
      }

      cLast.onAction = (e: ActionEvent) => {
        if (text.text.value.length > 0) {
          text.text = text.text.value.substring(0, text.text.value.size - 1)
        }
        commonOperations()
      }

      numberButtons.foreach(button => {
        button.onAction = (e: ActionEvent) => {
          text.text = text.text.value + button.text.value
          commonOperations()
        }
      })

      mathOperators.foreach(button => {
        button.onAction = (e: ActionEvent) => {
          text.text = text.text.value + button.text.value
          commonOperations()
        }
      })

      eval.onAction = (e: ActionEvent) => {
        val toCalculate = text.text.value
        try {
          val result = Calculator.evaluate(toCalculate)
          text.text = result
          commonOperations()
        } catch {
          case e: InvalidExpression => {
            text.text = "Invalid expression"
          }
          case e: DivideByZero => {
            text.text = "Cannot divide by zero moron"
          }
        }
      }

      dot.onAction = (e: ActionEvent) => {
        text.text = text.text.value + "."
        commonOperations()
      }

      startBracket.onAction = (e: ActionEvent) => {
        if (canActivateStartBracket(text.text.value)) {
          bracket = bracket + 1
          text.text = text.text.value + "("
        } else {
          startBracket.disable = true
        }
        commonOperations()
      }

      endBracket.onAction = (e: ActionEvent) => {
        if (canActivateEndBracket(text.text.value)) {
          endBracket.disable = false
          text.text = text.text.value + ")"
          bracket = bracket - 1
        } else {
          endBracket.disable = true
        }
        commonOperations()
      }

      // ADD ALL TO PANE
      pane.add(numberButtons.apply(7), 1, 0)
      pane.add(numberButtons.apply(8), 2, 0)
      pane.add(numberButtons.apply(9), 3, 0)
      pane.add(getSimpleMathOperator("/"), 4, 0)
      pane.add(getSimpleMathOperator("*"), 5, 0)
      pane.add(numberButtons.apply(4), 1, 1)
      pane.add(numberButtons.apply(5), 2, 1)
      pane.add(numberButtons.apply(6), 3, 1)
      pane.add(getSimpleMathOperator("-"), 4, 1)
      pane.add(getSimpleMathOperator("+"), 5, 1)
      pane.add(numberButtons.apply(1), 1, 2)
      pane.add(numberButtons.apply(2), 2, 2)
      pane.add(numberButtons.apply(3), 3, 2)
      pane.add(startBracket, 4, 2)
      pane.add(endBracket, 5, 2)
      pane.add(numberButtons.apply(0), 1, 3)
      pane.add(dot, 2, 3)
      pane.add(clear, 3, 3)
      pane.add(cLast, 4, 3)
      pane.add(eval, 5, 3)

      pane.setLayoutY(80)

      content = List(text, pane)
    }

    def commonOperations() = {
      if (canActivateNumbers(text.text.value)) deactivateNumeric() else activateNumeric()
      if (canActivateDot(text.text.value)) activeButton(dot) else deactiveButton(dot)
      if (canActivateOperators(text.text.value)) activateMathOperators() else deactivateMathOperators()
      if (canActivateStartBracket(text.text.value)) activeButton(startBracket) else deactiveButton(startBracket)
      if (canActivateEndBracket(text.text.value)) activeButton(endBracket) else deactiveButton(endBracket)
      if (canActivateEvaluate(text.text.value)) activeButton(eval) else deactiveButton(eval)
    }

    def activateMathOperators() = {
      mathOperators.foreach(button => {
        activeButton(button)
      })
    }

    def deactivateMathOperators(): Unit = {
      mathOperators.foreach(button => {
        deactiveButton(button)
      })
    }

    def canActivateEvaluate(input: String): Boolean = {
      if (canActivateOperators(input) && mathOperatorSigns.exists(input.contains) && bracket == 0 && !Implicits.ExtendedString(input).contains("\\d+[\\.\\d]*(/0(?!\\.|\\d))")) true else false
    }

    def canActivateDot(input: String): Boolean = {
      val valueAfterOp = getValueAfterLastOperator(input)
      if (!valueAfterOp.isBlank && !valueAfterOp.contains(".")) true else false
    }

    def canActivateOperators(input: String): Boolean = {
      val valueAfterOp = getValueAfterLastOperator(input)
      if (!valueAfterOp.isBlank && valueAfterOp.last != '.') true else false
    }

    def canActivateStartBracket(input: String): Boolean = {
      if (input.nonEmpty && mathOperatorSigns.appended('(').contains(input.last) || input.isEmpty) true else false
    }

    def canActivateEndBracket(input: String): Boolean = {
      if (canActivateOperators(input) && bracket > 0) true else false
    }

    def canActivateNumbers(input: String): Boolean = {
      val valueAfterOp = getValueAfterLastOperator(input)
      if (!valueAfterOp.startsWith("0") && valueAfterOp.startsWith("0.")) true else false
    }

    //GETTERS
    def getSimpleMathOperator(input: String): Button = {
      mathOperators.find(button => button.text.value == input).orNull
    }

    def getValueAfterLastOperator(input: String): String = {
      var lastIndex: Int = mathOperatorSigns.map(char => input.lastIndexOf(char)).max
      if (lastIndex == -1) lastIndex = 0 else lastIndex += 1
      input.substring(lastIndex, input.length)
    }

    def deactivateNumeric() = {
      numberButtons.foreach(button => {
        deactiveButton(button)
      })
    }

    def activateNumeric() = {
      numberButtons.foreach(button => {
        activeButton(button)
      })
    }
  }

  //COMMON
  def activeButton(button: Button) = {
    button.disable = false
  }

  def deactiveButton(button: Button) = {
    button.disable = true
  }
}
