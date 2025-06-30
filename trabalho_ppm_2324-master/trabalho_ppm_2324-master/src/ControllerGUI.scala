import javafx.fxml.FXML
import javafx.scene.control.Label

class ControllerGUI {

  @FXML
  private var labels: Array[Label] = _

  def zezoca(): Unit = {
    val newTextArray = Array("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y")
    for (i <- labels.indices) {
      labels(i).setText(newTextArray(i))
    }
  }
}


