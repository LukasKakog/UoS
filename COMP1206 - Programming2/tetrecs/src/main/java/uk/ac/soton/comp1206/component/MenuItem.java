package uk.ac.soton.comp1206.component;

import javafx.scene.Group;
import javafx.scene.text.Text;
import uk.ac.soton.comp1206.utility.Multimedia;

public class MenuItem extends Group {
  private Runnable act;
  private final Text txt;

  public MenuItem(String name) {
    txt = new Text(name);
    txt.getStyleClass().add("menuItem");
    getChildren().add(txt);
  }

  public void setOnAction(Runnable act) {
    this.act = act;
    setOnMouseClicked(e -> {
      Multimedia.playAudio("rotate.wav");
      act.run();
    });
  }

  public void fire() {
    act.run();
  }

  public void select() {
    txt.getStyleClass().add("selected");
  }

  public void deselect() {
    txt.getStyleClass().remove("selected");
  }
}
