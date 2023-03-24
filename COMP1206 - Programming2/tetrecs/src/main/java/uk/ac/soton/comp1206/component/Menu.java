package uk.ac.soton.comp1206.component;

import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.layout.VBox;
import java.util.ArrayList;

public class Menu extends Group {

  private final VBox vbox;

  private final ArrayList<MenuItem> itms = new ArrayList<>();

  private int s = -1;

  public Menu() {
    vbox = new VBox(5.0);
    vbox.setAlignment(Pos.CENTER);
    vbox.getStyleClass().add("menu");
    setOnMouseMoved(e -> {
      for (MenuItem itm : itms)
        itm.deselect();
    });
    getChildren().add(vbox);
  }

  private void paint() {
    for (MenuItem itm : itms)
      itm.deselect();
    (itms.get(s)).select();
  }

  public void add(String label, Runnable action) {
    MenuItem itm = new MenuItem(label);
    itms.add(itm);
    vbox.getChildren().add(itm);
    itm.setOnAction(action);
  }

  public void up() {
    if (s > 0) {
      s--;
    } else if (s < 0) {
      s = 0;
    }
    paint();
  }

  public void down() {
    if (s < itms.size() - 1)
      s++;
    paint();
  }

  public void select() {
    (itms.get(s)).fire();
  }
}
