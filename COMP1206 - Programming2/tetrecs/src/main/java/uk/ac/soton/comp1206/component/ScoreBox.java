package uk.ac.soton.comp1206.component;


import java.util.ArrayList;
import javafx.animation.Animation;
import javafx.animation.FadeTransition;
import javafx.animation.SequentialTransition;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.ListChangeListener;
import javafx.geometry.Pos;
import javafx.util.Duration;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * Local and Online HighScore Boxes (for single player and multiplayer
 */
public class ScoreBox extends VBox{

  private static final Logger logger = LogManager.getLogger(ScoreBox.class);

  public final SimpleListProperty<Pair<String, Integer>> scrs = new SimpleListProperty();

  private ArrayList<HBox> scrBoxes = new ArrayList<>();

  private int scrsToShow = 10;

  private boolean aReveal = false;

  private StringProperty name = new SimpleStringProperty();

  private ArrayList<String> deadP = new ArrayList<>();

  public ScoreBox() {
    getStyleClass().add("scorelist");
    setAlignment(Pos.CENTER);
    setSpacing(2.0);

    scrs.addListener((ListChangeListener<? super Pair<String, Integer>>) c -> updList());
    name.addListener(e -> updList());
  }

  public void setAReveal(boolean aReveal) {
    this.aReveal = aReveal;
  }

  public void setScrsToShow(int amount) {
    this.scrsToShow = amount;
  }


  /**
   * Animation to reveal the highscores
   */
  public void reveal() {
    logger.info("Revealing {} scores", scrBoxes.size());
    ArrayList trnstns = new ArrayList();

    for (HBox scrBox : scrBoxes) {
      FadeTransition fdr = new FadeTransition(new Duration(300.0), scrBox);
      fdr.setFromValue(0.0);
      fdr.setToValue(1.0);
      trnstns.add(fdr);
    }

    SequentialTransition trnstn = new SequentialTransition((Animation[])trnstns.toArray(Animation[]::new));
    trnstn.play();
  }

  /**
   * Update the lists of highscores
   */
  public void updList() {
    logger.info("Updating score list");
    scrBoxes.clear();

    getChildren().clear();
    int c = 0;
    for (Pair pr : scrs) {
      c++;
      if (c > scrsToShow)
        break;
      HBox scrBox = new HBox();
      scrBox.setOpacity(0.0);
      scrBox.getStyleClass().add("scoreitem");
      scrBox.setAlignment(Pos.CENTER);
      scrBox.setSpacing(10.0);

      Color clr = GameBlock.COLOURS[c];
      Text pl = new Text(pr.getKey() + ":");
      pl.getStyleClass().add("scorer");

      if (pr.getKey().equals(name.get())) {
        pl.getStyleClass().add("myscore");
      }
      if (deadP.contains(pr.getKey())) {
        pl.getStyleClass().add("deadscore");
      }

      pl.setTextAlignment(TextAlignment.CENTER);
      pl.setFill(clr);
      HBox.setHgrow(pl, Priority.ALWAYS);

      Text pts = new Text(((Integer)pr.getValue()).toString());
      pts.getStyleClass().add("points");
      pts.setTextAlignment(TextAlignment.CENTER);
      pts.setFill(clr);
      HBox.setHgrow(pts, Priority.ALWAYS);

      scrBox.getChildren().addAll(pl, pts);
      getChildren().add(scrBox);
      scrBoxes.add(scrBox);
    }

    if (this.aReveal)
      reveal();
  }

  public ListProperty<Pair<String, Integer>> scoreProperty() {
    return scrs;
  }

  public StringProperty nameProperty() {
    return name;
  }

  public void kill(String player) {
    deadP.add(player);
  }
}
