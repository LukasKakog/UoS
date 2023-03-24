package uk.ac.soton.comp1206.scene;

import javafx.animation.FadeTransition;
import javafx.animation.PauseTransition;
import javafx.animation.SequentialTransition;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.utility.Multimedia;

public class IntroScene extends BaseScene {
  private static final Logger logger = LogManager.getLogger(IntroScene.class);

  private SequentialTransition sequence;

  /**
   * Intro Scene is created
   * @param gWindow the Game Window
   */
  public IntroScene(GameWindow gWindow) {
    super(gWindow);
    logger.info("Creating Intro Scene");
    Multimedia.playAudio("intro.mp3");
  }

  /**
   * Scene is initialised
   */
  @Override
  public void initialise() {
    scene.setOnKeyPressed(e -> {
      if (e.getCode() == KeyCode.ESCAPE) {
        Multimedia.stopAll();
        sequence.stop();
        gameWindow.startMenu();
      }
    });
  }

  /**
   * Intro window is built
   */
  public void build() {
    logger.info("Building " + this.getClass().getName());

    root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());

    var introP = new StackPane();
    introP.setMaxWidth(gameWindow.getWidth());
    introP.setMaxHeight(gameWindow.getHeight());
    introP.getStyleClass().add("intro");

    //Fade in and out transitions of ECS logo
    var lg = new ImageView(Multimedia.getImage("ECSGames.png"));
    lg.setFitWidth((gameWindow.getWidth() / 2.5));
    lg.setPreserveRatio(true);
    lg.setOpacity(0);
    introP.getChildren().add(lg);

    root.getChildren().add(introP);

    var fIn = new FadeTransition(new Duration(1600), lg);
    fIn.setToValue(1.0D);

    var p = new PauseTransition(new Duration(1000));

    var fOut = new FadeTransition(new Duration(400), lg);
    fOut.setToValue(0);

    sequence = new SequentialTransition(fIn, p, fOut);
    sequence.play();

    sequence.setOnFinished(e -> gameWindow.startMenu());
  }
}
