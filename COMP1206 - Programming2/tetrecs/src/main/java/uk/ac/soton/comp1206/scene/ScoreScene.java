package uk.ac.soton.comp1206.scene;

import javafx.application.Platform;
import javafx.beans.property.*;
import java.util.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.component.ScoreBox;
import uk.ac.soton.comp1206.game.Game;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.utility.Multimedia;
import uk.ac.soton.comp1206.utility.Utility;

/**
 * The Score scene is responsible for the scores displayed after each game
 */
public class ScoreScene extends BaseScene {

  private static final Logger logger = LogManager.getLogger(ScoreScene.class);

  private final Game game;
  private Timer timer;
  private VBox scrBox;
  private Text hiscrText;

  private Communicator communicator;

  private boolean newScr = false;
  private boolean newRemoteScr = false;
  private boolean waitingForScrs = true;

  private Pair<String, Integer> myScore;

  private ScoreBox highscoreBl1;
  private ScoreBox highscoreBl2;

  private final ArrayList<Pair<String, Integer>> remoteScrs = new ArrayList<>();
  private ObservableList<Pair<String, Integer>> scrList;
  private ObservableList<Pair<String, Integer>> remoteScrList;

  private StringProperty myName = new SimpleStringProperty("");
  private BooleanProperty showScrs = new SimpleBooleanProperty(false);

  /**
   * Create a new Score scene
   * @param gWindow
   * @param gm
   */
  public ScoreScene(GameWindow gWindow, Game gm) {
    super(gWindow);
    logger.info("Creating Score Scene");
    communicator = gWindow.getCommunicator();
    this.game = gm;
  }

  /**
   * Initialisation of Scene and scores shown
   */
  @Override
  public void initialise() {
    Multimedia.playAudio("explode.wav");
    Multimedia.startBackgroundMusic("end.wav", false);
    communicator.addListener(message -> Platform.runLater(() -> receiveMessage(message.trim())));
    if (!game.getScores().isEmpty()) {
      myName.set(game.nameProperty().getValue());
    }
    communicator.send("HISCORES");
  }

  /**
   * Creates Score window
   */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());
    root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());

    //Creates window pane and set background
    var scrPane = new StackPane();
    scrPane.setMaxWidth(gameWindow.getWidth());
    scrPane.setMaxHeight(gameWindow.getHeight());
    scrPane.getStyleClass().add("menu-background");
    root.getChildren().add(scrPane);

    var mainP = new BorderPane();
    scrPane.getChildren().add(mainP);

    scrBox = new VBox();
    scrBox.setAlignment(Pos.TOP_CENTER);
    scrBox.setPadding(new Insets(5.0, 5.0, 5.0, 5.0));
    scrBox.setSpacing(20.0);
    mainP.setCenter(scrBox);

    //Adds game logo to VBox
    var img = new ImageView(Multimedia.getImage("TetrECS.png"));
    img.setFitWidth(gameWindow.getWidth() * 0.7);
    img.setPreserveRatio(true);
    scrBox.getChildren().add(img);

    var gameOverText = new Text("Game Over");
    gameOverText.setTextAlignment(TextAlignment.CENTER);
    VBox.setVgrow(gameOverText, Priority.ALWAYS);
    gameOverText.getStyleClass().add("bigtitle");
    scrBox.getChildren().add(gameOverText);

    hiscrText = new Text("High Scores");
    hiscrText.setTextAlignment(TextAlignment.CENTER);
    VBox.setVgrow(hiscrText, Priority.ALWAYS);
    hiscrText.getStyleClass().add("title");
    hiscrText.setFill(Color.LIGHTSEAGREEN);
    scrBox.getChildren().add(hiscrText);

    var scoreGrid = new GridPane();
    scoreGrid.visibleProperty().bind(showScrs);
    scoreGrid.setAlignment(Pos.CENTER);
    scoreGrid.setHgap(100.0);
    scrBox.getChildren().add(scoreGrid);

    var localScoresLabel = new Text("Local Scores");
    localScoresLabel.setTextAlignment(TextAlignment.CENTER);
    localScoresLabel.getStyleClass().add("heading");
    scoreGrid.add(localScoresLabel, 0, 0);

    var remoteScoresLabel = new Text("Online Scores");
    remoteScoresLabel.setTextAlignment(TextAlignment.CENTER);
    remoteScoresLabel.getStyleClass().add("heading");
    scoreGrid.add(remoteScoresLabel, 1, 0);

    //Box containing local scores
    highscoreBl1 = new ScoreBox();
    var b1 = new Button("Button");
    highscoreBl1.getChildren().add(b1);
    GridPane.setHalignment(highscoreBl1, HPos.CENTER);
    scoreGrid.add(highscoreBl1, 0, 1);

    //Box containing online scores
    highscoreBl2 = new ScoreBox();
    var b2 = new Button("Button");
    highscoreBl2.getChildren().add(b2);
    GridPane.setHalignment(highscoreBl2, HPos.CENTER);
    scoreGrid.add(highscoreBl2, 1, 1);

    //If there are no existing local scores, create some by calling Utility class
    if (game.getScores().isEmpty()) {
      scrList = FXCollections.observableArrayList(Utility.loadScores());
    } else {
      //If there are already high scores, the new one is added
      scrList = FXCollections.observableArrayList(game.getScores());
      localScoresLabel.setText("This game");
    }

    //List of scores is sorted again
    scrList.sort((a, b) -> b.getValue().compareTo(a.getValue()));

    remoteScrList = FXCollections.observableArrayList(remoteScrs);

    SimpleListProperty<Pair<String, Integer>> w = new SimpleListProperty(scrList);
    highscoreBl1.scoreProperty().bind(w);
    highscoreBl1.nameProperty().bind(myName);

    SimpleListProperty<Pair<String, Integer>> w2 = new SimpleListProperty(remoteScrList);
    highscoreBl2.scoreProperty().bind(w2);
    highscoreBl2.nameProperty().bind(myName);
  }

  public void startTimer(int interval) {
    if (timer != null) {
      timer.cancel();
      timer.purge();
    }

    var task = new TimerTask(){
      @Override
      public void run() {
        Platform.runLater(ScoreScene.this::returnToMenu);
      }
    };

    timer = new Timer();
    timer.schedule(task, interval);
  }

  /**
   * When timer ends, return to menu
   */
  public void returnToMenu() {
    if (newScr)
      return;
    if (timer != null)
      timer.cancel();
    gameWindow.startMenu();
  }

  /**
   * Checks for online and local high scores
   */
  public void checkForHiScore() {
    logger.info("Checking for high score");

    int curScore = game.getScore();
    int cr = 0;
    int remoteCr = 0;
    int lowScore = 0;

    if (!game.getScores().isEmpty()) {
      reveal();
      return;
    }

    if (scrList.size() > 0) {
      lowScore = (Integer) ((Pair) scrList.get(scrList.size() - 1)).getValue();
    }
    if (scrList.size() < 10) {
      newScr = true;
    }
    int lowestScoreRemote = 0;
    if (remoteScrs.size() > 0) {
      lowestScoreRemote = remoteScrs.get(remoteScrs.size() - 1).getValue();
    }
    if (remoteScrs.size() < 10) {
      newRemoteScr = true;
    }
    if (curScore > lowScore) {
      for (Pair pair : scrList) {
        if ((Integer) pair.getValue() < curScore) {
          newScr = true;
          logger.info("New local high score:)");
          break;
        }
        cr++;
      }
    }
    if (curScore > lowestScoreRemote) {
      for (Pair pair : remoteScrs) {
        if ((Integer) pair.getValue() < curScore) {
          newRemoteScr = true;
          logger.info("New remote high score:)");
          break;
        }
        ++remoteCr;
      }
    }

    //If your score is either a local or online high score, a screen is shown to input your name, and add it to the list
    if (newScr || newRemoteScr) {
      hiscrText.setText("You got a High Score!");

      var nm = new TextField();
      nm.setPromptText("Enter your name");
      nm.setPrefWidth(gameWindow.getWidth() / 2);
      nm.requestFocus();
      scrBox.getChildren().add(2, nm);

      var b = new Button("Submit");
      b.setDefaultButton(true);
      scrBox.getChildren().add(3, b);

      int addRes = cr;
      int addRRes = remoteCr;

      b.setOnAction(e -> {
        String myNm = nm.getText().replace(":", "");
        this.myName.set(myNm);
        scrBox.getChildren().remove(2);
        scrBox.getChildren().remove(2);
        myScore = new Pair<>(myNm, curScore);
        if (newScr) {
          scrList.add(addRes, myScore);
        }
        if (newRemoteScr) {
          remoteScrList.add(addRRes, myScore);
        }
        communicator.send("HISCORE " + myNm + ":" + curScore);
        Utility.writeScores(scrList);
        communicator.send("HISCORES");
        newScr = false;
        newRemoteScr = false;
      });
    } else {
      logger.info("No high score");
      reveal();
    }
  }

  /**
   * Shows high scores, both local and online
   */
  public void reveal() {
    startTimer(20000);
    scene.setOnKeyPressed(e -> returnToMenu());
    showScrs.set(true);
    highscoreBl1.reveal();
    highscoreBl2.reveal();
  }

  /**
   * Communicator sends online scores
   * @param msg Online scores
   */
  private void receiveMessage(String msg) {
    logger.info("Received message: {}", msg);
    String[] elements = msg.split(" ", 2);
    String cmd = elements[0];
    if (cmd.equals("HISCORES"))
      if (elements.length > 1) {
        String info = elements[1];
        receiveScores(info);
      } else {
        receiveScores("");
      }
  }

  /**
   * Checks if current score has possibly beaten an online high score
   * @param info Online scores
   */
  private void receiveScores(String info) {
    logger.info("Received scores: {}", info);
    remoteScrs.clear();

    String[] scrLines = info.split("\\R");
    for (String scrLine : scrLines) {
      String[] elements = scrLine.split(":", 2);
      String plr = elements[0];
      int scr = Integer.parseInt(elements[1]);
      remoteScrs.add(new Pair<>(plr, scr));
    }

    remoteScrs.sort((a, b) -> b.getValue().compareTo(a.getValue()));
    remoteScrList.clear();
    remoteScrList.addAll(remoteScrs);

    if (waitingForScrs) {
      checkForHiScore();
      waitingForScrs = false;
      return;
    }

    reveal();
  }
}
