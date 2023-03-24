package uk.ac.soton.comp1206.scene;

import javafx.application.Platform;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextFlow;
import javafx.util.Pair;
import uk.ac.soton.comp1206.component.*;
import uk.ac.soton.comp1206.game.MultiplayerGame;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.utility.Multimedia;
import java.util.ArrayList;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * The Multiplayer scene is responsible for the UI of the multiplayer challenge
 */
public class MultiplayerScene extends ChallengeScene{
  private static final Logger logger = LogManager.getLogger(MultiplayerScene.class);
  
  private final Communicator communicator;
  private ObservableList<Pair<String, Integer>> scrList;
  private ArrayList<Pair<String, Integer>> scrs = new ArrayList();
  private StringProperty nm = new SimpleStringProperty();
  private ScoreBox lboard;
  private Text rMsg;
  private TextField sMsg;

  /**
   * New Multiplayer Challenge Scene is created
   * @param gWindow the Game window
   */
  public MultiplayerScene(GameWindow gWindow) {
    super(gWindow);
    logger.info("Starting multiplayer scene");
    communicator = gWindow.getCommunicator();
  }

  /**
   * Scene is initialised, game starts
   */
  @Override
  public void initialise(){
    logger.info("Initialising Multiplayer");
    Multimedia.startBackgroundMusic("game_start.wav", "game.wav");

    game.scoreProperty().addListener(this::setScore);
    game.setOnLineCleared(this::lineCleared);
    game.setOnGameLoop(this::gameLoop);
    game.setOnNextPiece(this::nextPiece);
    scene.setOnKeyPressed(this::handleKey);

    startGame();
    communicator.addListener(message -> Platform.runLater(() -> receiveMessage(message.trim())));
    updateName();
    updateScores();

    game.livesProperty().addListener((observable, oldV, newV) -> {
      MultiplayerScene.this.sendMessage("LIVES " + newV);
      if (oldV.intValue() > newV.intValue()) {
        Multimedia.playAudio("lifelose.wav");
      } else {
        Multimedia.playAudio("lifegain.wav");
      }
    });

    game.levelProperty().addListener((observable, oldValue, newValue) -> {
      if (newValue.intValue() > oldValue.intValue())
        Multimedia.playAudio("level.wav");
    });

    this.game.scoreProperty().addListener((observable, oldV, newV) -> MultiplayerScene.this.sendMessage("SCORE " + newV));
    
    game.setOnGameOver(() -> {
      endGame();
      gameWindow.startScores(game);
    });
  }

  /**
   * Multiplayer window is built
   */
  @Override
  public void build(){
    logger.info("Building " + this.getClass().getName());
    setupGame();

    root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());

    var challengeP = new StackPane();
    challengeP.setBackground(new Background(new BackgroundFill(Color.BLACK, CornerRadii.EMPTY, Insets.EMPTY)));
    challengeP.setMaxWidth(gameWindow.getWidth());
    challengeP.setMaxHeight(gameWindow.getHeight());
    challengeP.getStyleClass().add("challenge-background");
    root.getChildren().add(challengeP);

    var mainP = new BorderPane();
    challengeP.getChildren().add(mainP);

    var mainB = new VBox();
    mainB.setAlignment(Pos.CENTER);
    BorderPane.setAlignment(mainB, Pos.CENTER);
    mainP.setCenter(mainB);

    var gBoard = new GameBoard(game.getGrid(), gameWindow.getWidth() / 2, gameWindow.getWidth() / 2);
    gBoard.keyboardModeProperty().bind(kbMode);
    mainB.getChildren().add(gBoard);
    VBox.setVgrow(mainB, Priority.ALWAYS);

    rMsg = new Text("Press T to send a chat message");
    var messageF = new TextFlow();
    messageF.setTextAlignment(TextAlignment.CENTER);
    messageF.getChildren().add(rMsg);
    messageF.getStyleClass().add("messages");
    mainB.getChildren().add(messageF);

    //functions of the chat in the multiplayer game
    sMsg = new TextField();
    sMsg.setVisible(false);
    sMsg.setEditable(false);
    sMsg.getStyleClass().add("messageBox");
    sMsg.setOnKeyPressed(e -> {
      
      if (e.getCode().equals(KeyCode.ESCAPE)) {
        this.sendMssg("");
      }
      
      if (!e.getCode().equals(KeyCode.ENTER)) {
        return;
      }
      
      sendMssg(sMsg.getText());
      sMsg.clear();
    });
    
    mainB.getChildren().add(sMsg);

    var sBar = new VBox();
    sBar.setAlignment(Pos.CENTER);
    sBar.setSpacing(6.0);
    sBar.setPadding(new Insets(5.0, 5.0, 5.0, 5.0));
    mainP.setRight(sBar);

    var tBar = new GridPane();
    tBar.setPadding(new Insets(10.0, 10.0, 10.0, 10.0));
    mainP.setTop(tBar);

    var scrBox = new VBox();
    scrBox.setAlignment(Pos.CENTER);
    var scrLabel = new Text("Score");
    scrLabel.textProperty().bind(nm);
    scrLabel.getStyleClass().add("heading");
    scrBox.getChildren().add(scrLabel);

    //Displays your score
    var scrField = new Text("0");
    scrField.getStyleClass().add("score");
    scrField.textProperty().bind(scr.asString());
    scrBox.getChildren().add(scrField);
    tBar.add(scrBox, 0, 0);

    var ttl = new Text("Multiplayer Mode");
    HBox.setHgrow(ttl, Priority.ALWAYS);
    ttl.getStyleClass().add("title");
    ttl.setTextAlignment(TextAlignment.CENTER);
    tBar.add(ttl, 1, 0);
    GridPane.setFillWidth(ttl, Boolean.TRUE);
    GridPane.setHgrow(ttl, Priority.ALWAYS);
    GridPane.setHalignment(ttl, HPos.CENTER);

    var lBox = new VBox();
    lBox.setAlignment(Pos.CENTER);
    var lvsLabel = new Text("Lives");
    lvsLabel.getStyleClass().add("heading");
    lBox.getChildren().add(lvsLabel);

    //Displays your lives
    var livesField = new Text("0");
    livesField.getStyleClass().add("lives");
    livesField.textProperty().bind(game.livesProperty().asString());
    lBox.getChildren().add(livesField);
    tBar.add(lBox, 2, 0);

    var highscoreLabel = new Text("Versus");
    highscoreLabel.getStyleClass().add("heading");
    sBar.getChildren().add(highscoreLabel);

    scrList = FXCollections.observableArrayList(scrs);
    SimpleListProperty<Pair<String, Integer>> scoreWrapper = new SimpleListProperty<>(scrList);

    //extends the scoreList(for multiplayer)
    lboard = new ScoreBox();
    lboard.getStyleClass().add("leaderboard");
    lboard.setAReveal(true);
    lboard.setScrsToShow(5);
    lboard.scoreProperty().bind(scoreWrapper);
    lboard.nameProperty().bind(nm);
    sBar.getChildren().add(lboard);

    var nextPLabel = new Text("Incoming");
    nextPLabel.getStyleClass().add("heading");
    sBar.getChildren().add(nextPLabel);

    //Creates GameBoard of the first piece
    nextPiece1 = new GameBoard(3, 3, (this.gameWindow.getWidth() / 6), (this.gameWindow.getWidth() / 6));
    nextPiece1.setReadOnly(true);
    nextPiece1.showCentre(true);
    nextPiece1.setOnBlockClick(this::rotateBlock);
    sBar.getChildren().add(nextPiece1);

    //Creates GameBoard of the second piece
    nextPiece2 = new GameBoard(3, 3, (this.gameWindow.getWidth() / 10), (this.gameWindow.getWidth() / 10));
    nextPiece2.setReadOnly(true);
    nextPiece2.setPadding(new Insets(20.0, 0.0, 0.0, 0.0));
    nextPiece2.setOnBlockClick(this::swapBlock);
    sBar.getChildren().add(nextPiece2);

    //Handle block on GameBoard grid being clicked
    gBoard.setOnRightClick(this::rotateBlock);
    gBoard.setOnBlockClick(this::blockClicked);

    //Displays timebar
    var timerStack = new StackPane();
    mainP.setBottom(timerStack);
    var timer = new TimeBar();
    BorderPane.setMargin(timerStack, new Insets(5.0, 5.0, 5.0, 5.0));
    timerStack.getChildren().add(timer);
    StackPane.setAlignment(timer, Pos.CENTER_LEFT);
  }

  /**
   * Game object and models are set up
   */
  @Override
  public void setupGame() {
    logger.info("Starting a new multiplayer challenge");
    game = new MultiplayerGame(communicator, 5, 5);
  }

  /**
   * Game is ended
   */
  @Override
  public void endGame() {
    logger.info("End game");
    super.endGame();
    sendMessage("DIE");
  }

  private void updateName() {
    sendMessage("NICK");
  }

  private void updateScores() {
    sendMessage("SCORES");
  }

  private void sendMessage(String msg) {
    communicator.send(msg);
  }

  private void receiveMessage(String msg) {
    logger.info("Received message: {}", msg);
    
    String[] elements = msg.split(" ", 2);
    String command = elements[0];
    
    if (command.equals("SCORES") && elements.length > 1) {
      String info = elements[1];
      obtainScores(info);
    } else if (command.equals("NICK") && elements.length > 1) {
      String name = elements[1];
      
      if (!name.contains(":")) {
        setName(elements[1]);
      }
    } else if (command.equals("MSG")) {
      String info = elements[1];
      receiveMsg(info);
    }
  }

  private void receiveMsg(String info) {
    logger.info("Receieved chat: " + info);
    String[] elements = info.split(":", 2);
    String nick = elements[0];
    
    if (nick.equals(nm.get())) {
      chatMode = false;
    }
    String msg = elements[1];
    this.rMsg.setText("<" + nick + " > " + msg);
    Multimedia.playAudio("message.wav");
  }

  private void setName(String nm) {
    logger.info("Name: " + nm);
    this.nm.set(nm);
    game.nameProperty().set(nm);
  }

  private void obtainScores(String info) {
    logger.info("Received scores: {}", info);
    String[] scoreLines;
    scrs.clear();
    
    for (String scoreLine : scoreLines = info.split("\\R")) {
      String[] elements = scoreLine.split(":");
      String plr = elements[0];
      int scr = Integer.parseInt(elements[1]);
      String lvs = elements[2];
      
      if (lvs.equals("DEAD")) {
        lboard.kill(plr);
      }
      scrs.add(new Pair<>(plr, scr));
    }
    
    scrs.sort((x, y) -> y.getValue().compareTo(x.getValue()));
    scrList.clear();
    scrList.addAll(scrs);
  }

  @Override
  protected void startChat() {
    chatMode = true;
    
    Platform.runLater(() -> {
      sMsg.setVisible(true);
      sMsg.setEditable(true);
      sMsg.requestFocus();
    });
  }

  private void sendMssg(String msg) {
    sMsg.setEditable(false);
    sMsg.setVisible(false);
    sendMessage("MSG " + msg);
  }
}
