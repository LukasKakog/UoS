package uk.ac.soton.comp1206.scene;

import java.util.ArrayList;
import java.util.Collection;
import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableValue;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.util.Duration;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBlockCoordinate;
import uk.ac.soton.comp1206.component.GameBoard;
import uk.ac.soton.comp1206.component.TimeBar;
import uk.ac.soton.comp1206.game.Game;
import uk.ac.soton.comp1206.game.GamePiece;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.utility.Multimedia;
import uk.ac.soton.comp1206.utility.Utility;

/**
 * The Single Player challenge scene. Holds the UI for the single player challenge mode in the game.
 */
public class ChallengeScene extends BaseScene {

    private static final Logger logger = LogManager.getLogger(MenuScene.class);
    protected Game game;

    protected StackPane timerStack;
    protected GameBoard board;
    protected TimeBar timer;

    protected IntegerProperty scr = new SimpleIntegerProperty(0);
    protected IntegerProperty highscr = new SimpleIntegerProperty(0);
    protected BooleanProperty kbMode = new SimpleBooleanProperty(false);

    protected int kbX = 0;
    protected int kbY = 0;
    protected GameBoard nextPiece1;
    protected GameBoard nextPiece2;
    protected boolean chatMode = false;

    /**
     * Create a new Single Player challenge scene
     * @param gameWindow the Game Window
     */
    public ChallengeScene(GameWindow gameWindow) {
        super(gameWindow);
        logger.info("Creating Challenge Scene");
    }

    /**
     * Build the Challenge window
     */
    @Override
    public void build() {
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

        board = new GameBoard(game.getGrid(), gameWindow.getWidth() / 2, gameWindow.getWidth() / 2);
        board.keyboardModeProperty().bind(kbMode);
        mainP.setCenter(board);

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
        scrLabel.getStyleClass().add("heading");
        scrBox.getChildren().add(scrLabel);

        //Displays score
        var scrField = new Text("0");
        scrField.getStyleClass().add("score");
        scrField.textProperty().bind(scr.asString());
        scrBox.getChildren().add(scrField);
        tBar.add(scrBox, 0, 0);

        var ttl = new Text("Challenge Mode");
        HBox.setHgrow(ttl, Priority.ALWAYS);
        ttl.getStyleClass().add("title");
        ttl.setTextAlignment(TextAlignment.CENTER);
        tBar.add(ttl, 1, 0);
        GridPane.setFillWidth(ttl, Boolean.TRUE);
        GridPane.setHgrow(ttl, Priority.ALWAYS);
        GridPane.setHalignment(ttl, HPos.CENTER);

        var lvBox = new VBox();
        lvBox.setAlignment(Pos.CENTER);
        var lvsLabel = new Text("Lives");
        lvsLabel.getStyleClass().add("heading");
        lvBox.getChildren().add(lvsLabel);

        //Displays number of lives
        var lvsField = new Text("0");
        lvsField.getStyleClass().add("lives");
        lvsField.textProperty().bind(game.livesProperty().asString());
        lvBox.getChildren().add(lvsField);
        tBar.add(lvBox, 2, 0);

        var hsLabel = new Text("High Score");
        hsLabel.getStyleClass().add("heading");
        sBar.getChildren().add(hsLabel);

        //Displays the local high score
        var hsField = new Text("0");
        hsField.getStyleClass().add("hiscore");
        sBar.getChildren().add(hsField);
        hsField.textProperty().bind(highscr.asString());

        var lvlLabel = new Text("Level");
        lvlLabel.getStyleClass().add("heading");
        sBar.getChildren().add(lvlLabel);

        //Displays number of lives left
        var lvlField = new Text("0");
        lvlField.getStyleClass().add("level");
        sBar.getChildren().add(lvlField);
        lvlField.textProperty().bind(game.levelProperty().asString());

        var mprLabel = new Text("Multiplier");
        mprLabel.getStyleClass().add("heading");
        sBar.getChildren().add(mprLabel);

        //Displays multiplier of score
        var mprField = new Text("1");
        mprField.getStyleClass().add("heading");
        sBar.getChildren().add(mprField);
        mprField.textProperty().bind(game.multiplierProperty().asString());

        var nPieceLabel = new Text("Incoming");
        nPieceLabel.getStyleClass().add("heading");
        sBar.getChildren().add(nPieceLabel);

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
        board.setOnRightClick(this::rotateBlock);
        board.setOnBlockClick(this::blockClicked);

        //Displays timebar
        timerStack = new StackPane();
        mainP.setBottom(timerStack);
        timer = new TimeBar();
        BorderPane.setMargin(timerStack, new Insets(5.0, 5.0, 5.0, 5.0));
        timerStack.getChildren().add(timer);
        StackPane.setAlignment(timer, Pos.CENTER_LEFT);
    }

    /**
     * Handle when a block is clicked
     * @param gameBlock the Game Block that was clocked
     */
    protected void blockClicked(GameBlock gameBlock) {
        kbMode.set(false);
        blockAction(gameBlock);
    }

    /**
     * Set up the game object and model
     */
    public void setupGame() {
        logger.info("Starting a new challenge");
        //Start new game
        game = new Game(5, 5);
    }

    /**
     * Game is looped + UI of timebar
     * @param nLoop
     */
    protected void gameLoop(int nLoop) {
        Timeline tl = new Timeline(new KeyFrame(Duration.ZERO, new KeyValue(timer.fillProperty(), Color.GREEN)), new KeyFrame(Duration.ZERO, new KeyValue(timer.widthProperty(), timerStack.getWidth())), new KeyFrame(new Duration((double)nLoop * 0.5), new KeyValue(timer.fillProperty(), Color.YELLOW)), new KeyFrame(new Duration((double)nLoop * 0.75), new KeyValue(timer.fillProperty(), Color.RED)), new KeyFrame(new Duration(nLoop), new KeyValue(timer.widthProperty(), 0)));
        tl.play();
    }

    /**
     * Game Starts
     */
    public void startGame() {
        logger.info("Start game");
        game.start();
    }

    /**
     * Game ends
     */
    public void endGame() {
        logger.info("End game");
        game.stop();
        Multimedia.stopAll();
    }

    /**
     * Scene is initialised and game starts
     */
    @Override
    public void initialise() {
        logger.info("Initialising Challenge");
        Multimedia.startBackgroundMusic("game_start.wav", "game.wav");

        game.scoreProperty().addListener(this::setScore);
        game.setOnLineCleared(this::lineCleared);
        game.setOnGameLoop(this::gameLoop);
        game.setOnNextPiece(this::nextPiece);
        scene.setOnKeyPressed(this::handleKey);

        ArrayList<Pair<String, Integer>> scrs = Utility.loadScores();
        highscr.set(scrs.get(0).getValue());

        startGame();

        game.livesProperty().addListener((observable, oldValue, newValue) -> {

            if (oldValue.intValue() > newValue.intValue()) {
                Multimedia.playAudio("lifelose.wav");
            } else {
                Multimedia.playAudio("lifegain.wav");
            }
        });

        game.levelProperty().addListener((observable, oldValue, newValue) -> {

            if (newValue.intValue() > oldValue.intValue())
                Multimedia.playAudio("level.wav");
        });

        game.setOnGameOver(() -> {
            endGame();
            gameWindow.startScores(game);
        });
    }

    /**
     * Defines the next piece to be displayed
     * @param nPiece
     */
    protected void nextPiece(GamePiece nPiece) {
        logger.info("Next piece: " + nPiece);
        nextPiece1.setPiece(nPiece);
        nextPiece2.setPiece(game.getNextPiece());
    }

    /**
     * All the blocks cleared are highlighted
     * @param bls
     */
    protected void lineCleared(Collection<GameBlockCoordinate> bls) {

        for (GameBlockCoordinate bl : bls) {
            board.highlight(board.getBlock(bl.getX(), bl.getY()));
        }

        Multimedia.playAudio("clear.wav");
    }

    /**
     * The two pieces currently displayed are swapped
     * @param gBlock
     */
    protected void swapBlock(GameBlock gBlock) {
        swapBlock();
    }

    protected void swapBlock() {
        logger.info("Swapped block");
        Multimedia.playAudio("rotate.wav");
        game.swapCPiece();
        nextPiece1.setPiece(game.getCurrentPiece());
        nextPiece2.setPiece(game.getNextPiece());
    }

    /**
     * Current piece is rotated by 90 degrees (either left or right, accordingly)
     * @param gBlock
     */
    protected void rotateBlock(GameBlock gBlock) {
        rotateBlock();
    }

    protected void rotateBlock() {
        rotateBlock(1);
    }

    protected void rotateBlock(int r) {
        logger.info("Rotated block");
        Multimedia.playAudio("rotate.wav");
        game.rotateCurrentPiece(r);
        nextPiece1.setPiece(game.getCurrentPiece());
    }

    /**
     * Gameplay using keyboard
     * @param keyEvent key pressed on the keyboard
     */
    protected void handleKey(KeyEvent keyEvent) {

        if (this.chatMode) {
            return;
        }
        kbMode.set(true);

        if (keyEvent.getCode().equals(KeyCode.W) || keyEvent.getCode().equals(KeyCode.UP)) {

            if (kbY > 0)
                kbY--;

        } else if (keyEvent.getCode().equals(KeyCode.S) || keyEvent.getCode().equals(KeyCode.DOWN)) {

            if (kbY < game.getRows() - 1)
                kbY++;

        } else if (keyEvent.getCode().equals(KeyCode.A) || keyEvent.getCode().equals(KeyCode.LEFT)) {

            if (kbX > 0)
                kbX--;

        } else if (keyEvent.getCode().equals(KeyCode.D) || keyEvent.getCode().equals(KeyCode.RIGHT)) {

            if (kbX < game.getCols() - 1)
                kbX++;

        } else if (keyEvent.getCode().equals(KeyCode.X) || keyEvent.getCode().equals(KeyCode.ENTER)) {

            blockClicked(board.getBlock(kbX, kbY));

        } else if (keyEvent.getCode().equals(KeyCode.Q) || keyEvent.getCode().equals(KeyCode.Z) || keyEvent.getCode().equals(KeyCode.OPEN_BRACKET)) {

            rotateBlock(3);

        } else if (keyEvent.getCode().equals(KeyCode.E) || keyEvent.getCode().equals(KeyCode.C) || keyEvent.getCode().equals(KeyCode.CLOSE_BRACKET)) {

            rotateBlock();

        } else if (keyEvent.getCode().equals(KeyCode.ESCAPE)) {

            endGame();
            gameWindow.startMenu();

        } else if (keyEvent.getCode().equals(KeyCode.T)) {

            startChat();
        } else if (keyEvent.getCode().equals(KeyCode.R) || keyEvent.getCode().equals(KeyCode.SPACE)) {

            swapBlock();

        }
        board.hover(board.getBlock(kbX, kbY));
    }

    /**
     * Score of the game is set
     * If the local high score is surpassed, it is changed to the current score
     * @param observable
     * @param oldNo
     * @param newNo
     */
    protected void setScore(ObservableValue<? extends Number> observable, Number oldNo, Number newNo) {
        if (newNo.intValue() > highscr.get()) {
            highscr.set(newNo.intValue());
        }

        Timeline tl = new Timeline(new KeyFrame(Duration.ZERO, new KeyValue(scr, oldNo)), new KeyFrame(new Duration(500.0), new KeyValue(scr, newNo)));
        tl.play();
    }

    protected void startChat() { }

    /**
     * If a block is placed successfully or not, it plays the sound accordingly
     * @param gBlock
     */
    protected void blockAction(GameBlock gBlock) {
        if (game.blockClicked(gBlock)) {
            logger.info("Placed {}", gBlock);
            Multimedia.playAudio("place.wav");
            game.restartGameLoop();
        } else {
            logger.info("Can't place {}", gBlock);
            Multimedia.playAudio("fail.wav");
        }
    }
}