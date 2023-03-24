package uk.ac.soton.comp1206.game;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import javafx.application.Platform;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBlockCoordinate;
import uk.ac.soton.comp1206.event.GameLoopListener;
import uk.ac.soton.comp1206.event.GameOverListener;
import uk.ac.soton.comp1206.event.LineClearedListener;
import uk.ac.soton.comp1206.event.NextPieceListener;

/**
 * The Game class handles the main logic, state and properties of the TetrECS game. Methods to manipulate the game state
 * and to handle actions made by the player should take place inside this class.
 */
public class Game {

    private static final Logger logger = LogManager.getLogger(Game.class);

    /**
     * Number of rows
     */
    protected final int rows;

    /**
     * Number of columns
     */
    protected final int cols;

    protected IntegerProperty scr = new SimpleIntegerProperty(0);
    protected IntegerProperty lvl = new SimpleIntegerProperty(0);
    protected IntegerProperty lvs = new SimpleIntegerProperty(0);
    protected IntegerProperty multi = new SimpleIntegerProperty(0);
    protected StringProperty name = new SimpleStringProperty();

    protected ArrayList<Pair<String, Integer>> scrs = new ArrayList();

    protected GamePiece cPiece;
    protected GamePiece nPiece;

    protected LineClearedListener lineClearedListener = null;
    protected GameLoopListener gameLoopListener = null;
    protected NextPieceListener nextPieceListener = null;
    private GameOverListener gameOverListener = null;

    protected final ScheduledExecutorService exctr;
    protected ScheduledFuture<?> nLoop;
    protected boolean started = false;

    /**
     * The grid model linked to the game
     */
    protected final Grid grid;

    /**
     * Create a new game with the specified rows and columns. Creates a corresponding grid model.
     * @param cols number of columns
     * @param rows number of rows
     */
    public Game(int cols, int rows) {
        this.cols = cols;
        this.rows = rows;

        //Create a new grid model to represent the game state
        this.grid = new Grid(cols,rows);

        exctr = Executors.newSingleThreadScheduledExecutor();
    }

    /**
     * Start the game
     */
    public void start() {
        logger.info("Starting game");
        initialiseGame();
        startGameLoop();
    }

    /**
     * Stop the game
     */
    public void stop() {
        logger.info("Stopping game");
        exctr.shutdownNow();
    }

    /**
     * Initialise a new game and set up anything that needs to be done at the start
     */
    public void initialiseGame() {
        logger.info("Initialising game");
        scr.set(0);
        lvl.set(0);
        lvs.set(3);
        multi.set(1);
        nPiece = spawnPiece();
        nPiece();
        started = true;
    }

    /**
     * Replaces the current piece with a new piece
     * @return the swapped current piece
     */
    public GamePiece nPiece() {
        cPiece = nPiece;
        nPiece = spawnPiece();
        logger.info("Current piece: {}", cPiece);
        logger.info("Next piece: {}", nPiece);
        if (nextPieceListener != null)
            nextPieceListener.nextPiece(cPiece);
        return cPiece;
    }

    /**
     * Handle what should happen when a particular block is clicked
     * @param gBlock the block that was clicked
     */
    public boolean blockClicked(GameBlock gBlock) {
        //Get the position of this block
        int x = gBlock.getX();
        int y = gBlock.getY();

        logger.info("Block clicked: {},{}", x, y);

        if (cPiece == null) {
            logger.error("No current piece");
            return false;
        }
        boolean added = grid.addPieceCentered(cPiece, gBlock.getX(), gBlock.getY());
        if (!added) {
            return false;
        }
        afterPiece();
        nPiece();
        return true;
    }

    /**
     * It's called after playing a piece
     * Clears any full vertical/horizontal lines that have been made and when two or more lines are intersecting
     */
    public void afterPiece() {
        int ttl;
        int lns = 0;
        HashSet<IntegerProperty> clear = new HashSet<>();
        HashSet<GameBlockCoordinate> clearBlocks = new HashSet<>();

        //selects full horizontal lines
        for (int x = 0; x < this.cols; ++x) {
            int i;
            ttl = this.rows;
            for (i = 0; i < this.rows && this.grid.get(x, i) != 0; ++i) {
                ttl--;
            }
            if (ttl != 0) continue;
            lns++;
            for (i = 0; i < this.rows; ++i) {
                clear.add(this.grid.getGridProperty(x, i));
                clearBlocks.add(new GameBlockCoordinate(x, i));
            }
        }
        //selects full vertical lines
        for (int y = 0; y < this.rows; ++y) {
            int i;
            ttl = this.rows;
            for (i = 0; i < this.cols && this.grid.get(i, y) != 0; ++i) {
                ttl--;
            }
            if (ttl != 0) continue;
            lns++;
            for (i = 0; i < this.cols; ++i) {
                clear.add(this.grid.getGridProperty(i, y));
                clearBlocks.add(new GameBlockCoordinate(i, y));
            }
        }
        //when no lines are full multiplier is set back to 1
        if (lns == 0) {
            if (multi.get() > 1) {
                logger.info("Multiplier set to 1:(");
                multi.set(1);
            }
            return;
        }
        logger.info("Cleared {} lines", lns);

        //score increases according to the lines that are cleared and the multiplier
        increaseScore(lns * clear.size() * 10 * multi.get());
        logger.info("Score increased by {}", (lns * clear.size() * 10 * multi.get()));

        //multiplier increases by 1
        multi.set(multi.add(1).get());
        logger.info("Multiplier now at {}", multi.get());

        //level increases by 1 when a multiple of 1000 is reached
        lvl.set(Math.floorDiv(scr.get(), 1000));

        for (IntegerProperty sqr : clear)
            sqr.set(0);

        //clears full lines
        if (lineClearedListener != null)
            lineClearedListener.lineCleared(clearBlocks);
    }

    public void increaseScore(int amnt) {
        this.scr.set(this.scr.add(amnt).get());
    }

    public void setOnGameLoop(GameLoopListener listener) {
        gameLoopListener = listener;
    }
    public void setOnLineCleared(LineClearedListener listener) {
        lineClearedListener = listener;
    }
    public void setOnNextPiece(NextPieceListener listener) {
        nextPieceListener = listener;
    }
    public void setOnGameOver(GameOverListener listener) {
        gameOverListener = listener;
    }

    public int getScore() {
        return scoreProperty().get();
    }
    public ArrayList<Pair<String, Integer>> getScores() {
        return this.scrs;
    }

    public IntegerProperty scoreProperty() {
        return scr;
    }
    public IntegerProperty livesProperty() {
        return lvs;
    }
    public IntegerProperty levelProperty() {
        return lvl;
    }
    public IntegerProperty multiplierProperty() {
        return multi;
    }
    public StringProperty nameProperty() {
        return name;
    }

    /**
     * In case you run out of time, the game loops
     */
    public void startGameLoop() {
        nLoop = exctr.schedule(this::gameLoop, getTimerDelay(), TimeUnit.MILLISECONDS);
        if (gameLoopListener != null)
            gameLoopListener.gameLoop(getTimerDelay());
    }

    /**
     * Game ends
     */
    public void gameOver() {
        logger.info("Game over:(");
        if (gameOverListener != null)
            Platform.runLater(() -> gameOverListener.gameOver());
    }

    /**
     * When a life is lost, the game loops and multiplier is reduced to 1, a new piece appears, and the timer is reset
     */
    public void gameLoop() {
        logger.info("Game Loop!");
        if (multi.get() > 1) {
            logger.info("Multiplier set to 1");
            multi.set(1);
        }
        decreaseLives();
        nPiece();
        int nextRun = getTimerDelay();
        if (gameLoopListener != null)
            gameLoopListener.gameLoop(nextRun);
        nLoop = exctr.schedule(this::gameLoop, nextRun, TimeUnit.MILLISECONDS);
    }

    /**
     * Restarts the game loop (either when a piece is placed, a line is cleared, etc.)
     */
    public void restartGameLoop() {
        nLoop.cancel(false);
        startGameLoop();
    }

    /**
     * Sets the timer according to current level, minimum of 2.5 seconds
     * @return timer length
     */
    public int getTimerDelay() {return Math.max(2500, 12000 - 500 * lvl.get());}

    public GamePiece getNextPiece() { return nPiece; }
    public GamePiece getCurrentPiece() {
        return cPiece;
    }
    public void rotateCurrentPiece() {
        cPiece.rotate();
    }
    public void rotateCurrentPiece(int rotations) {
        cPiece.rotate(rotations);
    }
    public void increaseLevel() {
        lvl.set(lvl.add(1).get());
    }

    /**
     * Decreases lives by 1, if they get to less than 0, game is over.
     */
    public void decreaseLives() {
        if (lvs.get() > 0) {
            lvs.set(lvs.subtract(1).get());
            logger.info("Lost a life:(");
        } else {
            gameOver();
        }
    }

    /**
     * Current and next pieces are swapped
     */
    public void swapCPiece() {
        GamePiece holdingPiece = cPiece;
        cPiece = nPiece;
        nPiece = holdingPiece;
    }

    /**
     * New Random GamePiece is created and placed in a random rotation
     * @return the random piece
     */
    public GamePiece spawnPiece(){
        var r = new Random();
        GamePiece p = GamePiece.createPiece(r.nextInt(15), r.nextInt(3));
        return p;
    }

    /**
     * Get the grid model inside this game representing the game state of the board
     * @return game grid model
     */
    public Grid getGrid() {
        return grid;
    }

    /**
     * Get the number of columns in this game
     * @return number of columns
     */
    public int getCols() {
        return cols;
    }

    /**
     * Get the number of rows in this game
     * @return number of rows
     */
    public int getRows() {
        return rows;
    }
}
