package uk.ac.soton.comp1206.game;

import javafx.application.Platform;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.network.Communicator;
import java.util.ArrayDeque;
import java.util.Random;

/**
 * The main logic, state and properties are controlled by the MultiplayerGame Class.
 */
public class MultiplayerGame extends Game{
  private static final Logger logger = LogManager.getLogger(MultiplayerGame.class);
  private final Communicator communicator;
  private final Random r = new Random();
  private final ArrayDeque<GamePiece> inc = new ArrayDeque();

  /**
   * A new Multiplayer game is created with the specified rows and columns
   * Connects communicator to send and receive messages from the server.
   * @param communicator
   * @param c
   * @param r
   */
  public MultiplayerGame(Communicator communicator, int c, int r) {
    super(c, r);
    this.communicator = communicator;
    communicator.addListener(mssg -> Platform.runLater(() -> receiveMessage(mssg.trim())));
  }

  @Override
  public void initialiseGame() {
    logger.info("Initialising game:)");
    scr.set(0);
    lvl.set(0);
    lvs.set(3);
    multi.set(1);
    iPieces();
  }

  /**
   * Following piece and all scores are received
   * @param mssg
   */
  private void receiveMessage(String mssg) {
    logger.info("Received message: {}", mssg);
    String[] elements = mssg.split(" ", 2);
    String instruction = elements[0];
    if (instruction.equals("PIECE") && elements.length > 1) {
      String data = elements[1];
      obtainPiece(Integer.parseInt(data));
    } else if (instruction.equals("SCORES") && elements.length > 1) {
      String info = elements[1];
      acquireScores(info);
    }
  }

  /**
   * Initial pieces are requested
   */
  public void iPieces() {
    for (int i = 0; i < 5; ++i) {
      communicator.send("PIECE");
    }
  }

  /**
   * Following piece is requested
   * @return the game piece
   */
  @Override
  public GamePiece spawnPiece() {
    communicator.send("PIECE");
    return inc.pop();
  }

  private void obtainPiece(int b) {
    GamePiece p = GamePiece.createPiece(b, r.nextInt(3));
    inc.push(p);
    if (!started && inc.size() > 2) {
      nPiece = spawnPiece();
      nPiece();
      started = true;
    }
  }

  private void acquireScores(String d) {
    String[] scrLines;
    scrs.clear();
    for (String scoreLine : scrLines = d.split("\\R")) {
      String[] elements = scoreLine.split(":");
      String plr = elements[0];
      int scr = Integer.parseInt(elements[1]);
      scrs.add(new Pair<>(plr, scr));
    }
    scrs.sort((a, b) -> b.getValue().compareTo(a.getValue()));
  }

  /**
   * When changed, updates player board
   * @param gBlock the block that was clicked
   * @return current board
   */
  @Override
  public boolean blockClicked(GameBlock gBlock) {
    boolean r = super.blockClicked(gBlock);
    communicator.send("BOARD " + encode());
    return r;
  }


  public String encode() {
    StringBuilder b = new StringBuilder();
    for (int x = 0; x < cols; ++x) {
      for (int y = 0; y < rows; ++y) {
        b.append(grid.get(x, y)).append(" ");
      }
    }
    return b.toString().trim();
  }
}
