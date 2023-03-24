package uk.ac.soton.comp1206.scene;

import java.util.Objects;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.image.ImageView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextFlow;
import uk.ac.soton.comp1206.component.GameBoard;
import uk.ac.soton.comp1206.game.GamePiece;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class InstructionsScene extends BaseScene{

  private static final Logger logger = LogManager.getLogger(InstructionsScene.class);

  /**
   * New Instruction Scene is created
   * @param gWindow the Game Window
   */
  public InstructionsScene(GameWindow gWindow) {
    super(gWindow);
    logger.info("Creating Instructions Scene");
  }

  /**
   * Scene is initialised
   */
  @Override
  public void initialise() {
    scene.setOnKeyPressed(e -> gameWindow.startMenu());
  }

  /**
   * Instructions window is built
   */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());

    root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());

    var instPane = new StackPane();
    instPane.setMaxHeight(this.gameWindow.getHeight());
    instPane.setMaxWidth(this.gameWindow.getWidth());
    instPane.getStyleClass().add("menu-background");
    root.getChildren().add(instPane);

    var mainP = new BorderPane();
    instPane.getChildren().add(mainP);

    var vB = new VBox();
    BorderPane.setAlignment(vB, Pos.CENTER);
    vB.setAlignment(Pos.TOP_CENTER);
    mainP.setCenter(vB);

    var inst = new Text("Instructions");
    inst.getStyleClass().add("heading");
    vB.getChildren().add(inst);

    //Explanation of the game
    var instText = new Text("TetrECS is a fast-paced gravity-free block placement game, where you must survive by clearing rows through careful placement of the upcoming blocks before the time runs out. Lose all 3 lives and you're destroyed!");
    var instFlow = new TextFlow(instText);
    instText.getStyleClass().add("instructions");
    instText.setTextAlignment(TextAlignment.CENTER);
    instFlow.setTextAlignment(TextAlignment.CENTER);
    vB.getChildren().add(instFlow);

    //Insert image with instructions and use of buttons
    var instImage = new ImageView(
        Objects.requireNonNull(getClass().getResource("/images/Instructions.png")).toExternalForm());
    instImage.setFitWidth(gameWindow.getWidth() / 1.5);
    instImage.setPreserveRatio(true);
    vB.getChildren().add(instImage);

    var p = new Text("Game Pieces");
    p.getStyleClass().add("heading");
    vB.getChildren().add(p);

    var gridP = new GridPane();
    vB.getChildren().add(gridP);

    //set game pieces instructions and text on centre
    double pdding = (gameWindow.getWidth() - gameWindow.getWidth() / 13 * 5 ) / 2.0;
    gridP.setPadding(new Insets(0.0, pdding, 0.0, pdding));
    gridP.setHgap(10.0);
    gridP.setVgap(10.0);

    //grid pane of game pieces (game boards)
    int h = 0;
    int v = 0;
    for (int i = 0; i < 15; i++) {

      GamePiece piece = GamePiece.createPiece(i);
      GameBoard gameBoard = new GameBoard(3, 3, (gameWindow.getWidth() / 15), (gameWindow.getWidth() / 15));
      gameBoard.setPiece(piece);
      gameBoard.setReadOnly(true);
      gridP.add(gameBoard, h, v);
      h++;

      if (h == 5) {
        h = 0;
        v++;
      }
    }
  }
}
