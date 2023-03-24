package uk.ac.soton.comp1206.scene;

import javafx.animation.RotateTransition;
import javafx.geometry.Pos;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.*;
import javafx.util.Duration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.component.Menu;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.utility.Multimedia;

/**
 * The main menu of the game. Provides a gateway to the rest of the game.
 */
public class MenuScene extends BaseScene {

    private static final Logger logger = LogManager.getLogger(MenuScene.class);
    private Menu gMenu;

    /**
     * Create a new menu scene
     * @param gameWindow the Game Window this will be displayed in
     */
    public MenuScene(GameWindow gameWindow) {
        super(gameWindow);
        logger.info("Creating Menu Scene");
    }

    /**
     * Build the menu layout
     */
    @Override
    public void build() {
        logger.info("Building " + this.getClass().getName());

        root = new GamePane(gameWindow.getWidth(),gameWindow.getHeight());

        var menuPane = new StackPane();
        menuPane.setMaxWidth(gameWindow.getWidth());
        menuPane.setMaxHeight(gameWindow.getHeight());
        menuPane.getStyleClass().add("menu-background");
        root.getChildren().add(menuPane);

        var mainPane = new BorderPane();
        menuPane.getChildren().add(mainPane);

        //Logo of the menu
        var img = new ImageView(Multimedia.getImage("TetrECS.png"));
        img.setFitWidth(gameWindow.getHeight());
        img.setPreserveRatio(true);
        mainPane.setCenter(img);

        //Animation of logo
        RotateTransition r = new RotateTransition(new Duration(3000), img);
        r.setCycleCount(-1);
        r.setFromAngle(-3);
        r.setToAngle(3);
        r.setAutoReverse(true);
        r.play();

        //components of the menu scene
        gMenu = new Menu();
        BorderPane.setAlignment(gMenu, Pos.CENTER);
        gMenu.add("Single Player", gameWindow::startChallenge);
        gMenu.add("Multi Player", gameWindow::startMultiplayerLobby);
        gMenu.add("How to Play", gameWindow::startInstructions);
        gMenu.add("Exit", () -> App.getInstance().shutdown());
        mainPane.setBottom(gMenu);
    }

    /**
     * Initialise the menu
     */
    @Override
    public void initialise() {
        Multimedia.startBackgroundMusic("menu.mp3");
        scene.setOnKeyPressed(this::handleKey);
    }

    /**
     * Menu navigation by using the keyboard
     * @param keyEvent key pressed on the keyboard
     */
    private void handleKey(KeyEvent keyEvent) {
        if (keyEvent.getCode().equals(KeyCode.ESCAPE)) {
            App.getInstance().shutdown();
        } else if (keyEvent.getCode().equals(KeyCode.DOWN) || keyEvent.getCode().equals(KeyCode.S)) {
            gMenu.down();
        } else if (keyEvent.getCode().equals(KeyCode.UP) || keyEvent.getCode().equals(KeyCode.W)) {
            gMenu.up();
        } else if (keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(KeyCode.SPACE)) {
            gMenu.select();
        }
    }
}
