package uk.ac.soton.comp1206.scene;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import uk.ac.soton.comp1206.component.ChannelBox;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * The Lobby scene. Holds the UI for the lobby window.
 */
public class LobbyScene extends BaseScene {
  private static final Logger logger = LogManager.getLogger(LobbyScene.class);

  protected ScheduledExecutorService executor;
  protected Communicator communicator;
  protected VBox channelList;
  protected HBox mainBox;
  protected List<String> list;
  protected ScheduledFuture<?> loop;
  protected boolean channel;
  protected ChannelBox channelBox;

  /**
   * Lobby Scene is created
   * @param gWindow the Game Window
   */
  public LobbyScene(GameWindow gWindow) {
    super(gWindow);
    logger.info("Creating Lobby Scene");
    communicator = gWindow.getCommunicator();
    executor = Executors.newSingleThreadScheduledExecutor();
    list = new ArrayList<>();
  }

  /**
   * Scene is initialised and chat appears
   */
  @Override
  public void initialise() {
    communicator.addListener(message -> Platform.runLater(() -> handleMessage(message)));
    requestChannels();
  }

  /**
   * Lobby Window is built
   */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());
    root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());

    mainBox = new HBox();
    channelList = new VBox();
    var lPane = new VBox();

    var textF = new TextField();
    var curGames = new Text("Current Games");
    curGames.getStyleClass().add("title");
    var hostN = new Text("Host New Game");
    hostN.getStyleClass().add("heading");

    hostN.setOnMousePressed(e -> textF.setOpacity(1));

    textF.setOpacity(0);

    textF.setOnKeyPressed(e -> {
      if (e.getCode().equals(KeyCode.ENTER)) {
        textF.setOpacity(0);
        String nm = textF.getText();
        textF.clear();
        communicator.send("CREATE " + nm);
      }
    });

    lPane.getChildren().addAll(curGames, hostN, textF, channelList);
    mainBox.getChildren().add(lPane);
    mainBox.setMaxWidth(gameWindow.getWidth());
    mainBox.setMaxHeight(gameWindow.getHeight());
    mainBox.getStyleClass().add("menu-background");

    root.getChildren().add(mainBox);

    Platform.runLater(() -> scene.setOnKeyPressed(this::handleKeyPress));
  }

  public void handleKeyPress(KeyEvent ev){

    if (ev.getCode().equals(KeyCode.ESCAPE)) {
      executor.shutdownNow();
      gameWindow.startMenu();
    }
  }

  /**
   * List of users in the channel is requested and channel list is updated
   */
  private void requestChannels() {
    if (channel) {
      communicator.send("USERS");
    }
    communicator.send("LIST");
    loop = executor.schedule(this::requestChannels, 2500, TimeUnit.MILLISECONDS);
  }

  /**
   * Operates network messages
   * @param msg
   */
  private void handleMessage(String msg) {
    String[] segment = msg.split(" ", 2);
    String head = segment[0];

    //Get a list of all current channels
    if (head.equals("CHANNELS")) {

      if (segment.length == 1) {
        channelList.getChildren().clear();
        return;
      }

      String mssg = segment[1];
      List<String> li = Arrays.asList(mssg.split("\\s+"));

      if (!li.equals(list)) {
        channelList.getChildren().clear();

        for (String s : li) {
          var txt = new Text(s);
          txt.getStyleClass().add("channelItem");
          txt.setOnMouseClicked(e -> communicator.send("JOIN " + s));
          channelList.getChildren().add(txt);

        }
        list.clear();
        list.addAll(li);
      }
    }

    //Request to join the given channel, if not already in a channel
    if (head.equals("JOIN")) {
      channel = true;
      String chName = segment[1];

      channelBox = new ChannelBox(gameWindow, chName);
      mainBox.getChildren().add(channelBox);
    }

    //Received if an action was not possible
    if (head.equals("ERROR")) {
      String fmssg = segment[1];
      var al = new Alert(Alert.AlertType.ERROR);

      al.setContentText(fmssg);
      logger.error(fmssg);
      al.showAndWait();
    }

    //You are the host of this channel and can start a game
    if (head.equals("HOST")) {
      channelBox.revealStartButton();
    }

    //Leave a channel
    if (head.equals("PARTED")) {
      mainBox.getChildren().remove(channelBox);
      channel = false;
    }

    //User list in channel
    if (head.equals("USERS")) {
      String fmssg= segment[1];
      List<String> list = Arrays.asList(fmssg.split("\\s+"));

      channelBox.updateUsers(list);
    }

    //Change nickname
    if (head.equals("NICK")) {
      String message = segment[1];
    }

    //Start game
    if (head.equals("START")) {
      executor.shutdownNow();
      communicator.clearListeners();
      gameWindow.startMultiplayer();
    }

    //Received a chat message from the player
    if (head.equals("MSG")) {
      String[] parts = segment[1].split(":");

      if (parts.length == 2) {
        String nickname = parts[0];
        String fmssg = parts[1];
        channelBox.addMessage(nickname, fmssg);
      }
    }
  }
}
