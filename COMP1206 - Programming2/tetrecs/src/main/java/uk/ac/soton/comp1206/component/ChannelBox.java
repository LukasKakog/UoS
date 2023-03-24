package uk.ac.soton.comp1206.component;

import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import java.util.ArrayList;
import java.util.List;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.utility.Multimedia;

/**
 * Essentially the chat box of the channel selected
 */
public class ChannelBox extends VBox {
  protected GameWindow gW;
  protected Communicator communicator;
  protected String chnnlName;

  private VBox mssgs;
  protected HBox usrs;
  protected List<String> lst;
  protected ScrollPane sp;
  private Node strt;

  public ChannelBox(GameWindow gW, String chnnlName) {
    this.gW = gW;
    this.chnnlName = chnnlName;
    this.communicator = gW.getCommunicator();
    lst = new ArrayList<>();
    build();
  }

  public void build() {
    sp = new ScrollPane();
    sp.setFitToHeight(true);
    sp.setFitToWidth(true);

    sp.getStyleClass().add("gameBox");

    var ent = new TextField();
    mssgs = new VBox(2);

    ent.setOnKeyPressed(e -> {
      if (e.getCode().equals(KeyCode.ENTER)) {
        String txt = ent.getText();
        ent.clear();
        String[] prt = txt.split(" ", 2);
        if (prt[0].equals("/nick")) {
          if (prt.length > 1) {
            communicator.send("NICK " + prt[1]);
          }
          return;
        }
        communicator.send("MSG " + txt);
      }
    });

    HBox.setHgrow(this, Priority.ALWAYS);
    VBox.setVgrow(mssgs, Priority.ALWAYS);
    VBox.setVgrow(sp, Priority.ALWAYS);

    usrs = new HBox(4);

    var bttns = new HBox(4);

    var leaveGame = new Button("Leave Game");
    leaveGame.setOnMouseClicked(e -> communicator.send("PART"));

    strt = new Button("Start Game");
    strt.setOnMouseClicked(e -> communicator.send("START"));
    strt.setOpacity(0);

    bttns.getChildren().addAll(leaveGame, strt);
    sp.setContent(mssgs);

    getChildren().addAll(usrs, sp, bttns, ent);
  }

  public void updateUsers(List<String> list) {
    if (!list.equals(lst)) {
      usrs.getChildren().clear();
      for (String i : list) {
        var str = new Text(i);
        str.getStyleClass().add("channelItem");
        usrs.getChildren().add(str);
      }
      lst.clear();
      lst.addAll(list);
    }
  }

  public void addMessage(String nick, String mssg) {
    var mssgObject = new Text("<" + nick+ "> : " + mssg);
    mssgObject.getStyleClass().add("messages");
    mssgs.getChildren().add(mssgObject);
    Multimedia.playAudio("message.wav");
    sp.layout();
    sp.setVvalue(1);
  }

  public void revealStartButton() {
    strt.setOpacity(1);
  }
}
