package uk.ac.soton.comp1206.utility;

import java.util.Objects;
import javafx.util.Duration;
import javafx.scene.image.Image;
import javafx.scene.media.MediaPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Multimedia {

  private static final Logger logger = LogManager.getLogger(Multimedia.class);

  private static boolean audioEnabled = true;
  private static MediaPlayer mPlayer;
  private static MediaPlayer bgPlayer;
  private static final double bgVolume = 0.4;

  public static void changeMusic(String m) {
    changeMusic(m, false);
  }

  /**
   * All sounds being played stop.
   */
  public static void stopAll() {
    if (mPlayer != null)
      mPlayer.stop();
    if (bgPlayer != null)
      bgPlayer.stop();
  }

  /**
   * Changes music from background music to an audio.
   */
  public static void changeMusic(String music, boolean replay) {
    if (!audioEnabled)
      return;
    logger.info("Change music to: " + music);
    try {
      String sound = Objects.requireNonNull(Multimedia.class.getResource("/music/" + music)).toExternalForm();
      javafx.scene.media.Media play = new javafx.scene.media.Media(sound);
      Duration antecedent = null;
      if (bgPlayer != null) {
        antecedent = bgPlayer.getCurrentTime();
        bgPlayer.stop();
      } else {
        replay = false;
      }
      bgPlayer = new MediaPlayer(play);
      bgPlayer.setVolume(bgVolume);
      if (replay)
        bgPlayer.setStartTime(antecedent);
      bgPlayer.play();
      bgPlayer.setOnEndOfMedia(() -> startBackgroundMusic(music));
    } catch (Exception e) {
      audioEnabled = false;
      e.printStackTrace();
      logger.error("Unable to play audio file");
    }
  }

  /**
   * Play background music
   * @param Intro
   * @param music
   */
  public static void startBackgroundMusic(String Intro, String music) {
    if (!audioEnabled)
      return;
    logger.info("Start background music: " + Intro + ", " + music);
    if (bgPlayer != null)
      bgPlayer.stop();
    try {
      String sound = Objects.requireNonNull(Multimedia.class.getResource("/music/" + Intro)).toExternalForm();
      javafx.scene.media.Media play = new javafx.scene.media.Media(sound);
      bgPlayer = new MediaPlayer(play);
      bgPlayer.setVolume(bgVolume);
      bgPlayer.setOnEndOfMedia(() -> startBackgroundMusic(music));
      bgPlayer.play();
    } catch (Exception e) {
      audioEnabled = false;
      e.printStackTrace();
      logger.error("Unable to play audio file");
    }
  }

  public static void startBackgroundMusic(String music) {
    startBackgroundMusic(music, true);
  }

  /**
   * Loop through the background music
   * @param bgMusic - name of selected background music file
   * @param l - boolean used to loop background music
   */
  public static void startBackgroundMusic(String bgMusic, boolean l) {
    if (!audioEnabled)
      return;
    logger.info("Start background music: " + bgMusic);
    if (bgPlayer != null)
      bgPlayer.stop();
    try {
      String sound = Objects.requireNonNull(Multimedia.class.getResource("/music/" + bgMusic)).toExternalForm();
      javafx.scene.media.Media pl = new javafx.scene.media.Media(sound);
      bgPlayer = new MediaPlayer(pl);
      bgPlayer.setVolume(bgVolume);
      if (l)
        bgPlayer.setCycleCount(-1);
      bgPlayer.play();
    } catch (Exception e) {
      audioEnabled = false;
      e.printStackTrace();
      logger.error("Unable to play audio file");
    }
  }

  /**
   * Playing audio file
   * @param f - file to be played
   */
  public static void playAudio(String f) {
    if (!audioEnabled)
      return;
    String sound = Objects.requireNonNull(Multimedia.class.getResource("/sounds/" + f)).toExternalForm();
    logger.info("Play audio: " + sound);
    try {
      javafx.scene.media.Media play = new javafx.scene.media.Media(sound);
      mPlayer = new MediaPlayer(play);
      double mediaVolume = 0.5;
      mPlayer.setVolume(mediaVolume);
      mPlayer.play();
    } catch (Exception e) {
      audioEnabled = false;
      e.printStackTrace();
      logger.error("Unable to play audio file, disabling audio");
    }
  }

  /**
   * Get images from the images folder
   * @param img - specified image
   * @return selected image
   */
  public static Image getImage(String img) {
    try {
      return new Image(Objects.requireNonNull(Multimedia.class.getResource("/images/" + img)).toExternalForm());
    } catch (Exception e) {
      e.printStackTrace();
      logger.error("Unable to load image: {}", img);
      return null;
    }
  }

  /**
   * Use of css stylesheet
   * @param sheet
   * @return selected stylesheet
   */
  public static String getStyle(String sheet) {
    return Multimedia.class.getResource("/style/" + sheet).toExternalForm();
  }
}
