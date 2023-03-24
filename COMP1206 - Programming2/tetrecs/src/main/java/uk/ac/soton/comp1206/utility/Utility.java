package uk.ac.soton.comp1206.utility;

import java.io.*;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Utility {
  private static final Logger logger = LogManager.getLogger(Utility.class);

  /**
   * Scores are loaded from scores.txt file.
   * If scores.txt doesn't exist, generates a new one with appointed scores(1000-10000).
   */
  public static ArrayList<Pair<String, Integer>> loadScores() {
    logger.info("Loading scores from scores.txt");
    ArrayList<Pair<String, Integer>> r = new ArrayList<>();
    File f = new File("scores.txt");
    try {
      var bool = f.createNewFile();
      if (bool) {
        for (int i = 0; i < 10; i++) {
          r.add(new Pair("Lukas", 1000 * (10 - i)));
        }
        writeScores(r);
      } else {
        BufferedReader rdr = new BufferedReader(new FileReader(f));
        Scanner sc = new Scanner(rdr);
        while (sc.hasNext()) {
          String[] segments = sc.next().split(":");
          var y = new Pair<>(segments[0], Integer.parseInt(segments[1]));
          r.add(y);
        }
        sc.close();
      }

    } catch (Exception e) {
      logger.error("Unable to read from scores.txt: " + e.getMessage());
      e.printStackTrace();
    }
    return r;
  }

  /**
   * The scores are written in the scores.txt file
   */
  public static void writeScores(List<Pair<String, Integer>> scores) {
    logger.info("Writing scores to scores.txt");
    scores.sort((a, b) -> b.getValue().compareTo(a.getValue()));
    File f = new File("scores.txt");
    try {
      f.createNewFile();
      FileWriter fWriter = new FileWriter(f);
      BufferedWriter bWriter = new BufferedWriter(fWriter);

      for (Pair<String, Integer> i : scores) {
        String s = i.getKey() + ":" + i.getValue() + "\n";
        bWriter.write(s);
      }
      bWriter.close();
      fWriter.close();
    } catch (Exception e) {
      logger.error("Unable to write to scores.txt: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
