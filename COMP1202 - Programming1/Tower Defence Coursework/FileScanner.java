import bugs.Bug;
import bugs.ConcurrentModificationBug;
import bugs.NoneTerminationBug;
import bugs.NullPointerBug;
import java.io.File;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * The FileScanner class uses a scanner to scan through a file of the format "bugs.txt" extracts the
 * info from the file, and adds the bugs scanned to different waves,
 * which will be added to the building
 */
public class FileScanner {

  Scanner scanner;

  /**
   * @param filename inputs from command line the name of the file to be used, has to be bugs.txt
   * @throws Exception in case the file does not exist,
   *                   or in case the input is not correct
   *
   * Constructor that creates a scanner object which returns values from the file
   */
  public FileScanner(String filename) throws Exception {
    if (filename.equals("bugs.txt")) {
      try {
        scanner = new Scanner(new File(filename));
      } catch (Exception e) {
        System.err.println("File not found");
      }
    } else {
      throw new Exception("File format is not correct use bugs.txt file only");
    }
  }

  /**
   * @return the next line of the file
   */
  public String getNext() {
    try {
      return scanner.next();
    } catch (Exception e) {
      e.printStackTrace();
    }
    return "";
  }

  /**
   * @returns an arraylist with all the waves of bugs inside,
   *          each wave is an arraylist which contains the bugs inside
   *
   */
  public ArrayList<ArrayList<Bug>> getBugs() {

    ArrayList<ArrayList<Bug>> bugs = new ArrayList<>();

    while (scanner.hasNext()) {
      ArrayList<Bug> waves = new ArrayList<>();
      /*
       * gets the next line in file and splits it in smaller strings which contain one bug
       */
      String[] line = getNext().split(";");

      /*
       * for each bug in the line Array,
       * it is split again on the opening parenthesis, to get the name of the bug
       * and then on the commas to get the type, level and steps
       *
       * However, instead of using info[0] to get the type,
       * I chose to just use .contains() instead
       * Depending on what the type of the bug is, a new object is created accordingly
       *
       * Finally, the bug is added to the wave,
       * and when all the bugs are added to the wave,
       * the wave is added to the returned arraylist
       */
      for (String bug : line) {
        String[] name = bug.split("\\(");
        String bugName = name[0];
        String[] info = name[1].split(",");
        String bugLevel = info[1];
        String bugSteps = info[2].replace(")", "");

        if (bug.contains("CMB")) {
          ConcurrentModificationBug bug1 = new ConcurrentModificationBug(bugName,
              Integer.parseInt(bugLevel), Integer.parseInt(bugSteps));
          waves.add(bug1);
        } else if (bug.contains("NTB")) {
          NoneTerminationBug bug1 = new NoneTerminationBug(bugName,
              Integer.parseInt(bugLevel), Integer.parseInt(bugSteps));
          waves.add(bug1);
        } else if (bug.contains("NPB")) {
          NullPointerBug bug1 = new NullPointerBug(bugName,
              Integer.parseInt(bugLevel), Integer.parseInt(bugSteps));
          waves.add(bug1);
        }
      }
      bugs.add(waves);
    }
    return bugs;
  }
}
