import java.util.ArrayList;

import bugs.Bug;
import building.Building;
import students.Team;

public class EcsBuildingDefence {

  /**
   *
   * @param args inputs from command line
   *             args[0] - top floor of the building
   *             args[1] - construction points of the building
   *             args[2] - name of the file to be scanned
   *             args[3] - Initial knowledge points of the team
   * @throws Exception in case the file does not exist,
   *                   or in case the input is not correct
   */
  public static void main(String[] args) throws Exception{

    int waveNo = 0;
    int tf = Integer.parseInt(args[0]);
    int cp = Integer.parseInt(args[1]);
    Team team = new Team(Integer.parseInt(args[3]));
    Building building = new Building(tf, cp);
    Battle battle = new Battle(team, building);
    FileScanner scanner = new FileScanner(args[2]);
    ArrayList<ArrayList<Bug>> bugs = scanner.getBugs();

    while (true) {

      boolean status = !bugs.isEmpty();
      ArrayList<Bug> wave;

      /*
       * if there is a next wave of bugs, they are added to the building
       * otherwise the program is terminated
       */
      if (status) {
        wave = bugs.get(0);
        bugs.remove(0);
        for (Bug b : wave) {
          building.addBug(b);
        }
        waveNo++;
      } else {
        break;
      }

      System.out.println();
      System.out.println(" WAVE NUMBER : " + waveNo);

      /*
       * for loop so there can be (8 * topFloor) steps
       */
      for (int i = 0; i < (tf * 8); i++) {
        battle.step();
        System.out.println("----------------- step " + (i + 1) + " completed -----------------");
        /*
         * this if statement is used so there is no need
         * for extra empty steps in case the bugs are killed earlier.
         * So i has to be larger than the size of the wave to
         * make sure all the bugs are in the building before it considers
         * termination.
         */
        if ((building.getAllBugs().length == 0) && (i > wave.size()) || (
            building.getConstructionPoints() <= 0)) {
          i = (tf * 8);
        }
      }

      if ((bugs.size() == 0) && (building.getAllBugs().length == 0) || (
          building.getConstructionPoints() <= 0)) {
        break;
      }
    }
  }
}
