package building;

import bugs.Bug;
import bugs.ConcurrentModificationBug;
import bugs.NoneTerminationBug;
import bugs.NullPointerBug;
import java.util.ArrayList;
import java.util.Iterator;

public class Building {

  /*
   * variable declaration + arraylist which will contain all the bugs
   */
  private Integer constructionPoints;
  private final Integer topFloor;
  ArrayList<Bug> bugs = new ArrayList<>();

  /**
   * @param constructionPoints input from command line, contains the building's Hp
   * @param topFloor input from the command line, contains the number of floors the building has
   *
   * Building constructor
   */
  public Building(int topFloor, int constructionPoints) {
    this.topFloor = topFloor;
    this.constructionPoints = constructionPoints;
  }

  /**
   * @returns the top floor of the building
   */
  public Integer getTopFloor() {
    return topFloor;
  }

  /**
   * @returns how many construction points the building has left
   */
  public Integer getConstructionPoints() {
    return constructionPoints;
  }

  /**
   * @returns all the bugs currently in the building, sorted in descending order
   */
  public Bug[] getAllBugs() {

    int a = 0;
    for (Bug bug : bugs) {
      if (bug.getCurrentFloor() >= 0) {
        a++; //for each loop used to find the amount of bugs inside the building
      }
    }

    /*
     * initialization of 3 arrays to be used in Selection Sort
     */
    Bug[] Bug = new Bug[a];
    int[] CF = new int[a];
    int[] CS = new int[a];

    /*
     * by iterating through the arraylist,
     * all the bugs currently in the building are stored in an array,
     * and their current Floor and Steps accordingly
     */
    Iterator<Bug> iterator = bugs.iterator();
    int s = 0;
    while (iterator.hasNext()) {
      Bug it0 = iterator.next();
      if (it0.getCurrentFloor() >= 0) {
        Bug[s] = it0;
        CF[s] = it0.getCurrentFloor();
        CS[s] = it0.getCurrentSteps();
        s++;
      }
    }

    /*
     * nested loop used to sort the array,
     * starting from finding the furthest away bug from the top, until the closest one
     */
    for (int i = 0; i < Bug.length; i++) {
      for (int j = 1; j < (Bug.length - i); j++) {
        if (CF[j - 1] < CF[j]) { //compares the current floor of two adjacent bugs in order to find the furthest away from the top
          sS(Bug, CF, CS, j);
        } else if (CF[j - 1] == CF[j]) { //if they are in the same floor,
          if (CS[j - 1] > CS[j]) {       //compare their current steps to find the closest one to the next floor
            sS(Bug, CF, CS, j);
          }
        }
      }
    }
    return Bug;
  }

  /*
   * selection sort algorithm used twice in the getAllBugs() method
   */
  private void sS(Bug[] bug, int[] CF, int[] CS, int j) {
    Bug temp;
    int tempInt;
    temp = bug[j - 1];
    bug[j - 1] = bug[j];
    bug[j] = temp; // switch places of two adjacent bugs
    tempInt = CF[j - 1];
    CF[j - 1] = CF[j];
    CF[j] = tempInt; // switch their current floor accordingly
    tempInt = CS[j - 1];
    CS[j - 1] = CS[j];
    CS[j] = tempInt; // and finally, their current steps
  }

  /**
   * @param bug to be added to the building
   * @returns status of whether the bug has been added or not
   *
   * if the bug is already in the building, -1 is returned
   * otherwise it is added to the building and the size of the array of bugs is returned
   */
  public int addBug(Bug bug) {
    int stat = 0;
    boolean e = false; //boolean that represents whether @param bug is already in the building

    /*
     * iterates through the arraylist with all the bugs,
     * comparing @param bug to all the bugs in the building (also in floor -1)
     */
    Iterator<Bug> iterator = bugs.iterator();
    while (iterator.hasNext()) {
      Bug it1 = iterator.next();
      if (it1.equals(bug)) {
        e = true;
      }
    }

    if (e) {
      stat = -1;
    } else {
      bugs.add(bug); //bug is added if e is false
      stat = bugs.size();
    }

    return stat;

  }

  /**
   * @param bug is the bug to be removed of the building
   */
  public void removeBug(Bug bug) {
    bugs.remove(bug);
  }

  /*
   * method that moves all the bugs in the building one step closer to the top
   */
  public void bugsMove() {

    for (int i = 0; i < bugs.size(); i++) {
      bugs.get(i).move();

      /*
       * if the bug reaches the top floor,
       * it deals a specific amount of damage depending on its type, and it is removed
       */
      if (this.getTopFloor() == bugs.get(i).getCurrentFloor()) {
        if (bugs.get(i) instanceof NoneTerminationBug) {
          this.constructionPoints -= 4;
        } else if (bugs.get(i) instanceof ConcurrentModificationBug) {
          this.constructionPoints -= 2;
        } else if (bugs.get(i) instanceof NullPointerBug) {
          this.constructionPoints -= 1;
        }
        removeBug(bugs.get(i));
        i--;
      }
    }

    System.out.println();
    System.out.println("  Bugs moved"); //used for debugging
    System.out.println();

  }

  /*
   *
   */
  public void setConstructionPoints(int CP){
    this.constructionPoints = CP;
  }
}
