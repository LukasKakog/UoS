package bugs;

/*
 * this library is imported in order to use the toIntExact, round and pow functions
 */

import java.lang.Math;

public class Bug {

  /*
   * variable declaration
   */
  final String name;
  int baseHp;
  private final int baseSteps;
  int level;

  private int currentHp;
  private int currentSteps;
  private int currentFloor;

  /*
   * Bug constructor No. 1
   */
  public Bug(String name1, int baseHp, int baseSteps, int level) {
    this.name = name1;
    this.baseHp = baseHp;
    this.baseSteps = baseSteps;
    this.level = level;
    this.currentFloor = -1;
  }

  /**
   * @returns amount of steps the bug takes before advancing to the next floor
   */
  public Integer getBaseSteps() {
    return baseSteps;
  }

  /**
   * @returns current level of the bug
   */
  public Integer getLevel() {
    return level;
  }

  /*
   * Bug constructor No. 2
   */

  public Bug(String name, int baseHp, int baseSteps, int level, int initialSteps) {
    this.name = name;
    this.currentHp = Math.toIntExact(Math.round(baseHp * (Math.pow(level, 1.5))));
    this.baseSteps = baseSteps;
    this.level = level;
    this.currentSteps = initialSteps;
    this.currentFloor = -1;
  }

  /**
   * @returns the current Hp of the bug
   */
  public int getCurrentHp() {
    return currentHp;
  }

  /**
   * @returns the remaining steps fot the bug to advance to the next floor
   */
  public int getCurrentSteps() {
    return currentSteps;
  }

  /**
   * @returns in which floor the bug is
   */
  public int getCurrentFloor() {
    return currentFloor;
  }

  /*
   * method that moves the bug one step closer to the next floor
   */
  public void move() {
    if (currentSteps > 0) {
      currentSteps--;
    } else { //if it reaches 0, it goes to the next floor on the following step
      currentFloor++;
      this.currentSteps = this.getBaseSteps() - 1;
    }
  }

  /**
   * @param amount amount of damage that is dealt to the bug
   *               <p>
   *               the amount is removed from the Hp of the bug, if the Hp reaches 0 or becomes
   *               negative, it is set to 0
   */
  public void damage(int amount) {
    if (currentHp > amount) {
      currentHp -= amount;
    } else {
      currentHp = 0;
    }
  }

  /*
   * method that slows down the bug, by adding steps to its current steps, used by SeStudent
   */
  public void slowDown(int steps) {
    this.currentSteps += steps;
  }

}