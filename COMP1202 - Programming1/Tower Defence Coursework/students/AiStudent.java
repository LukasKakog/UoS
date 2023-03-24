package students;

import bugs.Bug;
import building.Building;

/*
 * implements Student interface so methods can be overridden
 */
public class AiStudent implements Student {

  private int level;
  private int AP;
  private int CD;

  /**
   * @param level sets the level of the student when the object is created, always 1
   *
   * AiStudent Constructor
   */
  public AiStudent(int level) {
    this.level = level;
    this.AP = 7;
    this.CD = 7;
  }

  @Override
  public Integer getLevel() {
    return level;
  }

  @Override
  public Integer getCD() {
    return CD;
  }

  @Override
  public Integer getAP() {
    return AP;
  }

  @Override
  public Integer upgradeCost() {
    int uC;
    uC = Math.toIntExact(Math.round(100 * Math.pow(2, getLevel())));
    return uC;
  }

  /**
   * when cool down {CD} is equal to 1:
   *    the following student attacks the 3 closest bugs to the top,
   * when it's not equal to 1 however, it deals normal damage
   */
  @Override
  public Integer defence(Building building, Team team) {
    int KP = 0;
    Bug[] allBugs = building.getAllBugs();
    if (CD <= 1) {
      /*
       * if statement used in case there aren't three bugs in the building
       */
      if ((allBugs.length < 3) && (allBugs.length != 0)) {
        for (int i = 0; i < allBugs.length; i++) {
          allBugs[i].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
        }
      } else {
        for (int i = 0; i < 3; i++) {
          allBugs[i].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
        }
      }
      System.out.println("AiStudent used Ultimate");
      CD = 7;
    } else {
      allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
      System.out.println("AiStudent auto attacked");
      CD -= 1;
    }

    /*
     * if statement used to check if a bug is dead and adds the KP accordingly
     */
    if (allBugs[0].getCurrentHp() <= 0) {
      KP += (allBugs[0].getLevel() * 20);
      building.removeBug(allBugs[0]);
    }
    return KP;
  }

  @Override
  public void setLevel(int level) {
    this.level = level;
  }

  @Override
  public void setCD(int CD) {
    this.CD = CD;
  }

  @Override
  public void setAP(int AP) {
    this.AP = AP;
  }

}
