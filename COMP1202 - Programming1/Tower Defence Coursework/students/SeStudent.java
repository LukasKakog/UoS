package students;

import bugs.Bug;
import building.Building;

/*
 * implements Student interface so methods can be overridden
 */
public class SeStudent implements Student {

  private int level;
  private int AP;
  private int CD;

  /**
   * @param level sets the level of the student when the object is created, always 1
   *
   * SeStudent Constructor
   */
  public SeStudent(int level) {
    this.level = level;
    this.AP = 5;
    this.CD = 6;
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
   *    the following student uses slowDown on the 5 closest bugs to the top,
   * when it's not equal to 1 however, it deals normal damage
   */
  @Override
  public Integer defence(Building building,Team team) {
    int KP = 0;
    Bug[] allBugs = building.getAllBugs();
    if (CD <= 1) {
      if ((allBugs.length < 5) && (allBugs.length != 0)) {
        for (int i = 0; i < allBugs.length; i++) {
          allBugs[i].slowDown(2);
        }
      } else {
        for (int i = 0; i < 5; i++) {
          allBugs[i].slowDown(2);
        }
      }
      System.out.println("SeStudent used Ultimate");
      CD = 6;
    } else {
      allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
      System.out.println("SeStudent auto attacked");
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
