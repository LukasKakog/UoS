package students;

import bugs.Bug;
import building.Building;

/*
 * implements Student interface so methods can be overridden
 */
public class CyberStudent implements Student {

  private int level;
  private int AP;
  private int CD;

  /**
   * @param level sets the level of the student when the object is created, always 1
   *
   * CyberStudent Constructor
   */
  public CyberStudent(int level) {
    this.level = level;
    this.AP = 7;
    this.CD = 8;
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
   *    the following student has a chance of removing the first bug in the building,
   *    if it doesn't remove the bug, it deals double damage to it.
   * when it's not equal to 1 however, it deals normal damage
   */
  @Override
  public Integer defence(Building building, Team team) {
    int KP = 0;
    int pir = level + 20; //probability of instant removal
    Bug[] allBugs = building.getAllBugs();
    Toolbox myToolbox = new Toolbox();
    if (CD <= 1) {
      if (pir > 50) { //pir has to be capped at 50
        pir = 50;
      }
      int percentage = myToolbox.getRandomInteger(100);
      /*
       * if percentage is smaller or equal to pir,
       * bug is removed from the building and KP is added accordingly
       */
      if (percentage <= pir) {
        KP += (allBugs[0].getLevel() * 20);
        building.removeBug(allBugs[0]);
      } else {
        allBugs[0].damage((Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2))))) * 2);
      }
      System.out.println("CyberStudent used Ultimate");
      CD = 8;
    } else {
      allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
      System.out.println("CyberStudent auto attacked");
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
