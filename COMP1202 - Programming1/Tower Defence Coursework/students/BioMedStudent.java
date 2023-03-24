package students;

import bugs.Bug;
import building.Building;

public class BioMedStudent implements Student{

  private int level;
  private int AP;
  private int CD;

  /**
   * @param level sets the level of the student when the object is created, always 1
   *
   * BioMedStudent Constructor
   */
  public BioMedStudent(int level){
    this.level = level;
    this.AP = 4;
    this.CD = 9;
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

  @Override
  public Integer upgradeCost() {
    int uC;
    uC = Math.toIntExact(Math.round(100 * Math.pow(2, getLevel())));
    return uC;
  }

  /**
   * when cool down {CD} is equal to 1:
   *    the following student increases the building's construction points by 2.
   * when it's not equal to 1 however, it deals normal damage
   */
  @Override
  public Integer defence(Building building, Team team) {
    int KP = 0;
    Bug[] allBugs = building.getAllBugs();
    if (CD <= 1) {
      building.setConstructionPoints(building.getConstructionPoints() + 2);
      System.out.println("BioMedStudent used Ultimate");
      CD = 8;
    } else {
      allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
      System.out.println("BioMedStudent auto attacked");
      CD -= 1;
    }

    /*
     * if statement used to check if a bug is dead and add the KP accordingly
     */
    if (allBugs[0].getCurrentHp() <= 0) {
      KP += (allBugs[0].getLevel() * 20);
      building.removeBug(allBugs[0]);
    }
    return KP;
  }
}
