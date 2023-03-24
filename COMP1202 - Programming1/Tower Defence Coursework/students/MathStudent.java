package students;

import bugs.Bug;
import building.Building;

public class MathStudent implements Student{

  private int level;
  private int AP;
  private int CD;

  /**
   * @param level sets the level of the student when the object is created, always 1
   *
   * MathStudent Constructor
   */
  public MathStudent(int level){
    this.level = level;
    this.AP = 6;
    this.CD = 5;
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
   *    the following student deals damage to the bug based on their HP (percentage)
   *    if the bugs Hp however is less than 100, it deals normal damage and slows them down
   * when it's not equal to 1 however, it deals normal damage
   */
  @Override
  public Integer defence(Building building, Team team) {
    int KP = 0;
    Bug[] allBugs = building.getAllBugs();
    if (CD <= 1) {
      if (allBugs[0].getCurrentHp() > 100) {
        allBugs[0].damage(Math.toIntExact(Math.round((allBugs[0].getCurrentHp() * this.AP) / 100)));
      } else {
        allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
        allBugs[0].slowDown(3);
      }
      System.out.println("MathStudent used Ultimate");
      CD = 5;
    } else {
      allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
      System.out.println("MathStudent auto attacked");
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
