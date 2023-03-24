package students;

import bugs.Bug;
import building.Building;

public class MechStudent implements Student{

  private int level;
  private int AP;
  private int CD;

  /**
   * @param level sets the level of the student when the object is created, always 1
   *
   * MechStudent Constructor
   */
  public MechStudent(int level) {
    this.level = level;
    this.AP = 5;
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
   *    the following student decreases every other student's CD by 1,
   *    if that student is ready to use his ultimate, it just gets + 1 attack power instead.
   * when it's not equal to 1 however, it deals normal damage
   */
  @Override
  public Integer defence(Building building, Team team) {
    int KP = 0;
    Bug[] allBugs = building.getAllBugs();
    Student[] allStudents = team.getStudents();
    if (CD <= 1) {
      for (Student student : allStudents) {
        if (student.getCD() > 1)
          student.setCD(student.getCD() - 1);
        else
          student.setAP(student.getAP() + 1);
      }
      System.out.println("MechStudent used Ultimate");
      CD = 7;
    } else {
      allBugs[0].damage(Math.toIntExact(Math.round(this.AP * (Math.pow(level, 1.2)))));
      System.out.println("MechStudent auto attacked");
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
