package students;

import building.Building;

public interface Student {

  /*
   * we define the Student as an interface in order to achieve maximum abstraction,
   * the following methods are overridden into all student classes that implement the interface,
   * each one with a different block of code.
   */

  /**
   * @returns current level, is the same in all student classes
   */
  Integer getLevel();

  Integer getCD();

  Integer getAP();

  /*
   * method created to facilitate upgrade() in the Team class
   */
  void setLevel(int level);

  void setCD(int CD);

  void setAP(int AP);

  /**
   * @returns upgrade cost, is the same in all student classes
   */
  Integer upgradeCost();

  /**
   * @param building the object building which students will defend
   * @returns the knowledge points gained in case a bug is killed
   *          otherwise returns 0
   *
   * method used for the student to attack the bugs in the building,
   * differs from student to student
   */
  Integer defence(Building building, Team team);


}

