import bugs.Bug;
import building.Building;
import students.Student;
import students.Team;

public class Battle {

  private final Team team;
  private final Building building;

  /**
   * @param building building which the bugs will attack
   * @param team team of students to defend the building
   *
   * Battle constructor
   */
  public Battle(Team team, Building building) {
    this.team = team;
    this.building = building;
  }

  /*
   * this method is the intelligence of the game,
   * it includes all the decision-making.
   * I decided to buy students until the team has 6 of them,
   * and then start upgrading them
   */
  public void manageTeam() {
    /*
     * try-catch block is mandatory
     * since recruitNewStudent() and upgrade(Student student) both throw exceptions,
     * but manageTeam shouldn't throw any.
     */
    try {
      Student[] studs = this.team.getStudents();
      if (studs.length <= 5) {
        if (this.team.getKnowledgePoints() >= this.team.getNewStudentCost()) {
          this.team.recruitNewStudent();
        }
      } else {
        for (Student stud : studs) {
          if (this.team.getKnowledgePoints() >= stud.upgradeCost()) {
            this.team.upgrade(stud);
          }
        }
      }
    } catch (Exception ignored) {
    }
  }

  /*
   * this method is where the actual battle happens,
   * every time step() is called,
   * 1- manageTeam is called, to buy/upgrade students
   * 2- all the bugs in the building move one step closer to the top
   * 3- if there are bugs in teh building, all the students defend once
   * 4- the information on the round is displayed, used for debugging
   */
  public void step() {
    try {
      Thread.sleep(500);
      //code here vvv
      this.manageTeam();
      this.building.bugsMove();
      this.team.studentsAct(this.building);
      this.printInfo();
      //code here ^^^
    } catch (InterruptedException e) {
    }
  }

  /*
   * this method prints all the stats of the team, the students, the building and the bugs
   * mainly used for debugging.
   */
  public void printInfo() {
    System.out.println(this.team.getKnowledgePoints() + " knowledge points available");
    System.out.println("current recruiting cost: " + this.team.getNewStudentCost());
    System.out.println();
    Student[] stds = this.team.getStudents();
    for (int i = 0; i < stds.length; i++) {
      System.out.println("student: " + stds[i]);
      System.out.println("  AP: "+ stds[i].getAP());
      System.out.println("  CD: "+ stds[i].getCD());
      System.out.println("  level: " + stds[i].getLevel());
      System.out.println("  upgrade Cost: " + stds[i].upgradeCost());
      System.out.println();
    }
    System.out.println("** " +
        this.building.getConstructionPoints() + " construction points currently left **");
    System.out.println();
    Bug[] bg = this.building.getAllBugs();
    if (bg.length != 0) {
      for (int i = 0; i < bg.length; i++) {
        System.out.println("bug: " + bg[i]);
        System.out.println("  level: " + bg[i].getLevel());
        System.out.println("  current HP: " + bg[i].getCurrentHp());
        System.out.println("  current floor: " + bg[i].getCurrentFloor());
        System.out.println("  " + bg[i].getCurrentSteps() + "  steps left until the next floor");
        System.out.println();
      }
    }
    System.out.println(bg.length + " bugs in the building");
  }
}

