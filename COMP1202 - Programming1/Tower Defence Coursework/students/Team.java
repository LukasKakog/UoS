package students;

import building.Building;
import java.util.ArrayList;
import java.util.Iterator;

public class Team {

  private Integer KP;
  private int nmbr;
  private int KPpR;
  ArrayList<Student> students;

  /**
   * @param IKP gets as a parameter from the command line the initial knowledge points
   *            of the team
   *
   * Team constructor
   */
  public Team(int IKP) {
    this.KP = IKP;
    this.KPpR = 0;
    this.nmbr = 0;
    this.students = new ArrayList<>();
  }

  /**
   * @returns the current knowledge points the team has
   */
  public int getKnowledgePoints() {
    return KP;
  }

  /**
   * @returns the current knowledge points the team has gained so far in the round
   */
  public int getKPpR() {
    return KPpR;
  }

  public void setKPpR(int KPpR) {
    this.KPpR = KPpR;
  }

  /**
   * @returns an array with all the students defending the building
   */
  public Student[] getStudents() {
    Student[] Students = new Student[students.size()];
    Iterator<Student> iterator = students.iterator();
    int s = 0;
    while (iterator.hasNext()) {
      Student it0 = iterator.next();
      Students[s] = it0;
      s++;
    }
    return Students;
  }

  /*
   * method which when called, makes all the students use defence() on the building once
   */
  public void studentsAct(Building building) {
    Student[] allStudents = this.getStudents();
    for (int i = 0; i < allStudents.length; i++) {
      if (building.getAllBugs().length > 0) {
        int a = allStudents[i].defence(building, this);
        this.KPpR += a;
      }
    }
    this.KP += this.KPpR;
    this.KPpR = 0;
    System.out.println();
  }

  /**
   * @returns the cost of buying a student
   */
  public int getNewStudentCost() {
    return 100 + (10 * nmbr);
  }

  /*
   * method that recruits a new student
   */
  public void recruitNewStudent() throws Exception {
    Student St;
    Toolbox myToolbox = new Toolbox();
    if (this.KP >= this.getNewStudentCost()) {
      int percentage = myToolbox.getRandomInteger(100);
    //vvv Everytime a new student is recruited, its type is picked randomly
      if (percentage > 0 && percentage < 11) {
        St = new AiStudent(1);
      } else if (percentage > 10 && percentage < 21) {
        St = new CsStudent(1);
      } else if (percentage > 20 && percentage < 31) {
        St = new CyberStudent(1);
      } else if (percentage > 30 && percentage < 41) {
        St = new SeStudent(1);
      } else if (percentage > 40 && percentage < 51) {
        St = new AeroStudent(1);
      } else if (percentage > 50 && percentage < 61) {
        St = new BioMedStudent(1);
      } else if (percentage > 60 && percentage < 71) {
        St = new EconStudent(1);
      } else if (percentage > 70 && percentage < 81) {
        St = new EEEStudent(1);
      } else if (percentage > 80 && percentage < 91) {
        St = new MechStudent(1);
      } else {
        St = new MathStudent(1);
      }
      this.KP -= this.getNewStudentCost();
      students.add(St);
      nmbr++;
    } else {
      throw new Exception("Not enough Knowledge Points");
    }
  }

  /*
   * method that upgrades an existing student if there are enough knowledge points
   */
  public void upgrade(Student student) throws Exception {
    if (this.KP >= student.upgradeCost()) {
      this.KP -= student.upgradeCost();
      student.setLevel(student.getLevel() + 1);
    } else {
      throw new Exception("Not enough Knowledge Points");
    }
  }

}
