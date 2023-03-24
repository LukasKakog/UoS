package bugs;

public class NullPointerBug extends Bug{

  /**
   * @param name name of the bug
   * @param initialSteps number of steps the bug has to take
   *                     before reaching floor 0
   * @param level level of the bug
   *
   * NullPointerBug constructor
   */
  public NullPointerBug(String name, int level, int initialSteps) {
    super(name, 10, 2, level, initialSteps);
  }


}