package bugs;

public class NoneTerminationBug extends Bug{

  /**
   * @param name name of the bug
   * @param initialSteps number of steps the bug has to take
   *                     before reaching floor 0
   * @param level level of the bug
   *
   * NoneTerminationBug constructor
   */
  public NoneTerminationBug(String name, int level, int initialSteps) {
    super(name, 200, 6, level, initialSteps);
  }

}
