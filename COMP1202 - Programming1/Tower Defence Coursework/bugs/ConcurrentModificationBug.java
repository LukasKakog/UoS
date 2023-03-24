package bugs;

public class ConcurrentModificationBug extends Bug {

  /**
   * @param name name of the bug
   * @param initialSteps number of steps the bug has to take
   *                     before reaching floor 0
   * @param level level of the bug
   *
   * ConcurrentModificationBug constructor
   */
  public ConcurrentModificationBug(String name, int level, int initialSteps) {
    super(name, 20,4, level, initialSteps);
  }

}
