import static org.junit.jupiter.api.Assertions.*;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

public class DAGSortTest {

  @Test
  // Testing if a given DAG is null
  public void NullDAGTest() {
    assertThrows(NullPointerException.class, () -> DAGSort.sortDAG(null), "Given DAG is null");
  }

  @Test
  // Testing if a given DAG is empty
  public void EmptyDAGTest() {
    boolean testRs = false;
    int[] c = null;

    int[][] g = new int[][] {};
    int[][] exp = new int[][] {};

    try {
      c = DAGSort.sortDAG(g);
    } catch (Exception ignored) {
    }

    if (c.length == exp.length) {
      testRs = true;
    }

    assertTrue(testRs);
  }

  @Test
  // Testing if the output of a given DAG is correct
  public void DAGOutputTest() {
    boolean testRs = false;
    int[] c = null;

    int[][] g = new int[6][];
    g[0] = new int[] {};
    g[1] = new int[] {};
    g[2] = new int[] {3};
    g[3] = new int[] {0};
    g[4] = new int[] {1, 1};
    g[5] = new int[] {2, 0};
    try {
      c = DAGSort.sortDAG(g);
    } catch (Exception ignored) {
    }

    int[][] exp = new int[13][];
    exp[0] = new int[] {3, 2, 5, 4, 0, 1};
    exp[1] = new int[] {4, 5, 0, 3, 2, 1};
    exp[2] = new int[] {1, 2, 3, 5, 0, 4};
    exp[3] = new int[] {4, 3, 2, 5, 1, 0};
    exp[4] = new int[] {1, 3, 2, 4, 0, 5};
    exp[5] = new int[] {5, 2, 4, 3, 1, 0};
    exp[6] = new int[] {2, 3, 4, 0, 5, 1};
    exp[7] = new int[] {5, 1, 4, 3, 2, 0};
    exp[8] = new int[] {5, 2, 4, 3, 1, 0};
    exp[9] = new int[] {5, 1, 0, 4, 3, 2};
    exp[10] = new int[] {0, 2, 3, 4, 5, 1};
    exp[11] = new int[] {5, 4, 2, 3, 0, 1};
    exp[12] = new int[] {5, 4, 2, 3, 1, 0};
    for (int[] expArray : exp) {
      if (Arrays.equals(expArray, c)) {
        testRs = true;
        break;
      }
    }
    assertTrue(testRs);
  }

  @Test
  // Testing that the given DAG has no node bigger than its size
  public void OutOfBoundsNodeTest() {
    int[][] g = new int[3][];
    g[0] = new int[] {1, 2};
    g[1] = new int[] {2};
    g[2] = new int[] {3};

    assertThrows(
        InvalidNodeException.class,
        () -> DAGSort.sortDAG(g),
        "Nodes of given DAG must be between 0 and N - 1");
  }

  @Test
  // Testing if the given DAG doesn't have negative values
  public void NegativeNodeTest() {
    int[][] g = new int[3][];
    g[0] = new int[] {1, 2};
    g[1] = new int[] {2};
    g[2] = new int[] {-1}; //Negative value inserted

    assertThrows(
        InvalidNodeException.class,
        () -> DAGSort.sortDAG(g),
        "Nodes of given DAG must not be negative");
  }

  @Test
  // Testing that the given DAG is not a singleton, and therefore a cyclic graph
  public void SingletonDAGTest() {
    int[][] singletonG = new int[1][];
    singletonG[0] = new int[] {0};

    assertThrows(
        CycleDetectedException.class,
        () -> DAGSort.sortDAG(singletonG),
        "Cyclic graphs are not allowed");
  }

  @Test
  // Testing that the given DAG is not a cyclic graph
  public void CyclicGraphTest() {
    int[][] cyclicG = new int[3][];
    cyclicG[0] = new int[] {1, 2};
    cyclicG[1] = new int[] {2};
    cyclicG[2] = new int[] {2};

    assertThrows(
        CycleDetectedException.class,
        () -> DAGSort.sortDAG(cyclicG),
        "Cyclic graphs are not allowed");
  }

  @Test
  // Testing a node from a given DAG with multiple children, no errors should be thrown
  public void MultipleChildrenTest() {
    int[] c = null;
    boolean testRs = false;

    int[][] g = {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}};
    try {
      c = DAGSort.sortDAG(g);
    } catch (Exception ignored) {
    }

    if (c[0] == 0) {
      testRs = true;
    }
    assertTrue(testRs);
  }
}