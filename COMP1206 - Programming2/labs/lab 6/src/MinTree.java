import uk.ac.soton.ecs.comp1206.labtestlibrary.datastructure.Tree;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.MinimumInTree;

public class MinTree implements MinimumInTree {

  /*
   * Auxiliary function that compares two nodes in a tree and returns the smallest one
   */
  public Tree findMinTree(Tree t1, Tree t2){
    if (t1.getVal() < t2.getVal()){
      return t1;
    } else {
      return t2;
    }
  }

  /*
   * Recursive function
   */
  public Tree findMinAux(Tree t){

    /*
     * Base Case: Tree has a single node, return the node
     * However, it's also used to compare a node to all the nodes in its subtree
     */
    if (t.left() == null && t.right() == null){
      return t;
    } else {

      /*
       * Three different cases, the node contains:
       * One left child
       * One right child
       * Both children
       */
      if ((t.left() != null) && (t.right() == null)){

        /*
         * Return smallest value between current node and its subtree
         */
        return findMinTree(t, findMinAux(t.left()));
      } else if ((t.left() == null) && (t.right() != null) ){

        /*
         * Return smallest value between current node and its subtree
         */
        return findMinTree(t, findMinAux(t.right()));
      } else {

        /*
         * Return smallest value between current node, and smallest value between both of its children
         */
        return findMinTree(t, findMinTree(findMinAux(t.left()), findMinAux(t.right())));
      }
    }
  }

  /*
   * Recursive function is called, value of the node returned is returned
   */
  @Override
  public int findMin(Tree tree) {
      return findMinAux(tree).getVal();
  }
}
