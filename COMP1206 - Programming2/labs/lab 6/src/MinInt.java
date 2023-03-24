import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.MinimumInArray;

public class MinInt implements MinimumInArray {

  /*
   * Recursive function that starts from the last element of an array
   * and compares all numbers until index becomes negative.
   * The smallest value is the output
   */
  public int findMinAux(int index, int[] arr, int min){
    if (index < 0){
      return min;
    } else {
      min = Math.min(arr[index],min);
      return findMinAux(index - 1, arr, min);
    }
  }

  /*
   * Checking base cases, length of array is 1 or 2
   * Call recursive function if array is larger
   */
  @Override
  public int findMin(int[] arr) {
    if (arr.length == 1){
      return arr[0];
    } else if (arr.length == 2){
      return Math.min(arr[0], arr[1]);
    } else {
      return findMinAux(arr.length - 2, arr, arr[arr.length - 1]);
    }
  }
}
