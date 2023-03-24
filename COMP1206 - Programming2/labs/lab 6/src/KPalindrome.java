import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.PalindromeChecker;

public class KPalindrome implements PalindromeChecker {

  @Override
  public boolean isKPalindrome(String word, int j) {

    String reverseWord = new StringBuilder(word).reverse().toString();
    int wordLength = word.length();

    return (KPalAux(word, reverseWord, wordLength, wordLength) <= j * 2);
  }

  public int KPalAux(String input, String reverseInput, int a, int b){
    if (a == 0){
      return b;
    } else if (b == 0){
      return a;
    } else if (input.charAt(a - 1) == reverseInput.charAt(b - 1)) {
      return KPalAux(input, reverseInput, a - 1, b - 1);
    }
    return 1 + Math.min(KPalAux(input, reverseInput, a - 1, b), KPalAux(input, reverseInput, a, b - 1));
  }
}
