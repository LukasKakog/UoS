import java.util.HashSet;

public class WordGroup {

  String words;

  WordGroup(String input) {
    words = input.toLowerCase();
  }

  String[] getWordArray() {
    return words.split(" ");
  }

  HashSet<String> getWordSet(WordGroup group) {

    HashSet<String> wordSet = new HashSet<String>();

    String[] thisWords = this.getWordArray();
    String[] parameterWords = group.getWordArray();

    for (int i=0; i<thisWords.length; i++) {
      wordSet.add(thisWords[i]);
    }

    for (int j=0; j<parameterWords.length; j++) {
      wordSet.add(parameterWords[j]);
    }

    return wordSet;
  }
}
