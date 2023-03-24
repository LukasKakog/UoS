import java.util.HashMap;
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

  HashMap<String, Integer> getWordCounts() {
    HashMap<String, Integer> map = new HashMap<String, Integer>();
    String[] words = getWordArray();

    for (int i=0; i<words.length; i++) {
      if (map.containsKey(words[i])) {
        map.put(words[i], map.get(words[i]) + 1);
      } else {
        map.put(words[i], 1);
      }
    }

    return map;
  }

}
