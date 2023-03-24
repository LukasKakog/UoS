import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;


public class Main {

  public static void main(String[] args) {
    WordGroup plato = new WordGroup("You can discover more about a person in an hour of play than in a year of conversation");
    WordGroup roosevelt = new WordGroup("When you play play hard when you work dont play at all");

    String[] platoArray = plato.getWordArray();
    String[] rooseveltArray = roosevelt.getWordArray();

    for (int i=0; i<platoArray.length; i++) {
      System.out.println(platoArray[i]);
    }

    for (int j=0; j<rooseveltArray.length; j++) {
      System.out.println(rooseveltArray[j]);
    }

    HashSet<String> hashSet = plato.getWordSet(roosevelt);

    for(String element : hashSet) {
      System.out.println(element);
    }

    HashMap<String, Integer> platoHash = plato.getWordCounts();
   Set<String> platoSet = platoHash.keySet();
    for (String key : platoSet) {
      System.out.println(key + ": " + platoHash.get(key));
    }

    System.out.println("===Printing roosevelt WordCounts===");
    HashMap<String, Integer> rooseveltHash = roosevelt.getWordCounts();
    Set<String> rooseveltSet = rooseveltHash.keySet();
    for (String key : rooseveltSet) {
      System.out.println(key + ": " + rooseveltHash.get(key));
    }

    for (String word : hashSet) {
      int wordRepetition = 0;
      for (String platoKey : platoSet) {
        if (word.equals(platoKey)) {
          wordRepetition += platoHash.get(platoKey);
        }
      }
      for (String rooseveltKey : rooseveltSet) {
        if (word.equalsIgnoreCase(rooseveltKey)) {
          wordRepetition += rooseveltHash.get(rooseveltKey);
        }
      }
      System.out.println(word + " " + wordRepetition);
    }
  }

}