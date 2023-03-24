import java.util.HashSet;

public class Main {

  public static void main(String[] args) {
    WordGroup plato = new WordGroup(
        "You can discover more about a person in an hour of play than in a year of conversation");
    WordGroup roosevelt = new WordGroup("When you play play hard when you work dont play at all");

    String[] platoArray = plato.getWordArray();
    String[] rooseveltArray = roosevelt.getWordArray();

    HashSet<String> hashSet = plato.getWordSet(roosevelt);

    System.out.println("===Printing Combined Quote HashSet===");
    for (String element : hashSet) {
      System.out.println(element);

    }
  }
}
