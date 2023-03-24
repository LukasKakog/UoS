public class Main {

  public static void main(String[] args) {
    WordGroup plato = new WordGroup(
        "You can discover more about a person in an hour of play than in a year of conversation");
    WordGroup roosevelt = new WordGroup("When you play play hard when you work dont play at all");

    String[] platoArray = plato.getWordArray();
    String[] rooseveltArray = roosevelt.getWordArray();

    for (int i = 0; i < platoArray.length; i++) {
      System.out.println(platoArray[i]);
    }

    for (int j = 0; j < rooseveltArray.length; j++) {
      System.out.println(rooseveltArray[j]);
    }
  }
}
