public class WordGroup {

  String words;

  WordGroup(String input) {
    words = input.toLowerCase();
  }

  String[] getWordArray() {
    return words.split(" ");
  }
}
