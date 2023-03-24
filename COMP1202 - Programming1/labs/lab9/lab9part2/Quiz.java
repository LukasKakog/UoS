package lab9part2;

import java.util.ArrayList;

public class Quiz {
	FlashCardReader cardReader;
	ArrayList<FlashCard> cards;
	int score = 0;
	int questionsAtt = 0;
	boolean saveAns;
	ArrayList<String> userAns;
	
	public static void main(String args[]) throws Exception {
		Quiz quiz = new Quiz();
	}
	
	public void play() {
		String input;
		userAns = new ArrayList<String>();
		
		for (int i=0; i<cards.size(); i++) {
			questionsAtt++;
			System.out.println("Question" + (i+1));
			System.out.println(cards.get(i).getQuestion());
			Toolbox myToolbox = new Toolbox();
			input = myToolbox.readStringFromCmd();
			
			if(input.equals(cards.get(i).getAnswer())) {
				System.out.println("Right");
				score++;
				userAns.add(cards.get(i).getQuestion() + "," + input + ",right");
			} else {
				System.out.println("Wrong. Actual answer:" + cards.get(i).getAnswer());
				userAns.add(cards.get(i).getQuestion() + "," + input + ",wrong");
			}
		}
	}
	
	public Quiz() {
		cardReader = new FlashCardReader();
		cards = cardReader.getFlashCard();
		play();
	}

}
