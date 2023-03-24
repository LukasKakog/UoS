class GuessingGame{
	public static void main(String Args[]){
		int numberToGuess, guessedNumber;//declared integers
		Toolbox myToolbox = new Toolbox();//declared object myToolbox

		System.out.println("Welcome to the guessing Game!");// welcome :)
		numberToGuess = myToolbox.getRandomInteger(10);//sets a random number to be guessed from Toolbox
		guessedNumber = myToolbox.readIntegerFromCmd();//reads input from cmd user
		if (numberToGuess<guessedNumber){
			System.out.println("too high");
		}//if the number guessed is higher than the number to be guessed, a message is displayed saying it's too high
		else if (numberToGuess>guessedNumber){
			System.out.println("too low");
		}//if the number guessed is lower than the number to be guessed, a message is displayed saying it's too low
		else
			System.out.println("right");
	}
}