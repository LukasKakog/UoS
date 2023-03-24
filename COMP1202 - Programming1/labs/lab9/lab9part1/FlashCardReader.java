import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class FlashCardReader {
	
	BufferedReader reader;
	
	FlashCardReader() {
		try {
			this.reader = new BufferedReader(new FileReader("Questions.txt"));
		} catch (FileNotFoundException e) {
			System.out.println("Error accessing file Questions.txt");
			e.printStackTrace();
		}
	}
	
	public String getLine() {
		String currentLine;
		
		try {
			if ((currentLine = reader.readLine()) != null) {
				return currentLine;
			}
		} catch (IOException e) {
			System.out.println("Error reading from file Questions.txt");
			e.printStackTrace();
		}
		
		return "";
	}
	
	
	public boolean isFileReady() {
		try {
			return reader.ready();
		} catch (IOException e) {
			System.out.println("Error accessing file Questions.txt");
			e.printStackTrace();
		}
		
		return false;
	}
	