//libraries imported
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.io.RandomIO;

public class RandomNumberWriter implements RandomIO {

  private Random random;

  //Constructor
  public RandomNumberWriter(long rn){
    random = new Random(rn);
  }

  //Overridden methods from implementing RandomIO interface
  @Override
  public void writeRandomChars(String p) throws IOException {

    //creation of a new file instance
    var f = new File(p);
    //FileWriter object created given a file object "f"
    var out = new FileWriter(f);

    //for loop to go through the process of generating a random number 10000 times
    for (int i = 0;i <10000; i++){

      //random number between 0 and 100000(bound is exclusive) is generated
      int number = random.nextInt(100000);

      //random number stored in a String + new line
      String str = number + "\n";

      //number written in the file
      out.write(str);
    }

    //close the stream
    out.close();
  }

  @Override
  public void writeRandomByte(String p) throws IOException {

    //creation of a new file instance
    var f = new File(p);

    //byteStream opened
    var fOutput = new FileOutputStream(f);
    var dOutput = new DataOutputStream(fOutput);

    //for loop to go through the process of generating a random number 10000 times
    for (int i = 0;i <10000; i++){

      //random number between 0 and 100000(bound is exclusive) is generated
      int number = random.nextInt(100000);

      //number written in the file
      dOutput.writeInt(number);
    }

    //close the stream
    dOutput.close();
  }
}
