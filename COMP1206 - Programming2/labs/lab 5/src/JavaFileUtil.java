//libraries imported
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.io.ConcatenateJavaFiles;

public class JavaFileUtil implements ConcatenateJavaFiles {

  //Overridden method from implementing ConcatenateJavaFiles interface
  @Override
  public void concatenateJavaFiles(String dirName, String fileName) throws IOException {

    //try-catch block in case directory given is not a directory
    try {

      //create new file/directory instance
      File dirP = new File(dirName);

      //appropriate exception thrown in case it is not a directory
      if(!dirP.isDirectory()) {
        throw new IllegalArgumentException("Argument not a Directory");
      }

      //create new file instance with pathname
      File p = new File(dirName + File.separator + fileName);

      //creation of a new array of files within the given directory that end in .java
      File[] jFs = dirP.listFiles((dir, name) -> name.endsWith(".java"));

      //open OutputStream
      BufferedWriter buffWriter = new BufferedWriter(new FileWriter(p));

      //for-each loop; the content of every file in the array is written in a file "p"
      for (File javaFile : jFs) {

        //open inputStream
        FileReader fReader = new FileReader(javaFile);
        BufferedReader bReader = new BufferedReader(fReader);

        String l;

        //while the next line on the file is not null, it is written in the file "p"
        while ((l = bReader.readLine()) != null) {
          buffWriter.write(l);
          buffWriter.write("\n");
        }

        //close inputStream
        bReader.close();
      }

      //close outputStream
      buffWriter.close();
    }
    catch (IllegalArgumentException exc) {
      System.out.println("Argument not a Directory");
      throw exc;
    }
  }
}
