import java.io.*;
//import java.io.BufferedReader;
//import java.io.FileReader;

public class ReadFileByLines {

  private static void processLine(int lineNo, String line) {
    // contents of this from RG; rest from Rosettacode.org
    System.out.println(lineNo); 
    System.out.println(line); 
    //System.out.println("hello");
  }

  public static void main(String[] args) {
      for (String filename : args) {
          BufferedReader br = null;
          FileReader fr = null;
          try {
              fr = new FileReader(filename);
              br = new BufferedReader(fr);
              String line;
              int lineNo = 0;
              while ((line = br.readLine()) != null) {
                  processLine(++lineNo, line);
              }
          }
          catch (Exception x) {
              x.printStackTrace();
          }
          finally {
              if (fr != null) {
                  try {br.close();} catch (Exception ignoreMe) {}
                  try {fr.close();} catch (Exception ignoreMe) {}
              }
          }
      }
  }
}
