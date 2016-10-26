import java.io.*;

public class Main {
  static public void main(String argv[]) {
    try {
      new LexicalAnalyzer(new FileReader(argv[0]));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
