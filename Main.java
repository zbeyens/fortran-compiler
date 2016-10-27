import java.io.*;

public class Main {
  static public void main(String argv[]) {
    try {
      LexicalAnalyzer.main(new FileReader(argv[0]));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
