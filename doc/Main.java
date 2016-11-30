import java.io.*;

public class Main {
public static void main(String argv[]) {
    try {
        LexicalAnalyzer.main(argv);
        Parser p = new Parser();
        p.parse();
    } catch (Exception e) {
        e.printStackTrace();
    }
}
}
