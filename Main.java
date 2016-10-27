import java.io.*;
import java.util.ArrayList;

public class Main {
	public static void main(String argv[]) {
    try {
		FileReader file = new FileReader(argv[0]); 
		LexicalAnalyzer result = new LexicalAnalyzer(file);
		file.close();
		PrintWriter writer = new PrintWriter("file.out");
		
        ArrayList <Symbol> list = result.getSymbolTable();
		for (int i=0;i<list.size();i++){
			System.out.println(list.get(i).getValue());
		}
		
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}

