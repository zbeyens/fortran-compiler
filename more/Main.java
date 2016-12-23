import java.io.*;

public class Main {
	
	private static String nameOut(String input){
		if (input.contains(".")){
			input=input.substring(0, input.lastIndexOf('.'));
		}
		input=input+".ll";
		return input;
	}
	public static void main(String argv[]) {
		try {
			int len = argv.length;
			if (len < 2){
				LexicalAnalyzer.main(argv);
				String fileNameOut = nameOut(argv[0]);
				Parser p = new Parser(fileNameOut);
				p.parse();
			}
			else if (len < 1){
				System.out.println("no inputfile");
			}
			else{
				System.out.println("too many arguments");
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
