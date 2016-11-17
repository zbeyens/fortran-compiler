import java.io.*;

public class Main {
	

	
	private static String nameOut(String input){
		if (input.contains(".")){
			input=input.substring(0, input.lastIndexOf('.'));
		}
		input=input+".out";
		return input;
	}
	public static void main(String argv[]) {
		try {
				for (int i=0;i<argv.length;i++){
					String argv2[]={argv[i]};
					LexicalAnalyzer.name=nameOut(argv[i]);
					LexicalAnalyzer.main(argv2);
				}	
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}


