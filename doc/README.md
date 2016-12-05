# fortran-compiler
Command Lines to run without jar file in "more" folder:

jflex LexicalAnalyzer.flex

javac *.java

java Main ../test/exampleLong.alg

Command line to create the jarfile in "more" folder (with windows):

jflex LexicalAnalyzer.flex

javac *.java

jar cvmf META-INF/MANIFEST.MF Part2_Beyens_Nougba.jar *.class

command line to run the jar:

java -jar Part2_Beyens_Nougba.jar "../test/exampleLong.alg" (on windows)
