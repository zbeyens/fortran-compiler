# fortran-compiler
Command Lines to run without jar file in "more" folder:
jflex LexicalAnalyzer.flex
javac *.java
java Main ../test/exampleLong.alg

Command line to create the jarfile:
in doc:
jflex LexicalAnalyzer.flex
javac Main.java
java Main sourceFile
jar cvmf META-INF/MANIFEST.MF uberFortran.jar *.class

command line to run the jar:

java -jar uberFortran.jar "../test/factorielle.alg" (on windows)
