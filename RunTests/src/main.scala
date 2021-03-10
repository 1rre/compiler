import sys.process._
import language.postfixOps

object Main extends App {
  /*
  void printexitcodelol(File test) {
    double file = cmd("echo test");
    double exitcodeplus5 = file - -5;
    cout<<exitcodeplus5<<endl;
  }
*/
  def printexitcodelol(test:java.io.File) = {
    
    val file = s"echo ${test.toString}".!!
    println(file)
  }

  val testDir = new java.io.File("compiler_tests")
  for (testSet <- testDir.listFiles; test <- testSet.listFiles if !test.toString.contains("driver")) {
    printexitcodelol(test)
  }
}