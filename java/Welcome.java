import corejava.*;

public class Welcome { public static void main(String[] args)

  { 
    String[] greeting = new String[3];
    greeting[0] = "Welcome to java";
    greeting[1] = "rwg";
    greeting[2] = "testing";
    int i, nmax = Console.readInt("Number of iterations");

    for (i = 0; i < greeting.length*nmax; i++ ) 
       System.out.println(greeting[i%greeting.length]);
    
    
  }
}
