package TestPackage;

import java.util.Scanner;

public class Test {

  String name;

  public Test(String personName) {
    name = personName;
  }

  public String greet(String yourName) {
    return String.format("Hi %s, my name is %s", name, yourName);
  }

  public static void main(String[] args) {
    Test t = new Test("Jose");

    System.out.println(t.greet("bob"));

    int i = 4;
    double d = 4.0;
    String s = "HackerRank ";

    Scanner scan = new Scanner(System.in);

    int temp = Integer.parseInt(scan.nextLine());
    double temp2 = Double.parseDouble(scan.nextLine());
    String s2 = scan.nextLine();

    System.out.println(i + temp);
    System.out.println(d + temp2);
    System.out.println(s + s2);
  }
}
