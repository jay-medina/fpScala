package TestPackage;


public class GenericTest {

  static <E> void printArray(E[] objArr) {
    for(E i : objArr) {
      System.out.println(i);
    }
  }

  public static void main(String args[]){
    Integer[] intArray = { 1, 2, 3 };
    String[] stringArray = { "Hello", "World" };

    printArray( intArray  );
    printArray( stringArray );

    if(GenericTest.class.getDeclaredMethods().length > 2){
      System.out.println("You should only have 1 method named printArray.");
    }
  }
}
