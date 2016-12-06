import meggy.Meggy;

class TestInvArgTypeErr1{
  public static void main (String[] whatever){
    new Foo().setPx( (byte)0, (byte)0, Meggy.Color.RED);
  }
}
class Foo {
  public void setPx(byte a, byte b, int c){
    Meggy.setPixel(a,b,c);

  }
}
