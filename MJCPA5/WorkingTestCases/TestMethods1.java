import meggy.Meggy;

class TestMethods1{
  public static void main (String[] whatever){
    new Bar().setPx1( (byte)1, (byte)1, Meggy.Color.BLUE);
    new Bar().setPx2( (byte)0, (byte)0, Meggy.Color.RED );
  }
}

class Bar {
  public void setPx1(byte a, byte b, Meggy.Color c){
    Meggy.setPixel(a,b,c);
  }
  public void setPx2(byte d, byte f, Meggy.Color g){
    Meggy.setPixel(d,f,g);
  }
}
