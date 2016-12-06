import meggy.Meggy;

class TestMethods2{
  public static void main (String[] whatever){
    new Bar().setPx( (byte)1, (byte)1, Meggy.Color.BLUE, (byte)1, 10, (byte)1, 10, (byte)1, 10);
  }
}

class Bar {
  public void setPx(byte a, byte b, Meggy.Color c, byte d, int e, byte f, int g, byte h, int i){
    Meggy.setPixel(a,b,c);
  }
}
