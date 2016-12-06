import meggy.Meggy;

class TestThis1{
  public static void main (String[] whatever){
    new Bar().count((byte)0);
  }
}

class Bar {
  public void count(byte p){

    if(p < 2){
      this.count((byte)(p + (byte)1));
    }
    Meggy.setPixel(p, (byte)0, Meggy.Color.BLUE);
  }
}
