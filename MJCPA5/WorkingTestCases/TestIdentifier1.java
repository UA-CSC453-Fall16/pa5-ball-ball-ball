import meggy.Meggy;

class TestIdentifier1 {
  public static void main(String[] whatever){
    new C().setPix((byte)0, (byte)0);
  }
}

class C {
    public void setPix(byte pixel_i, byte pixel_j) {
      Meggy.setPixel(pixel_i, pixel_j, Meggy.Color.RED );
   } 
}
