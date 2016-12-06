import meggy.Meggy;

class TestReturnType {
  public static void main(String[] whatever){
    new C().setPix((byte)1,(byte)1);
  }
}

class C {
    public byte setPix(byte pixeli, byte pixelj) {
      Meggy.setPixel(pixeli, pixelj, Meggy.Color.RED );
     return pixeli;
   } 
}
