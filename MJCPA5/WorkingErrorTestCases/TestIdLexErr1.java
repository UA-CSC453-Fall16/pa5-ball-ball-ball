import meggy.Meggy;

class TestIdLexErr1 {
  public static void main(String[] whatever){
    new C().setPix((byte)1);
  }
}

class C {
    public void setPix(byte p@ixel) {
   }
}
