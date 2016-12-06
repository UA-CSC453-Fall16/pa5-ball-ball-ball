import meggy.Meggy;

class TestIdColorTypeErr {
  public static void main(String[] whatever){
    new C().setPix((byte)1,(byte)1,Meggy.Color.RED);
  }
}

class C {
    public void setPix(byte pixel_i_1, byte pixel_j_1, int RED){

      Meggy.setPixel(pixel_i_1, pixel_j_1, RED );  
   } 
}
