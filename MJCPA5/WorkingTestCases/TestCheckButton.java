import meggy.Meggy;

class TestCheckButton {
  public static void main(String[] whatever){
    while( Meggy.checkButton(Meggy.Button.A) ){
            Meggy.setPixel( (byte)(2), (byte)(4), Meggy.Color.GREEN );
            Meggy.setPixel( (byte)(3), (byte)(4), Meggy.Color.RED );
    }
  }
}
