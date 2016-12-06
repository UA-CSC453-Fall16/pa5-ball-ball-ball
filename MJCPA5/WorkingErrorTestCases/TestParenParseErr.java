import meggy.Meggy;

class TestParenParseErr {

    public static void main(String[] whatever){
	new C(.setPx((byte)3,(byte)7,Meggy.Color.BLUE,10);
    }
}

class C {
    
    public int setPx(byte x, byte y, Meggy.Color c, int i) {
            Meggy.setPixel(x, y, c); 
      return i;   
    }
   
}
