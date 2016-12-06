import meggy.Meggy;

class TestCommaParseErr {

    public static void main(String[] whatever){
	new C().setP((byte)3,(byte)7,Meggy.Color.BLUE);
	}
}
class C {
    
    pub,,lic void setP(byte x, byte y, Meggy.Color c) {
	Meggy.setPixel(x, y, c);    
    }
    
}
