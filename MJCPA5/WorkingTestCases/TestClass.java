import meggy.Meggy;

class TestClass {

    public static void main(String[] whatever){
	new C().setPix((byte)3,(byte)7,Meggy.Color.BLUE);
    }
}

class C {
    
    public void setPix(byte x, byte y, Meggy.Color c) {
            Meggy.setPixel(x, y, c);    
    }
    
}
