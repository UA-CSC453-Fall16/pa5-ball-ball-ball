import meggy.Meggy;

class TestRetType2 {
    public static void main(String[] whatever){
	new C().setPix((byte)1,(byte)1);
    }
}

class C {
    public int setPix(byte pixeli, byte pixelj) {
	Meggy.setPixel(pixeli, pixelj, Meggy.Color.RED );
	return 99;
    } 
}
