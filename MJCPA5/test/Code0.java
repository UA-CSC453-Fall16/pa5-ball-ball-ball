import meggy.Meggy;

class Code0{
	public static void main(String[] args){
		new C().invoke((byte) 6 * (byte) 2 - 3);
		new C().bro();
		Meggy.delay(new C().ret(5));
	}
}

class C{
	int val;
	byte bbb;

	public void invoke(int hi){
		int b;
		b = hi;
		//val = b;
		//Meggy.setAuxLEDs(val);
		Meggy.setAuxLEDs(b);
	}

	public void bro(){
		this.invoke(6);
	}

	public int ret(int r){
		return r;
	}
}