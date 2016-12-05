import meggy.Meggy;

class Button0{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		Meggy.Button b;
		b = Meggy.Button.B;
		if(Meggy.checkButton(b))
			Meggy.delay(100);
	}
}