import meggy.Meggy;

class Assign1{
	public static void main(String[] args){
		new C().test();
		new C().test1();
		new C().test2();
	}
}

class C{
	public void test(){
		int b;
		b = 7;
	}

	public void test1(){
		byte b;
		b = (byte) 7;
	}
	
	public void test2(){
		Meggy.Color b;
		b = Meggy.Color.BLUE;
	}
} 