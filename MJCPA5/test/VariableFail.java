import meggy.Meggy;

class VariableFail{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		int var;
	}

	public void test2(){
		int var;
		byte variable;
	}

	public void test3(){
		byte variable;
		Meggy.Color color;
		boolean b;
		Meggy.Button butt;
		Meggy.Tone t;
		Meggy.delay(20);
		Meggy.Color[] cArr;
		int[] iArr;
	}
}
