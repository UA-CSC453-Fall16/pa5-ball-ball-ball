import meggy.Meggy;

class Variable1{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		int var;
		byte variable;
		Meggy.Color color;
		boolean b;
		//Meggy.Button butt;
		Meggy.Tone t;
		t = Meggy.Tone.C3;
		var = 5;
		Meggy.toneStart(t, var);
	}
} 