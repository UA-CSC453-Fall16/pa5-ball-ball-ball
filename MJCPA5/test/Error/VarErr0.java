import meggy.Meggy;

class VarErr0{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		Meggy.delay(400);
		int var;
	}
}