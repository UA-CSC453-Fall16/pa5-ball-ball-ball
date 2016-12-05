import meggy.Meggy;

class VarErr1{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
	}

	int var;
}