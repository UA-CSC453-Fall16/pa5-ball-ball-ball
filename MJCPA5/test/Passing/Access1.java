import meggy.Meggy;

class Access1{
	public static void main(String[] args){
		new C().test();
		new C().test1();
		new C().test2();
		new C().test3();
	}
}

class C{
	int[] ia;
	Meggy.Color[] mca;

	public void test(){
		ia = new int[3];
		ia[1] = 5;
	}

	public void test1(){
		mca = new Meggy.Color[3];
		mca[1] = Meggy.Color.BLUE;
	}

	public int test2(){
		int[] AI;	
		AI = new int[(byte)3];
		
		Meggy.delay(AI[0]);

		return ia[(byte)1];
	}

	public Meggy.Color test3(){
		return mca[1];
	}
} 