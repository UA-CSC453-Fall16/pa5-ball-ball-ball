import meggy.Meggy;

class Array1{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		int[] x;
		x = new int[10];
		Meggy.delay(x[1]);
	}
} 