import meggy.Meggy;

class Array2{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		int[] x;
		x = new int[10];
		x[1] = 100;
		Meggy.delay(x[1]);
	}
} 