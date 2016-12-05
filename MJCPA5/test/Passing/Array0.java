import meggy.Meggy;

class Array0{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		int[] b;
		b = new int[7];
		Meggy.delay(b.length);
	}
} 