import meggy.Meggy;

class Recursion2 {
	public static void main(String[] args){
		new C().testRecursion();
	}
}

class C{
	int num;

	public void testRecursion(){
		num = 0;
		this.test(3);
		Meggy.delay(num);
	}

	public void test(int u){
		if ( num < 3 ) {
			num = num + 1;
			this.test(u + 1);
		}
	}
}