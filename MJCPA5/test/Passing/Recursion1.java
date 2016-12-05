import meggy.Meggy;

class Recursion1 {
	public static void main(String[] args){
		new C().testRecursion();
	}
}

class C{
	int num;

	public void testRecursion(){
		num = 0;
		this.test();
		Meggy.delay(num);
	}

	public void test(){
		if ( num < 3 ) {
			num = num + 1;
			this.test();
		}
	}
}