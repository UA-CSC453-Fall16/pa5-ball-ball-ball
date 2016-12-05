import meggy.Meggy;

class Recursion0 {
	public static void main(String[] args){
		new C().testRecursion(0);
	}
}

class C{
	public void testRecursion(int c){
		if ( c < 3 )
			this.testRecursion(c + 1);
		Meggy.delay(c);
	}
}