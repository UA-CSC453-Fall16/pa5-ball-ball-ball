import meggy.Meggy;

class Assign2{
	public static void main(String[] args){
		new C().test();
		new C().test1();
	}
}

class C{
	public void test(){
		int[] b;
		b = new int[3];
		b[1] = 5;
	}

	public void test1(){
		Meggy.Color[] b;
		b = new Meggy.Color[3];
		b[1] = Meggy.Color.BLUE;
	}
} 