import meggy.Meggy;

class Assign0{
	public static void main(String[] args){
		new C().test();
	}
}

class C{

	int[] deep;

	public int[] test2(){
		deep = new int[5];
		deep[0] = 3;
		return deep;
	}

	public void test(){
		int heyo;
		heyo = this.test2().test3().length;
		//heyo = this.test2()[0];
		//this.test2()[0] = 3;
	}
}