import meggy.Meggy;

class Array6{
	public static void main(String[] args) {
		Meggy.delay(new C().invoke());
	}
}

class C{
	int[] arr;

	public int invoke(){
		int len;
		arr = new int[5];
		len = arr.length;
		return len;
	}
}