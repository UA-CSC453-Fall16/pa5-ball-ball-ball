import meggy.Meggy;

class Assign0{
	public static void main(String[] args){
		new C().test();
	}
}

class C{
	public void test(){
		int i;
		byte b;
		boolean bb;
		Meggy.Color mc;
		Meggy.Tone mt;
		//Meggy.Button mb;

		int[] ia;
		Meggy.Color[] mca;

		i = 10;
		b = (byte) i;
		bb = true;
		mc = Meggy.Color.RED;

		ia = new int[2];
		ia[0] = i + b;
		ia[1] = ((byte) 2) * ((byte) 3);

		mca = new Meggy.Color[2];
		mca[0] = mc; // mc
		mca[1] = mca[0];
	}
}
