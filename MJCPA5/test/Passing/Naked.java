import meggy.Meggy;

class Naked{
	public static void main(String[] args){
		new Test().naked0();
		new Test().naked1();
		new Test().naked2();
		new Test().naked3();
		new Test().naked4();
	}
}
class Test{
	public Test naked0(){
		return this;
	}

	public int naked1(){
		return 1;
	}

	public byte naked2(){
		return (byte) 1;
	}

	public Meggy.Color naked3(){
		return Meggy.Color.BLUE;
	}

	public boolean naked4(){
		return false;
	}
}