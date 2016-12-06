import meggy.Meggy;

class Naked2{
	public static void main(String[] args){
		new Test().naked0();
	}
}
class Test{
	public Test naked0(){
		if(this.naked1() == this.naked2()){
			this.naked1();
			this.naked2();
			this.naked3();
			this.naked4();
		}
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