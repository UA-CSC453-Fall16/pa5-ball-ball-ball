import meggy.Meggy;

class Type9{
    public static void main(String[] args){
        new C2().invoke();
    }
}

class C2{
    int[] ia;
    Meggy.Color[] mca;

    public void invoke(){
        ia = new int[2];
        ia[this.get0()] = this.get3();
        ia[this.get3()] = this.get0();
    }

	public int get3(){
		return 3;
	}

	public byte get0(){
		return (byte) 0;
	}
}
