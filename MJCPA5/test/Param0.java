import meggy.Meggy;

class Param0{
    public static void main(String[] args){
        Meggy.delay(new C().invoke());
    }
}

class C{
	public int invoke(){
		return new C().a(new C().a(new C().a(10)));
	}

	public int a(int b){
		return (byte) b * (byte) 2;
	}
}
