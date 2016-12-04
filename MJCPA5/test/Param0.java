import meggy.Meggy;

class Param0{
    public static void main(String[] args){
        Meggy.delay(new A().invoke());
    }
}

class A{
	public int invoke(){
		return new A().a(new A().a(new A().a(10)));
	}

	public int a(int b){
		return (byte) b * (byte) 2;
	}
}
