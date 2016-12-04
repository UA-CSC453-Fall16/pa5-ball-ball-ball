import meggy.Meggy;

class Invoke2{
    public static void main(String[] args){
        new C2().getC2().getC2().invoke();
        new D().here();
    }
}

class C2{
    public C2 getC2(){
        return this.againGetC2();
    }    
	
	public C2 againGetC2(){
		return this;
	}

    public void invoke(){
        Meggy.delay(100);
    }
}

class D{
	C2 val;
	public C2 here(){
		return this; // fails, this is of type D, typesig says here() returns type C2
	}
}