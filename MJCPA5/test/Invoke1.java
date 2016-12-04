import meggy.Meggy;

class Invoke1{
    public static void main(String[] args){
        new C2().getC2().getC2().invoke();
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