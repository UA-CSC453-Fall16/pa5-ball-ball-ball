import meggy.Meggy;

class Type8{
    public static void main(String[] args){
        new C2().invoke();
    }
}

class C2{
    int[] ia;
    Meggy.Color[] mca;

    public void invoke(){
		byte b;
		b = (byte) 5;
        ia = new int[b];
        mca = new Meggy.Color[2];
        ia[0] = 3;
        mca[0] = Meggy.Color.BLUE;    
    }
}
