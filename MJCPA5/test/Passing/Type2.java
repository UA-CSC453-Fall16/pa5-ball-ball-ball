import meggy.Meggy;

class Type2{
    public static void main(String[] args){
        new C2().invoke();
    }
}

class C2{
    int[] ia;
    Meggy.Color[] mca;
    int b;
    byte c;

    public void invoke(){
        ia = new int[2];
        mca = new Meggy.Color[2];
        ia[0] = true; //should fail
        mca[0] = Meggy.Color.BLUE;    
    }
}
