import meggy.Meggy;

class Type1{
    public static void main(String[] args){
        new C2().invoke();
    }
}

class C2{
    int[] ia;
    Meggy.Color[] mca;

    public void invoke(){
        ia = new int[2];
        mca = new Meggy.Color[2];
        ia[true] = 3; //should fail
        mca[0] = Meggy.Color.BLUE;    
    }
}
