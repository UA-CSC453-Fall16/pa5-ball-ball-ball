import meggy.Meggy;

class Type7{
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
        ia[mca.length - 1] = 3;
        mca[0] = Meggy.Color.BLUE;    
    }
}
