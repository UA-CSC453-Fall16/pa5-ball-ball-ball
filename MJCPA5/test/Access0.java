import meggy.Meggy;

class Access0{
    public static void main(String[] args){
        Meggy.delay(new C().test0());
    }
}

class C{

    int[] deep;
    D mine;
    D mine2;

    public int test0(){
        int val;
		val = (new int[6])[0]; // make sure our parser doesn't accept the removal of these parentheses
		val = new int[6][0];   // the above should pass this should fail.
    }

    public int[] test2(){
        deep = new int[5];
        deep[0] = 3;
        return deep;
    }

    public D test3(){
        mine2 = new D();
        return mine2;
    }

    public void test(){
        int a;
        int b;
        int c;
        int d;

        this.test2();
        deep[1] = 4;
        mine = new D();
        a = this.test3().init().length;
        // b = mine.vari; // MJ.jar doesn't like this
        // c = this.mine.hard().vari; // MJ.jar doesn't like this
        d = this.test2()[a];
    }
}

class D{
    int vari;
    int[] dArr;

    public void set(){
        vari = 3;
    }

    public int[] init(){
        dArr = new int[3];
        return dArr;
    }

    public D hard(){
        D soHard;
        soHard = new D();
        soHard.set();
        return soHard;
    }
}