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
		int[] arr;
		val = (new int[6])[0]; // make sure our parser doesn't accept the removal of these parentheses
							   // OR we can just have type checking say, the child of an array access cannot be another array access
		//val = new int[6][0];   // the above should pass this should fail.
		//arr = new int[5];
		//val = arr[0][1]; //Should fail and does correctly
        return val;
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
        // c = mine.hard().vari; // MJ.jar and our compiler both report this error correctly
        // c = this.mine.hard().vari; // MJ.jar and our compiler both report this error correctly
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