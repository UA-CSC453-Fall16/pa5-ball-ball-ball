import meggy.Meggy;

class ArrayAccess{
	public static void main(String[] args){
		new Test().testArrays().possiblyValid();
	}
}
class Test{
	int [] intArr;
	public Test testArrays(){
		if(Meggy.checkButton(Meggy.Button.B))
				intArr = new int[50];
		return this;
	}

	public int possiblyValid(){
		return intArr[15]; // Impossible to tell if this is valid at compile time.
	}
}