import meggy.Meggy;

class Loop{
	public static void main(String[] args){
		new TestLoop().test(4, Meggy.Color.BLUE);
		//new TestLoop().test(14, Meggy.Color.RED);
	}
}

class TestLoop{
	public void test(int numTimes, Meggy.Color c){
		int i;
		i = 0;
		while(i < numTimes){
			Meggy.setPixel((byte) 0, (byte) 0, c);
			i = i + 1;
		}
	}
}