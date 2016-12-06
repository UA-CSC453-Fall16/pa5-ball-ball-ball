import meggy.Meggy;

class TestInvArgTypeErr2 {
  public static void main(String[] whatever){
                
      Meggy.toneStart(Meggy.Color.RED, 20);
      Meggy.toneStart(Meggy.Tone.B3, 30); 
  }     
}
