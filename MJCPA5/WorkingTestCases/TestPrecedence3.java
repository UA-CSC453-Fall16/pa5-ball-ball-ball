import meggy.Meggy;

class TestPrecedence3 {
        public static void main(String[] whatever){
            if (10 < 11 && 5 < 6)
                  Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.GREEN );
            else
                  Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.RED );

        }
}
