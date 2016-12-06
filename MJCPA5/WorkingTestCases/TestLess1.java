import meggy.Meggy;

class TestLess1 {
        public static void main(String[] whatever){
            if (10 < 11)
                  Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.GREEN );
            else
                  Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.RED );

            if (11 < 10)
                  Meggy.setPixel( (byte)1, (byte)1, Meggy.Color.GREEN );
            else
                  Meggy.setPixel( (byte)1, (byte)1, Meggy.Color.RED );
        }
}
