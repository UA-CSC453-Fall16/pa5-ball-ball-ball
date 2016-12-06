import meggy.Meggy;

class TestGetPixel1 {
        public static void main(String[] whatever){

            Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.RED );

            if ( Meggy.Color.RED == Meggy.getPixel((byte)0, (byte)1) )
                  Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.GREEN );
            else
                  Meggy.setPixel( (byte)0, (byte)1, Meggy.Color.RED );
        }
}
