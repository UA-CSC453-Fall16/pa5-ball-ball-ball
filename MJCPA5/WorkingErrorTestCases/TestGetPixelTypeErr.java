import meggy.Meggy;

class TestGetPixelTypeErr {
        public static void main(String[] whatever){
            Meggy.setPixel( (byte)3, (byte)3, Meggy.Color.BLUE );
            Meggy.setPixel( (byte)1, (byte)1, Meggy.getPixel(3, (byte)3) );
            Meggy.setPixel( (byte)3, (byte)3, Meggy.Color.GREEN );
        }
}
