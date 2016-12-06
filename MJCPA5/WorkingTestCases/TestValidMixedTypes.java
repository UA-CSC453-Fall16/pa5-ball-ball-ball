import meggy.Meggy;

class TestValidMixedTypes {
    public static void main(String[] asdf) {
        if ((byte)4 == 4)
            Meggy.setPixel((byte)1,(byte)2,Meggy.Color.BLUE);
        if (-(byte)0 == -0)
            Meggy.setPixel((byte)1,(byte)2,Meggy.Color.GREEN);
        if ((byte)3 + 4 == (byte) 7)
            Meggy.setPixel((byte)1,(byte)2,Meggy.Color.RED);
        if (1 + (byte)2 == 0)
            Meggy.setPixel((byte)1,(byte)2,Meggy.Color.DARK);
        if (3 - (byte)2 == 1)
            Meggy.setPixel((byte)1,(byte)2,Meggy.Color.ORANGE);
        if ((byte)10 - 10 == 10)
            Meggy.setPixel((byte)1,(byte)2,Meggy.Color.VIOLET);
    }
}