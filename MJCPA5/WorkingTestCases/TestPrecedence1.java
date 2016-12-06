import meggy.Meggy;

class TestPrecedence1 {
    public static void main(String[] args) {
        Meggy.setPixel((byte)((byte)10 - (byte)2 * (byte)4), (byte)2, Meggy.Color.VIOLET);
    }
}
