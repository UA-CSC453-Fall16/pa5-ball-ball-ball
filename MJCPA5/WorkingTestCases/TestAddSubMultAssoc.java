import meggy.Meggy;

class TestAddSubMultAssoc {
    public static void main(String[] args) {
        Meggy.setPixel((byte)((byte)2 * (byte)3 - 2 + 1), (byte)(2), Meggy.Color.RED);
    }
}
