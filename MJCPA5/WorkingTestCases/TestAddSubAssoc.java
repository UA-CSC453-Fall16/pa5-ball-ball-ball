import meggy.Meggy;

class TestAddSubAssoc {
    public static void main(String[] args) {
        Meggy.setPixel((byte)(2 - 4 + 6 - 2), (byte)2, Meggy.Color.RED);
    }
}
