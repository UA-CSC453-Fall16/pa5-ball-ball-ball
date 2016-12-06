import meggy.Meggy;

class TestIntNegationAssoc {
    public static void main(String[] args) {
        Meggy.setPixel((byte)(10 + -9),(byte)1,Meggy.Color.VIOLET);
    }
}
