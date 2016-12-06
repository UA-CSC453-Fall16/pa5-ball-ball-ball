import meggy.Meggy;

class TestBoolNegationAssoc {
    public static void main(String[] args) {
        if (true == !false)
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.VIOLET);
    }
}