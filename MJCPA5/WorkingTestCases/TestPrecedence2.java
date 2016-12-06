import meggy.Meggy;

class TestPrecedence2 {
    public static void main(String[] args) {
        if (true == false && 10 < 11)
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.RED);
    }
}
