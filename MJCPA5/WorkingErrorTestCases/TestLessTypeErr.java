import meggy.Meggy;

class TestLessTypeErr {
    public static void main(String[] args) {
        if (true == false && false < true)
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.RED);
    }
}
