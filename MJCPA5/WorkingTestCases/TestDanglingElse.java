import meggy.Meggy;

class TestDanglingElse {
    public static void main(String[] args) {
        if (true)
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.RED);
        if (false)
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.GREEN);
        else
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.VIOLET);
    }
}
