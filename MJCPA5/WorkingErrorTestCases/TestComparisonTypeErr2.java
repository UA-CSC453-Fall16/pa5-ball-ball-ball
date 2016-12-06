import meggy.Meggy;

class TestComparisonTypeErr2 {
    public static void main(String[] args) {
        if (Meggy.Color.RED && true)
            Meggy.setPixel((byte)1,(byte)1,Meggy.Color.RED);
    }
}
