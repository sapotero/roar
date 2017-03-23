package down;

public class Shifter {
    public static long shiftLeft(long base, long pow){
        return base <<= pow;
    }

    public static long shiftRight(long base, long pow){
        return base >>= pow;
    }
}

//(Integer/toBinaryString 125) 