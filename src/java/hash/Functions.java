package hash;

public class Functions {
    public static long DJBHash(String str) {
        long hash = 5381;

        for(int i = 0; i < str.length(); i++) {
            hash = ((hash << 5) + hash) + str.charAt(i);
        }

        return hash;
    }
}
