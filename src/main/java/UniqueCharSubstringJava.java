/**
 * Given a string and number M find length of the longest substring of M unique chars.
 * Input: abbbcddd, M = 3
 * Output: 7 (length of bbbcddd)
 */

import java.util.*;

public class UniqueCharSubstringJava {

    public static int longest(String str, int m) {
        Set<Character> uniqueChars = new HashSet<>();
        int currentLen = 0;
        int maxLen = 0;
        int next = 0;

        for (int i = 0, strlen = str.length(); i < strlen; ++i) {
            char c = str.charAt(i);

            if (uniqueChars.isEmpty()) {
                next = i;
                while (next < strlen && str.charAt(next) == c) {
                    ++next;
                    ++currentLen;
                }
                i = next - 1;
            }

            maxLen = Math.max(currentLen, maxLen);
            ++currentLen;
            uniqueChars.add(c);

            if (uniqueChars.size() > m) {
                uniqueChars.clear();
                currentLen = 0;
                i = next;
            }
        }

        return Math.max(maxLen, currentLen);
    }

    public static void main(String[] args) {
        System.out.println( longest("abbbcddd", 3) );
        System.out.println( longest("aabbcccddddee", 3) );
        System.out.println( longest("aabbcccddddee", 1) );
    }

}
