/**
 * Given string of keystrokes for Nokia-type keypad find out all possible words.
 */

import java.util.*;
import java.util.stream.*;

public class PhoneKeypadJava {

    public static void main(String[] args) {
        Map<Integer, String> dictionary = new HashMap<>();
        dictionary.put(2, "abc");
        dictionary.put(3, "def");
        dictionary.put(4, "ghi");
        dictionary.put(5, "jkl");
        dictionary.put(6, "mno");
        dictionary.put(7, "pqrs");
        dictionary.put(8, "tuv");
        dictionary.put(9, "wxyz");

        Stream<Integer> keystrokes = Stream.of(2, 7, 9);

        Stream<String> ws = keystrokes
                .reduce(
                        Stream.of(""),
                        (current, k) -> current.flatMap(s -> dictionary.get(k).chars().mapToObj(c -> s + (char) c)),
                        (prev, next) -> next
                );

        ws.forEach(System.out::println);
    }
}