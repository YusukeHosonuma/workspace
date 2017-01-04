import java.util.stream.Stream;

/**
 * Created by yusuke on 6/21/15.
 */
public class FizzBuzzSample {

    public static void main(String[] args) {

        // Stream.iterateを使用して自然数（の無限数列）のストリームを用意
        Stream<Integer> naturalNumbers = Stream.iterate(1, integer -> integer + 1);

        // 自然数（の無限数列）をfizzBuzzメソッド（参照）を利用して文字列のストリームに変換
        Stream<String> fizzBuzz = naturalNumbers.map(FizzBuzzSample::fizzBuzz);

        // 文字列のストリームから先頭20件を取得して、それぞれ標準出力
        fizzBuzz.limit(20).forEach(s -> System.out.println(s));
    }

    // FizzBuzzメソッド
    private static String fizzBuzz(Integer i) {

        return (i % 15 == 0) ? "FizzBuzz"
                : (i % 3 == 0) ? "Fizz"
                : (i % 5 == 0) ? "Buzz"
                : String.valueOf(i);
    }
}