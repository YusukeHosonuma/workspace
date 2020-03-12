package language.function.client;

import language.function.StringFunctions;

import java.util.ArrayList;
import java.util.Collection;

public class FunctionBasicClient {

    public static void main(String[] args) {

        Collection<Integer> xs = new ArrayList<>();
        xs.add(1);
        xs.add(2);
        xs.add(3);

        /*
         Javaから呼び出すときはデフォルト引数が使えないっぽい
         */
        String s = StringFunctions.joinToString(xs, " | ");
        System.out.println(s); // => "1 | 2 | 3"
    }
}
