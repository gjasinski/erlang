import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Stack;

public class JavaONP {

    public static void main(String[] args) {
        System.out.println(onp("2 3 * 1 + 4 5 / - 6 +"));
        System.out.println(onp("1 2 3 4 5 6 7 * + + + + +"));
        System.out.println(onp("4 7 + 3 / 2 19 - *"));
        System.out.println(onp("17 31 4 + * 26 15 - 2 * 22 - / 1 -"));

    }

    private static double onp(String s) {
        double tmp;
        Deque<Double> deque = new ArrayDeque<>();
        String[] splited = s.split(" ");
        for(String expression: splited){
            switch (expression){
                case "+": deque.push(deque.pop() + deque.pop());
                    break;
                case "-": tmp = deque.pop();
                    deque.push(deque.pop() - tmp);
                    break;
                case "*": deque.push(deque.pop() * deque.pop());
                    break;
                case "/": tmp = deque.pop();
                    deque.push(deque.pop() / tmp);
                    break;
                case "pow": tmp = deque.pop();
                    deque.push(Math.pow(deque.pop(), tmp));
                    break;
                case "sqrt": deque.push(Math.sqrt(deque.pop()));
                    break;
                case "sin": deque.push(Math.sin(deque.pop()));
                    break;
                case "cos": deque.push(Math.cos(deque.pop()));
                    break;
                case "tan": deque.push(Math.tan(deque.pop()));
                    break;
                default: deque.push(Double.parseDouble(expression));
                    break;
            }
        }
        return deque.pop();
    }
}