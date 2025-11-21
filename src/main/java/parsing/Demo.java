import java.io.StringReader;

public class Demo {
	public static void main(String[] args) {
		String sample = "  (abc+5) * (r2d2 * 3 + abc) \n";
		RDInput input = new RDInput(new StringReader(sample));
		Expr e = Expr.parse(input);
		if (input.atEnd()) {
			System.out.println("Found " + e);
            Environment env = new Environment();
            env.bind("abc", 1);
            env.bind("r2d2", 2);
            System.out.println("Value = " + e.eval(env));
		} else {
			System.out.println("unscanned input after parsing " + e);
		}
	}
}