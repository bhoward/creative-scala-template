/**
 * Represents an expression node in an abstract syntax tree.
 */
public interface Expr {
	// instance methods appropriate to the application should be declared here
	public int eval(Environment env);

	/**
	 * Parse an expression (sum/difference of one or more terms).
	 * 
	 * @param input
	 * @return
	 */
	public static Expr parse(RDInput input) {
		Expr e = parseTerm(input);
		while (input.peek() == '+' || input.peek() == '-') {
			BinOp op = BinOp.parse(input);
			Expr e2 = Expr.parseTerm(input);
			e = new BinOpExpr(e, op, e2);
		}
		return e;
	}

	/**
	 * Parse a term (product/quotient of one or more factors).
	 * 
	 * @param input
	 * @return
	 */
	public static Expr parseTerm(RDInput input) {
		Expr e = parseFactor(input);
		while (input.peek() == '*' || input.peek() == '/') {
			BinOp op = BinOp.parse(input);
			Expr e2 = Expr.parseFactor(input);
			e = new BinOpExpr(e, op, e2);
		}
		return e;
	}

	/**
	 * Parse a factor (identifier, number, or parenthesized expression). Throws a
	 * RuntimeException if a factor is not available.
	 * 
	 * @param input
	 * @return
	 */
	public static Expr parseFactor(RDInput input) {
		if (Character.isLetter(input.peek())) {
			String id = input.readIdent();
			return new IdentExpr(id);
		} else if (Character.isDigit(input.peek())) {
			int n = input.readInt();
			return new NumExpr(n);
		} else if (input.peek() == '(') {
			input.skip();
			Expr e = parse(input);
			input.match(')');
			return e;
		} else {
			throw new RuntimeException("expected a factor");
		}
	}

	public record BinOpExpr(Expr left, BinOp op, Expr right) implements Expr {
		public int eval(Environment env) {
			int l = left.eval(env);
			int r = right.eval(env);
			return op.apply(l, r);
		}
	}

	public record IdentExpr(String id) implements Expr {
		public int eval(Environment env) {
			return env.lookup(id);
		}
	}

	public record NumExpr(int n) implements Expr {
		public int eval(Environment env) {
			return n;
		}
	}
}
