/**
 * Represents the binary operators available in the abstract syntax for
 * expressions.
 */
public enum BinOp {
	PLUS, MINUS, TIMES, DIVIDE;

    public int apply(int left, int right) {
        return switch (this) {
            case PLUS -> left + right;
            case MINUS -> left - right;
            case TIMES -> left * right;
            case DIVIDE -> left / right;
        };
    }

	/**
	 * Parse a binary operator from the given Input. Should only be called when the
	 * current character may start an operator.
	 * 
	 * @param input
	 * @return
	 */
	static BinOp parse(RDInput input) {
		switch (input.peek()) {
		case '+':
			input.skip();
			return PLUS;
		case '-':
			input.skip();
			return MINUS;
		case '*':
			input.skip();
			return TIMES;
		case '/':
			input.skip();
			return DIVIDE;
		default:
			return null; // shouldn't happen
		}
	}
}
