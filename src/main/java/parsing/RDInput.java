/**
 * Wrapper around a Reader that provides useful abstractions for recursive
 * descent parsing.
 */
public class RDInput {
	private java.io.Reader source;
	private char next;
	private boolean atEnd;

	public RDInput(java.io.Reader source) {
		this.source = source;
		skip();
	}

	/**
	 * @return current available character
	 */
	public char peek() {
		return next;
	}

	/**
	 * @return true if no more characters available
	 */
	public boolean atEnd() {
		return atEnd;
	}

	/**
	 * Read the next available character, skipping over whitespace
	 */
	public void skip() {
		readNext();
		skipWhitespace();
	}

	/**
	 * If the current character is c, skip to the next. Throw a RuntimeException if
	 * the character does not match.
	 * 
	 * @param c
	 */
	public void match(char c) {
		if (next == c) {
			skip();
		} else {
			throw new RuntimeException("expected " + c + " but found " + next);
		}
	}

	/**
	 * Read an identifier (letter followed by zero or more letters or digits). This
	 * should only be called when the current character is a letter.
	 * 
	 * @return the identifier
	 */
	public String readIdent() {
		StringBuilder builder = new StringBuilder();
		builder.append(next);
		readNext();
		while (!atEnd && Character.isLetterOrDigit(next)) {
			builder.append(next);
			readNext();
		}
		skipWhitespace();
		return builder.toString();
	}

	/**
	 * Read an integer (digit followed by zero or more additional digits). This
	 * should only be called when the current character is a digit.
	 * 
	 * @return the number
	 */
	public int readInt() {
		int result = next - '0';
		readNext();
		while (!atEnd && Character.isDigit(next)) {
			result = result * 10 + next - '0';
			readNext();
		}
		skipWhitespace();
		return result;
	}

	private void readNext() {
		try {
			int c = source.read();
			if (c != -1) {
				next = (char) c;
				atEnd = false;
			} else {
				next = '\0';
				atEnd = true;
			}
		} catch (java.io.IOException e) {
			next = '\0';
			atEnd = true;
		}
	}

	private void skipWhitespace() {
		while (!atEnd && Character.isWhitespace(next)) {
			readNext();
		}
	}
}
