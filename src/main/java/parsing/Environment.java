import java.util.HashMap;
import java.util.Map;

public class Environment {
    private Map<String, Integer> map;

    public Environment() {
        map = new HashMap<>();
    }

    public void bind(String id, int value) {
        map.put(id, value);
    }

    public int lookup(String id) {
        return map.getOrDefault(id, 0);
    }
}
