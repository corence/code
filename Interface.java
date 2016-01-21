
interface Ordinal<T> {
    public boolean lessThan(T rhs);
    public boolean greater(T rhs);
}

class OrdinalInt implements Ordinal<OrdinalInt> {
    public final Integer value;

    public OrdinalInt(Integer value) {
        this.value = value;
    }
    
    public boolean lessThan(OrdinalInt that) {
        return this.value < that.value;
    }

    public boolean greater(OrdinalInt that) {
        return this.value < that.value;
    }
}

public class Interface {
    public static Integer getMin(OrdinalInt lhs, OrdinalInt rhs) {
        if(lhs.lessThan(rhs)) {
            return lhs.value;
        } else {
            return rhs.value;
        }
    }
    
    public static void main(String[] args) {
        OrdinalInt xx = new OrdinalInt(3);
        OrdinalInt yy = new OrdinalInt(6);

        Integer result = getMin(xx, yy);

        System.out.println("win: " + result);
    }
}
