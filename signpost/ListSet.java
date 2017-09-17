
import java.util.*;

//public class ListSet<T> implements Set<T>, List<T> {
public class ListSet<T> extends ArrayList<T> implements Set<T>, List<T> {
    private static final long serialVersionUID = 0;

    // this breaks the contract of List to fulfill the contract of Set
    @Override
    public boolean add(T element) {
        if(!this.contains(element)) {
            return super.add(element);
        } else {
            return false;
        }
    }

    /*
    @Override
    public void clear() {
        values.clear();
    }

    @Override
    public int size() {
        return values.size();
    }

    @Override
    public boolean removeAll(Collection<?> elements) {
    }

    @Override
    public boolean retainAll(Collection<?> elements) {
    }

    @Override
    public boolean addAll(Collection<? extends T> elements) {
    }

    @Override
    public boolean containsAll(Collection<?> elements) {
    }

    @Override
    public boolean remove(Object object) {
    }

    @Override
    public <T> T[] toArray(T[] array) {
    }

    @Override
    public Object[] toArray() {
    }

    @Override
    public Iterator<T> iterator() {
    }

    @Override
    public boolean contains(Object object) {
    }
    */
}
