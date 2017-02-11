
import java.util.*;

public class RTree<T> {
    public static class Pos implements Cloneable {
        public int x;
        public int y;

        public Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public Pos clone() {
            return new Pos(x, y);
        }

        public String toString() {
            return "(" + x + "," + y + ")";
        }
    }
    
    public static class Zone {
        public static final Zone none() {
            return new Zone(null, null);
        }
        
        public Pos nw;
        public Pos se;

        public Zone(int w, int n, int e, int s) {
            this.nw = new Pos(w, n);
            this.se = new Pos(e, s);
        }

        public Zone(Pos nw, Pos se) {
            this.nw = nw;
            this.se = se;
        }

        public boolean contains(Pos pos) {
            if(this.nw == null || this.se == null) {
                return false;
            }
            
            if(pos.x < nw.x) return false;
            if(pos.x > se.x) return false;
            if(pos.y < nw.y) return false;
            if(pos.y > se.y) return false;
            return true;
        }

        public void extendTo(Pos pos) {
            if(this.nw == null) {
                this.nw = pos.clone();
            } else {
                this.nw.x = Math.min(this.nw.x, pos.x);
                this.se.x = Math.max(this.se.x, pos.x);
            }
            
            if(this.se == null) {
                this.se = pos.clone();
            } else {
                this.nw.y = Math.min(this.nw.y, pos.y);
                this.se.y = Math.max(this.se.y, pos.y);
            }
        }

        public void extendTo(Zone zone) {
            extendTo(zone.nw);
            extendTo(zone.se);
        }

        public String toString() {
            return "[Zone " + nw + ", " + se + "]";
        }
    }
    
    public static class RLeaf<T> {
        public Pos pos;
        public T value;

        public RLeaf(Pos pos, T value) {
            this.pos = pos;
            this.value = value;
        }

        public String toString() {
            return "{" + pos + ", " + value + "}";
        }
    }

    public static class Sorter<T> implements Comparator<RTree<T>> {
        public static int compare(int lhs, int rhs) {
            if(lhs < rhs) return -1;
            if(lhs > rhs) return 1;
            return 0;
        }
    
        public int compare(RTree<T> lhs, RTree<T> rhs) {
            int result = compare(lhs.numLeafs, rhs.numLeafs);
            if(result == 0) result = compare(lhs.id, rhs.id);
            return result;
        }
    }

    public static class RTreeValueIterator<T> implements Iterator<T> {
        public final RTreeIterator<T> nodeIterator;

        public RTreeValueIterator(RTree<T> target, Zone query) {
            this.nodeIterator = new RTreeIterator<T>(target, query);
        }

        public boolean hasNext() {
            return this.nodeIterator.hasNext();
        }

        public T next() {
            return this.nodeIterator.next().leaf.value;
        }
    }

    public static class RTreeIterator<T> implements Iterator<RTree<T>> {
        public final Zone query;
        public final Iterator<RTree<T>> childrenIterator;
        public RTreeIterator<T> childIterator;
        public boolean hasDoneLeaf;
        public RTree<T> successor;
        public final RTree<T> target;

        public RTreeIterator(RTree<T> target, Zone query) {
            this.target = target;
            this.query = query;
            this.childrenIterator = target.childs.iterator();
            this.childIterator = null;
            hasDoneLeaf = false;
            seek();
        }

        private void seek() {
            if(!hasDoneLeaf) {
                hasDoneLeaf = true;
                if(target.leaf != null) {
                    successor = target;
                    return;
                }
            }

            while(true) {
                if(childIterator.hasNext()) {
                    successor = childIterator.next();
                    break;
                } else if(childrenIterator.hasNext()) {
                    childIterator = new RTreeIterator<T>(childrenIterator.next(), query);
                    continue;
                } else {
                    successor = null;
                    break;
                }
            }
        }

        public boolean hasNext() {
            return successor != null;
        }

        public RTree<T> next() {
            RTree<T> result = successor;
            seek();
            return result;
        }
    }
    
    public final int maxChildCount;
    public int numLeafs;
    public Zone zone;
    public RLeaf<T> leaf;
    public SortedSet<RTree<T>> childs;
    public final int id;

    public static int nextId = 0;

    public RTree(int maxChildCount) {
        this.maxChildCount = maxChildCount;
        this.numLeafs = 0;
        this.zone = Zone.none();
        this.leaf = null;
        this.childs = new TreeSet<RTree<T>>(new Sorter<T>());
        this.id = nextId++;
    }

    public void add(Pos  pos, T value) {
        add(new RLeaf<T>(pos, value));
    }

    public void add(RLeaf<T> leaf) {
        // 1) if one of the childs can take it, hand it over
        for(RTree<T> child : childs) {
            if(child.zone.contains(leaf.pos)) {
                addLeafToChild(child, leaf);
                System.out.println("gave " + leaf + " to child; #" + this);
                return;
            }
        }
        
        // 2) if this node can take it, put it in
        if(this.leaf == null) {
            this.leaf = leaf;
            this.zone.extendTo(leaf.pos);
            ++this.numLeafs;
            System.out.println("taking it. #" + this);
            return;
        }
        
        // 3) if this node can handle another child, let's do that
        if(this.childs.size() < this.maxChildCount) {
            System.out.println("adding child for " + leaf + "... #" + this);
            RTree<T> child = new RTree<T>(maxChildCount);
            this.childs.add(child);
            addLeafToChild(child, leaf);
            System.out.println("added  child for it. #" + this);
            return;
        }
        
        // 4) otherwise, pass it to the child with the least descendants
        System.out.println("can't deal with it. #" + this);
        addLeafToChild(this.childs.first(), leaf);
    }

    private void addLeafToChild(RTree<T> child, RLeaf<T> leaf) {
        child.add(leaf);
        this.zone.extendTo(child.zone);
        ++this.numLeafs;
    }

    public Iterator<RTree<T>> nodeIterator(Zone zoneQuery) {
        return new RTreeIterator<T>(this, zoneQuery);
    }

    public Iterator<T> valueIterator(Zone zoneQuery) {
        return new RTreeValueIterator<T>(this, zoneQuery);
    }

    public String toString() {
        return zone.toString() + " " + leaf.toString();
    }

    public String print() {
        StringBuilder sb = new StringBuilder();
        print("\n", sb);
        return sb.toString();
    }

    public void print(String indent, StringBuilder sb) {
        sb.append(indent).append(this);
        for(RTree<T> child : childs) {
            child.print(indent + "    ", sb);
        }
    }

    public static void main(String[] args) {
        RTree<Integer> tree1 = new RTree<Integer>(3);
        System.out.println("***********");
        System.out.println("===========");
        tree1.add(new Pos(3, 3), 33);
        System.out.println("===========");
        tree1.add(new Pos(5, 5), 55);
        System.out.println("===========");
        tree1.add(new Pos(4, 5), 45);
        System.out.println("===========");
        tree1.add(new Pos(6, 6), 66);
        System.out.println("===========");
        tree1.add(new Pos(7, 7), 77);
        System.out.println("===========");
        tree1.add(new Pos(4, 4), 44);
        System.out.println("===========");
        tree1.add(new Pos(6, 3), 63);
        System.out.println("===========");
        tree1.add(new Pos(9, 6), 96);
        System.out.println("===========");
        tree1.add(new Pos(5, 6), 56);
        System.out.println("===========");
        tree1.add(new Pos(5, 6), 56);
        System.out.println("===========");
        tree1.add(new Pos(6, 3), 63);
        System.out.println("numLeafs: " + tree1.numLeafs);
        System.out.println("direct childs: " + tree1.childs.size());
        System.out.println(tree1.print());
        System.out.println("***********");
        for(Zone z : new Zone[] {new Zone(3, 3, 6, 5)}) {
            System.out.println("all values in zone " + z + ": ");
            Iterator<Integer> it = tree1.valueIterator(z);
            while(it.hasNext()) {
                Integer value = it.next();
                System.out.println("    " + value);
            }
        }
    }
}
