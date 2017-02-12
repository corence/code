
import java.util.*;

public class RTree<T> {
    public static class Pos {
        public final int x;
        public final int y;

        public Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public String toString() {
            return "(" + x + "," + y + ")";
        }
    }
    
    public static class Zone {
        public static final Zone none() {
            return new Zone(null, null);
        }
        
        public final Pos nw;
        public final Pos se;

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

        public boolean overlaps(Zone zone) {
            if(this.nw == null || this.se == null) {
                return false;
            }
            
            if(zone.nw == null || zone.se == null) {
                return false;
            }
            
            if(this.nw.x > zone.se.x) return false;
            if(this.nw.y > zone.se.y) return false;
            if(this.se.x < zone.nw.x) return false;
            if(this.se.y < zone.nw.y) return false;
            return true;
        }

        public Zone extendTo(Pos pos) {
            Pos nw, se;
            
            if(this.nw == null) {
                nw = pos;
            } else {
                int x = Math.min(this.nw.x, pos.x);
                int y = Math.min(this.nw.y, pos.y);
                nw = new Pos(x, y);
            }
            
            if(this.se == null) {
                se = pos;
            } else {
                int x = Math.max(this.se.x, pos.x);
                int y = Math.max(this.se.y, pos.y);
                se = new Pos(x, y);
            }

            return new Zone(nw, se);
        }

        public Zone extendTo(Zone zone) {
            return this.extendTo(zone.nw).extendTo(zone.se);
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
        public RTree<T> predecessor;
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
            predecessor = successor;
            
            if(!hasDoneLeaf) {
                hasDoneLeaf = true;
                if(target.leaf != null) {
                    successor = target;
                    return;
                }
            }

            while(true) {
                if(childIterator != null && childIterator.hasNext()) {
                    RTree<T> nextNode = childIterator.next();
                    if(nextNode.leaf != null & query.contains(nextNode.leaf.pos)) {
                        successor = nextNode;
                        break;
                    } else {
                        continue;
                    }
                } else if(childrenIterator.hasNext()) {
                    RTree<T> nextNode = childrenIterator.next();
                    if(nextNode.zone.overlaps(query)) {
                        childIterator = new RTreeIterator<T>(nextNode, query);
                    } else {
                        childIterator = null;
                    }
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

        public void remove() {
            predecessor.removeLeaf();
        }
    }
    
    public final RTree<T> parent;
    public final int maxChildCount;
    public int numLeafs;
    public Zone zone;
    public RLeaf<T> leaf;
    public SortedSet<RTree<T>> childs;
    public final int id;

    public static int nextId = 0;

    public RTree(RTree<T> parent, int maxChildCount) {
        this.parent = parent;
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
            this.zone = this.zone.extendTo(leaf.pos);
            ++this.numLeafs;
            System.out.println("taking it. #" + this);
            return;
        }
        
        // 3) if this node can handle another child, let's do that
        if(this.childs.size() < this.maxChildCount) {
            System.out.println("adding child for " + leaf + "... #" + this);
            RTree<T> child = new RTree<T>(this, maxChildCount);
            this.childs.add(child);
            addLeafToChild(child, leaf);
            System.out.println("added  child for it. #" + this);
            return;
        }
        
        // 4) otherwise, pass it to the child with the least descendants
        System.out.println("can't deal with it. #" + this);
        addLeafToChild(this.childs.first(), leaf);
    }

    public void removeLeaf() {
        if(this.leaf == null) return;

        this.leaf = null;
        if(this.parent != null) {
            parent.decrementLeafCount(this);
        } else {
            --numLeafs;
        }
    }

    public void decrementLeafCount(RTree<T> child) {
        this.childs.remove(child);
        --child.numLeafs;

        if(child.numLeafs > 0) {
            this.childs.add(child);
        }
        
        if(this.parent != null) {
            parent.decrementLeafCount(this);
        }
    }

    private void addLeafToChild(RTree<T> child, RLeaf<T> leaf) {
        child.add(leaf);
        this.zone = this.zone.extendTo(child.zone);
        ++this.numLeafs;
    }

    public Iterator<RTree<T>> nodeIterator(Zone zoneQuery) {
        return new RTreeIterator<T>(this, zoneQuery);
    }

    public Iterator<T> valueIterator(Zone zoneQuery) {
        return new RTreeValueIterator<T>(this, zoneQuery);
    }

    public String toString() {
        return zone + " " + leaf;
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
        RTree<Integer> tree1 = new RTree<Integer>(null, 3);
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
        for(Zone z : new Zone[] {new Zone(3, 3, 6, 5)}) {
            System.out.println("REMOVING all values in zone " + z + ". shouldn't remove 96: ");
            Iterator<RTree<Integer>> it = tree1.nodeIterator(z);
            while(it.hasNext()) {
                RTree<Integer> node = it.next();
                System.out.println("    removed " + node);
                it.remove();
            }
        }
        System.out.println(tree1.print());
        System.out.println("***********");
    }
}
