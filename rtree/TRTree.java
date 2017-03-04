
import java.util.*;

public class TRTree<T> {
    public static class Rect {
        public final int west;
        public final int north;
        public final int east;
        public final int south;

        public Rect(int west, int north, int east, int south) {
            this.west = west;
            this.north = north;
            this.east = east;
            this.south = south;
        }

        public static Rect extend(Rect a, Rect b) {
            if(a == null) {
                 return b;
            }

            return a.extendTo(b);
        }

        public boolean overlaps(Rect that) {
            if(this.west > that.east) return false;
            if(this.north > that.south) return false;
            if(this.east < that.west) return false;
            if(this.south < that.north) return false;
            return true;
        }

        public Rect extendTo(Rect that) {
            if(that == null) return this;
            return new Rect(
                    Math.max(this.west, that.west),
                    Math.max(this.north, that.north),
                    Math.max(this.east, that.east),
                    Math.max(this.south, that.south));
        }
    }

    public abstract class Tree<T> {
        public Node<T> parent;
        public Rect zone;
        public abstract Tree<T> insert(Tree<T> node);

        public void ensureParent() {
            if(this.parent == null) {
                this.parent = new Node<T>();
                parent.insert(this);
            }
        }
    }

    public class Node<T> extends Tree<T> {
        public final ArrayList<Tree<T>> childs = new ArrayList<>();

        public static final int maxChildsPerNode = 3;

        public Tree<T> insert(Tree<T> node) {
            zone = Rect.extend(zone, node.zone);
            Tree<T> child = chooseBestChild(node.zone);
            child.insert(node);
            if(this.childs.size() > maxChildsPerNode) {
                split();
            }
        }

        public Tree<T> insertDirectly(Tree<T> node) {
            this.childs.add(node);
            if(this.childs.size() > maxChildsPerNode) {
                return split();
            } else 
            return parent;
        }

        private Tree<T> chooseBestChild(Rect zone) {
        }
    }

    public class Leaf<T> extends Tree<T> {
        public final T value;

        public Leaf(T value) {
            this.value = value;
        }

        public Tree<T> insert(Tree<T> node) {
            ensureParent();
            return parent.insertDirectly(node);
        }
    }
}
