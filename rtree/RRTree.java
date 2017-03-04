
import java.util.*;

public class RRTree<T> {
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
        public static final Zone none = new Zone(null, null);
        
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

    public static abstract class Tree<T> {
        public Tree<T> parent;
        public Zone zone;
        
        public abstract void insert(Leaf<T> leaf);
        public abstract void upsert(Tree<T> node);
    }
    
    public static class Leaf<T> {
        public final Pos pos;
        public final T value;
        
        public Leaf(Pos pos, T value) {
            this.pos = pos;
            this.value = value;
        }
    }

    public static class Page<T> extends Tree<T> {
        public final ArrayList<Leaf<T>> leafs = new ArrayList<>();

        public static final int maxPageSize = 6;

        public void insert(Leaf<T> leaf) {
            leafs.add(leaf);
            if(leafs.size() > maxPageSize) {
                Page<T> newPage = split();
                if(parent == null) {
                    parent = new Node<T>();
                    parent.upsert(this);
                } else {
                    extendZone(leaf.pos);
                }
                parent.upsert(newPage);
            }
        }

        private Page<T> split() {
            Page<T> page = new Page<T>();
            for(int i = 0; i < leafs.size(); ++i) {
                Leaf<T> leaf = this.leafs.remove(leafs.size() - 1);
                page.insert(leaf);
            }
            return page;
        }

        public void upsert(Tree<T> node) {
            if(parent == null) {
                parent = new Node<T>();
                parent.upsert(this);
            }
            parent.upsert(node);
        }
    }

    public static class Node<T> extends Tree<T> {
        public final ArrayList<Tree<T>> childs = new ArrayList<>();

        public static final int maxChildsPerNode = 3;

        public void upsert(Tree<T> node) {
            if(childs.size() < maxChildsPerNode) {
                childs.add(node);
                node.parent = this;
                extendZone(node.zone);
            } else {
                if(parent == null) {
                    parent = new Node<T>();
                    parent.upsert(this);
                }
                parent.upsert(node);
            }
        }

        public void insert(Leaf<T> leaf) {
            Tree<T> child = chooseNearestChild(leaf.pos);
            child.insert(leaf);
            refreshZone();
        }

        private void refreshZone() {
            this.zone = nodesZone(this.childs);
        }

        public static Zone nodesZone(Iterable<Tree<T>> nodes) {
            Zone zone = Zone.none;
            for(Tree<T> node : nodes) {
                zone.extend(node.zone);
            }
            return zone;
        }
    }
}
