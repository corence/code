
public class RangeSolver {
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

    public static class CellState {
        public final int state;

        public static final CellState gas = new CellState(1);
        public static final CellState liquid = new CellState(2);
        public static final CellState solid = new CellState(3);

        private CellState(int state) {
            this.state = state;
        }
    }
    
    public final Map<Pos, CellState> grid = new HashMap<>();
    public static class Cell {
        public final Pos pos;
        public CellState state;
    }

    public static class Hint {
        public int value;
        public List<Map<Pos, CellState>>
    }
    
    public static class Solver {
        public final Map<Pos, CellState> grid;
        public 
    }
}

