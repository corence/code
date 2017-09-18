
import java.util.*;

class Signpost {
    public static class Pos {
        public final int x;
        public final int y;

        public Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override public boolean equals(Object rhs) {
            Pos that = (Pos)rhs;
            return this.x == that.x && this.y == that.y;
        }

        @Override public int hashCode() {
            return y << 32 | x;
        }

        @Override public String toString() {
            return "(" + x + "," + y + ")";
        }

        public Pos add(Pos that) {
            return new Pos(this.x + that.x, this.y + that.y);
        }

        public static Pos nowhere = new Pos(0, 0);
        public static Pos north = new Pos(0, -1);
        public static Pos northwest = new Pos(-1, -1);
        public static Pos west = new Pos(-1, 0);
        public static Pos southwest = new Pos(-1, 1);
        public static Pos south = new Pos(0, 1);
        public static Pos southeast = new Pos(1, 1);
        public static Pos east = new Pos(1, 0);
        public static Pos northeast = new Pos(1, -1);
    }

    public static class Cell {
        public final Pos pos;
        public final Pos direction;
        public final Integer value;
        public final ListSet<Cell> predecessors;
        public final ListSet<Cell> successors;
        public Chain chain;

        public Cell(Pos pos, Pos direction, Integer value) {
            this.pos = pos;
            this.direction = direction;
            this.value = value;
            this.predecessors = new ListSet<>();
            this.successors = new ListSet<>();
            this.chain = new Chain(this);
        }

        @Override
        public int hashCode() {
            return pos.hashCode();
        }

        @Override
        public boolean equals(Object rhs) {
            Cell that = (Cell)rhs;
            return this.pos.equals(that.pos);
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            sb.append("{");
            sb.append(this.pos).append(" ").append(this.direction);
            sb.append(" (" + this.value + ") ");
            sb.append(dump(poses(this.predecessors)) + ", " + dump(poses(this.successors)));
            sb.append("}");
            return sb.toString();
        }

        public static List<Pos> poses(Iterable<Cell> cells) {
            ArrayList<Pos> result = new ArrayList<>();
            for(Cell cell : cells) {
                result.add(cell.pos);
            }
            return result;
        }
    }

    public static class Chain {
        public List<Cell> cells;

        public Chain(Cell cell) {
            this.cells = new ArrayList<>();
            this.cells.add(cell);
        }

        public String toString() {
            return dump(cells);
        }
    }

    public static class Action {
        public final Cell cell1;
        public final Cell cell2;
        public final boolean shouldLink;

        public Action(Cell cell1, Cell cell2, boolean shouldLink) {
            this.cell1 = cell1;
            this.cell2 = cell2;
            this.shouldLink = shouldLink;
        }
    }

    public final Map<Pos, Cell> posToCell = new HashMap<>();
    public final Set<Cell> dirtyCells = new HashSet<>();

    public static List<Action> solveCell(Cell cell) {
        // find all of the actions that could arise from this cell.
        // let's start with successors that have only one predecessor

        List<Action> results = new ArrayList<>();
        for(Cell s : cell.successors) {
            if(cell.successors.size() == 1) {
                System.out.println("gonna link " + cell.pos + " to " + s.pos + " because succ=1");
                results.add(new Action(cell, s, true));
            }

            if(cell.value != null && s.value != null) {
                if(cell.value + 1 == s.value) {
                    System.out.println("gonna link " + cell.pos + " to " + s.pos + " because value follows");
                    results.add(new Action(cell, s, true));
                } else {
                    System.out.println("gonna unlink " + cell.pos + " to " + s.pos + " because value follows not");
                    results.add(new Action(cell, s, false));
                }
            }
        }

        for(Cell p : cell.predecessors) {
            if(cell.predecessors.size() == 1) {
                System.out.println("gonna link " + p.pos + " to " + cell.pos + " because pred=1");
                results.add(new Action(p, cell, true));
            }

            if(cell.value != null && p.value != null) {
                if(p.value + 1 == cell.value) {
                    System.out.println("gonna link " + p.pos + " to " + cell.pos + " because value follows");
                    results.add(new Action(p, cell, true));
                } else {
                    System.out.println("gonna unlink " + p.pos + " to " + cell.pos + " because value follows not");
                    results.add(new Action(p, cell, false));
                }
            }
        }

        return results;
    }

    public static Cell createCell(int x, int y, Pos direction, Integer value) {
        return new Cell(new Pos(x, y), direction, value);
    }

    public static final Cell[] sample = new Cell[] {
        createCell(0, 0, Pos.east, 1),
        createCell(1, 0, Pos.southwest, null),
        createCell(2, 0, Pos.southwest, null),
        createCell(0, 1, Pos.east, null),
        createCell(1, 1, Pos.north, null),
        createCell(2, 1, Pos.southwest, null),
        createCell(0, 2, Pos.northeast, null),
        createCell(1, 2, Pos.east, null),
        createCell(2, 2, Pos.nowhere, 9)
    };

    public Signpost(Cell[] cells) {
        for(Cell cell : cells) {
            this.posToCell.put(cell.pos, cell);
            this.dirtyCells.add(cell);
        }
        initializeCellLinks();
    }

    public Signpost(Iterable<Cell> cells) {
        for(Cell cell : cells) {
            this.posToCell.put(cell.pos, cell);
            this.dirtyCells.add(cell);
        }
        initializeCellLinks();
    }

    private void initializeCellLinks() {
        for(Cell cell : this.posToCell.values()) {
            for(Cell successor : this.scan(cell.pos, cell.direction)) {
                if(successor == cell) continue;
                System.out.println("adding successor " + successor + " to cell " + cell);
                cell.successors.add(successor);
                successor.predecessors.add(cell);
            }
        }
    }

    public List<Cell> scan(Pos pos, Pos direction) {
        List<Cell> results = new ArrayList<Cell>();
        if(direction == Pos.nowhere) {
            return results;
        }

        while(true) {
            pos = pos.add(direction);
            Cell result = posToCell.get(pos);
            if(result != null) {
                results.add(result);
            } else {
                break;
            }
        }
        return results;
    }

    public static <T> T pop(Collection<T> collection) {
        Iterator<T> it = collection.iterator();
        T result = it.next();
        it.remove();
        return result;
    }

    public void solve() {
        while(!dirtyCells.isEmpty()) {
            System.out.println(dump(this));
            System.out.print("dirties: " + dirtyCells.size() + ". ");
            Cell cell = pop(dirtyCells);
            System.out.println("Checking " + cell);
            for(Action action : solveCell(cell)) {
                Cell cell1 = action.cell1;
                Cell cell2 = action.cell2;
                if(action.shouldLink) {
                    link(cell1, cell2);
                } else {
                    System.out.println("cutting " + cell1 + " successor" + cell2);
                    if(cell1.successors.remove(cell2)) {
                        System.out.println("x3!");
                        this.dirtyCells.add(cell1);
                    }

                    System.out.println("cutting " + cell2 + " predecessor" + cell1);
                    if(cell2.predecessors.remove(cell1)) {
                        System.out.println("x4!");
                        this.dirtyCells.add(cell2);
                    }
                }
            }
        }
    }

    public void link(Cell cell1, Cell cell2) {
        if(!cell1.successors.isEmpty()) {
            if(!cell1.successors.contains(cell2)) {
                throw new IllegalStateException("nope1");
            }
            cell1.successors.clear();
            this.dirtyCells.add(cell1);
        }
        if(!cell2.predecessors.isEmpty()) {
            if(!cell2.predecessors.contains(cell1)) {
                throw new IllegalStateException("nope2");
            }
            cell2.predecessors.clear();
            this.dirtyCells.add(cell2);
        }
        if(cell1.chain.cells == cell2.chain.cells) {
            throw new IllegalStateException("nope3");
        }
        cell1.chain.cells.addAll(cell2.chain.cells);
        cell2.chain.cells = cell1.chain.cells;
        cell2.chain = cell1.chain;
    }

    public static <T> boolean assignSetContents(Set<T> set, T value) {
        System.out.println("assignSetContents(" + dump(set) + ", " + value);

        if(!set.contains(value)) {
            throw new IllegalStateException("whaaaat. Setting " + dump(set) + " but it lacks value " + value);
        }

        if(set.size() == 1) {
            return false;
        }

        set.clear();
        set.add(value);
        return true;
    }

    public static void main(String[] args) {
        Signpost sp = new Signpost(sample);
        sp.solve();

        System.out.println("outcome!");
        System.out.println(dump(sp));
    }

    public static <T> String dump(Iterable<T> collection) {
        final StringBuilder sb = new StringBuilder("[");
        final String filler = ", ";
        for(T t : collection) {
            sb.append(t.toString()).append(filler);
        }
        if(sb.length() > filler.length()) {
            sb.setLength(sb.length() - filler.length());
        }
        sb.append("]");
        return sb.toString();
    }

    public static String dump(Signpost sp) {
        final StringBuilder sb = new StringBuilder("<\n");
        final ListSet<List<Cell>> chains = new ListSet<>();
        for(Cell cell : sp.posToCell.values()) {
            sb.append(cell).append("\n");
            chains.add(cell.chain.cells);
        }
        sb.append("Chains:\n");
        for(List<Cell> chain : chains) {
            sb.append(dump(Cell.poses(chain))).append("\n");
        }
        sb.append(">\n");
        return sb.toString();
    }
}
