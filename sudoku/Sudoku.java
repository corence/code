
import java.util.*;

public class Sudoku {
    public static class Pos {
        public int x;
        public int y;

        public Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public int hashCode() {
            return x * 100000 + y;
        }

        public boolean equals(Object rhs) {
            Pos that = (Pos)rhs;
            return this.x == that.x && this.y == that.y;
        }
    }

    public static class Cell {
        public final Pos pos;

        public int value = 0;
        public final List<Integer> possibilities;
        public final List<Group> groups;

        public Cell(Pos pos, List<Integer> possibilities) {
            this.pos = pos;
            this.possibilities = new ArrayList<Integer>(possibilities);
            this.groups = new ArrayList<Group>();
        }
    }

    public static class Group {
        public final List<Cell> cells;

        public Group(List<Cell> cells) {
            this.cells = new ArrayList<Cell>(cells);
        }
    }

    public final Map<Pos, Cell> cells;
    public final ArrayList<Group> groups;

    public void addGroup(Group group) {
        this.groups.add(group);
    }

    public Sudoku() {
        this.cells = new HashMap<Pos, Cell>();
        this.groups = new ArrayList<>();

        for(int y = 0; y < 9; ++y) {
            for(int x = 0; x < 9; ++x) {
                Pos pos = new Pos(x, y);
                Cell cell = new Cell(pos, Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));
                cells.put(pos, cell);
            }
        }

        initRows();
        initCols();
        initSquares();
    }

    private void initRows() {
        for(int y = 0; y < 9; ++y) {
            ArrayList<Cell> cells = new ArrayList<Cell>();
            for(int x = 0; x < 9; ++x) {
                cells.add(this.cells.get(new Pos(x, y)));
            }
            addGroup(new Group(cells));
        }
    }

    private void initCols() {
        for(int x = 0; x < 9; ++x) {
            ArrayList<Cell> cells = new ArrayList<Cell>();
            for(int y = 0; y < 9; ++y) {
                cells.add(this.cells.get(new Pos(x, y)));
            }
            addGroup(new Group(cells));
        }
    }

    private void initSquares() {
        for(int yy = 0; yy < 3; ++yy) {
            for(int xx = 0; xx < 3; ++xx) {
                ArrayList<Cell> cells = new ArrayList<Cell>();
                for(int y = 0; y < 3; ++y) {
                    for(int x = 0; x < 3; ++x) {
                        cells.add(this.cells.get(new Pos(xx * 3 + x, yy * 3 + y)));
                    }
                }
                addGroup(new Group(cells));
            }
        }
    }

    public String display() {
        StringBuilder sb = new StringBuilder();

        sb.append(' ');
        for(int i = 0; i < 35; ++i) {
            sb.append('-');
        }
        sb.append("\n");

        for(int y = 0; y < 9; ++y) {
            for(int x = 0; x < 9; ++x) {
                Cell cell = this.cells.get(new Pos(x, y));
                System.out.println(x + ", " + y);
                if(!cell.possibilities.isEmpty()) {
                    sb.append("|" + cell.possibilities.get(0) + cell.possibilities.get(1) + cell.possibilities.get(2));
                }
            }
            sb.append("|\n");

            for(int x = 0; x < 9; ++x) {
                Cell cell = this.cells.get(new Pos(x, y));
                if(cell.possibilities.isEmpty()) {
                    sb.append("| " + cell.value + " ");
                } else {
                    sb.append("|" + cell.possibilities.get(3) + cell.possibilities.get(4) + cell.possibilities.get(5));
                }
            }
            sb.append("|\n");

            for(int x = 0; x < 9; ++x) {
                Cell cell = this.cells.get(new Pos(x, y));
                if(!cell.possibilities.isEmpty()) {
                    sb.append("|" + cell.possibilities.get(6) + cell.possibilities.get(7) + cell.possibilities.get(8));
                }
            }
            sb.append("|\n ");

            for(int i = 0; i < 35; ++i) {
                sb.append('-');
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    public static void main(String[] args) {
        System.out.println(new Sudoku().display());
    }
}
