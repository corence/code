
import java.util.*;

public class Sudoku {
    public static <T> String dump(Iterable<T> iterable) {
        StringBuilder sb = new StringBuilder("[");
        for(T t : iterable) {
            sb.append(t.toString()).append(",");
        }
        if(sb.length() > 1) {
            sb.setLength(sb.length() - 1);
        }
        sb.append("]");
        return sb.toString();
    }

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

        public String toString() {
            return "{" + x + "," + y + "}";
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

        public void setValue(Integer newValue) {
            if(!possibilities.contains(newValue)) {
                throw new IllegalStateException("value " + newValue + " doesn't exist in " + possibilities.size() + " possibilities");
            }

            if(value > 0) {
                throw new IllegalStateException("can't set value to " + newValue + " because it is already " + value);
            }

            if(newValue == 0) {
                throw new IllegalArgumentException("what? you can't set a cell to 0");
            }

            value = newValue;
            //System.out.println("clearing possibilities: " + dump(possibilities));
            possibilities.clear();

            //System.out.println("groups: " + groups.size());
            for(Group group : groups) {
                //System.out.println("cells: " + group.cells.size());
                for(Cell cell : group.cells) {
                    //System.out.println("removing " + newValue + " from a cell at " + cell.pos + " with possibilities: " + dump(cell.possibilities));
                    cell.possibilities.remove(newValue);
                    if(cell.possibilities.isEmpty() && cell.value == 0) {
                        throw new IllegalStateException("oops we cut too much from cell " + cell.pos + " -- it's empty now");
                    }
                }
                group.cells.remove(this);
            }
            groups.clear();
        }
    }

    public static class Group {
        public final List<Cell> cells;

        public Group(List<Cell> cells) {
            this.cells = new ArrayList<Cell>(cells);
            for(Cell cell : cells) {
                cell.groups.add(this);
            }
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

    public static <T> String displayElement(List<T> elements, int index) {
        if(index >= elements.size()) {
            return " ";
        } else {
            return elements.get(index).toString();
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
                sb.append('|');
                Cell cell = this.cells.get(new Pos(x, y));
                sb.append(displayElement(cell.possibilities, 0));
                sb.append(displayElement(cell.possibilities, 1));
                sb.append(displayElement(cell.possibilities, 2));
            }
            sb.append("|\n");

            for(int x = 0; x < 9; ++x) {
                sb.append('|');
                Cell cell = this.cells.get(new Pos(x, y));
                if(cell.possibilities.isEmpty()) {
                    sb.append(" " + cell.value + " ");
                } else {
                    sb.append(displayElement(cell.possibilities, 3));
                    sb.append(displayElement(cell.possibilities, 4));
                    sb.append(displayElement(cell.possibilities, 5));
                }
            }
            sb.append("|\n");

            for(int x = 0; x < 9; ++x) {
                sb.append('|');
                Cell cell = this.cells.get(new Pos(x, y));
                sb.append(displayElement(cell.possibilities, 6));
                sb.append(displayElement(cell.possibilities, 7));
                sb.append(displayElement(cell.possibilities, 8));
            }
            sb.append("|\n ");

            for(int i = 0; i < 35; ++i) {
                sb.append('-');
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    public static void addSampleBoard(Sudoku sudoku) {
        // 3x3:5_8h3_1_4c7b2b6_7f6_2a7_8b5e2b4_2a1_8f1_4b5b6c5_2_9h1_3
        sudoku.cells.get(new Pos(0, 0)).setValue(5);
        sudoku.cells.get(new Pos(0, 1)).setValue(8);
        sudoku.cells.get(new Pos(1, 1)).setValue(3);
        sudoku.cells.get(new Pos(1, 2)).setValue(1);
        sudoku.cells.get(new Pos(1, 3)).setValue(4);
        sudoku.cells.get(new Pos(1, 7)).setValue(7);
        sudoku.cells.get(new Pos(2, 1)).setValue(2);
        sudoku.cells.get(new Pos(2, 4)).setValue(6);
        sudoku.cells.get(new Pos(2, 5)).setValue(7);
        sudoku.cells.get(new Pos(3, 1)).setValue(1);
        sudoku.cells.get(new Pos(3, 3)).setValue(6);
        sudoku.cells.get(new Pos(3, 4)).setValue(2);
        sudoku.cells.get(new Pos(3, 6)).setValue(7);
        sudoku.cells.get(new Pos(3, 7)).setValue(8);
        sudoku.cells.get(new Pos(4, 1)).setValue(5);
        sudoku.cells.get(new Pos(4, 7)).setValue(2);
        sudoku.cells.get(new Pos(5, 1)).setValue(4);
        sudoku.cells.get(new Pos(5, 2)).setValue(2);
        sudoku.cells.get(new Pos(5, 4)).setValue(1);
        sudoku.cells.get(new Pos(5, 5)).setValue(8);
        sudoku.cells.get(new Pos(6, 3)).setValue(1);
        sudoku.cells.get(new Pos(6, 4)).setValue(4);
        sudoku.cells.get(new Pos(6, 7)).setValue(5);
        sudoku.cells.get(new Pos(7, 1)).setValue(6);
        sudoku.cells.get(new Pos(7, 5)).setValue(5);
        sudoku.cells.get(new Pos(7, 6)).setValue(2);
        sudoku.cells.get(new Pos(7, 7)).setValue(9);
        sudoku.cells.get(new Pos(8, 7)).setValue(1);
        sudoku.cells.get(new Pos(8, 8)).setValue(3);
    }

    public static void main(String[] args) {
        Sudoku sudoku = new Sudoku();
        addSampleBoard(sudoku);
        System.out.println(sudoku.display());
        for(int i=0; i<2000; ++i) {
            if(sudoku.isSolved()) break;
            System.out.println("* " + i + " *");
            sudoku.reduce();
            System.out.println(sudoku.display());
        }
    }

    public boolean isSolved() {
        for(Cell cell : this.cells.values()) {
            if(!cell.possibilities.isEmpty()) {
                return false;
            }
        }
        return true;
    }

    public void reduce() {
        reduceClusters();
        reduceUniques();
    }

//-- if a cluster of cells has that many possibilities, clean out the other cells within that group
    public void reduceClusters() {
        for(Group group : groups) {
            for(Cell keyCell : group.cells) {
                ArrayList<Cell> cellsMatchingKey = new ArrayList<Cell>();
                for(Cell otherCell : group.cells) {
                    if(keyCell.possibilities.containsAll(otherCell.possibilities)) {
                        cellsMatchingKey.add(otherCell);
                    }
                }
                if(cellsMatchingKey.size() == keyCell.possibilities.size()) {
                    // it's a match! cut these values from any other cell that contains them in the group
                    for(Cell victim : group.cells) {
                        if(cellsMatchingKey.contains(victim)) {
                            continue;
                        }
                        victim.possibilities.removeAll(keyCell.possibilities);
                    }
                    return;
                }
            }
        }
    }

//-- if a value only appears in one place in a group, solidify it
//-- a "unique" possibility is a possibility that only appears in one place in a group
    public void reduceUniques() {
        for(Group group : groups) {
            Map<Integer, ArrayList<Cell>> valueToCells = new HashMap<>();
            for(Cell cell : group.cells) {
                for(Integer value : cell.possibilities) {
                    ArrayList<Cell> things = valueToCells.get(value);
                    if(things == null) {
                        things = new ArrayList<Cell>();
                        valueToCells.put(value, things);
                    }
                    things.add(cell);
                }
            }

            for(Map.Entry<Integer, ArrayList<Cell>> entry : valueToCells.entrySet()) {
                Integer value = entry.getKey();
                ArrayList<Cell> things = entry.getValue();
                if(things.size() <= 1) {
                    things.get(0).setValue(value);
                    return;
                }
            }
        }
    }
}
