
import java.util.*;

/*
main = do
    numBalls <- getLine <&> words <&> head <&> read :: IO Int
    numQueries <- getLine <&> read :: IO Int
    queryResults <- getQueryResults numQueries
    colors <- markColors queryResults
    trimmedColors <- reduceColors queryResults colors
    case majority trimmedColors of
      White -> index White colors
      Black -> index Black colors
      otherwise -> putStrLn (nextQuery numQueries)
*/

class Oracle1 {
    public static enum Color {
        white,
        black,
        idk,
        none,
    }

    public static class QueryResult {
        public final int index1;
        public final int index2;
        public final boolean same;

        public QueryResult(int index1, int index2, boolean same) {
            this.index1 = index1;
            this.index2 = index2;
            this.same = same;
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int numBalls = scanner.nextInt();
        int numQueries = scanner.nextInt();
        scanner.nextLine();

        List<QueryResult> queryResults = new ArrayList<QueryResult>();
        for(int i = 0; i < numQueries; ++i) {
            int index1 = scanner.nextInt();
            int index2 = scanner.nextInt();
            String result = scanner.nextLine();

            QueryResult qr = new QueryResult(index1, index2, result.indexOf("YES") >= 0);
            queryResults.add(qr);
        }

        ArrayList<Color> colors = new ArrayList<Color>();
        colors.set(0, Color.white);
        for(int i = 1; i < numBalls; ++i) {
            colors.set(i, Color.idk);
        }

        // spread colors
        for(QueryResult qr : queryResults) {
            Color color1 = colors.get(qr.index1);
            Color color2 = colors.get(qr.index2);

            if(color1 == Color.white && color2 == Color.idk) {
                colors.set(qr.index2, qr.same ? Color.white : Color.black);
            }

            if(color1 == Color.black && color2 == Color.idk) {
                colors.set(qr.index2, qr.same ? Color.black : Color.white);
            }

            if(color1 == Color.idk && color2 == Color.white) {
                colors.set(qr.index1, qr.same ? Color.white : Color.black);
            }

            if(color1 == Color.idk && color2 == Color.black) {
                colors.set(qr.index1, qr.same ? Color.black : Color.white);
            }
        }

        // reject mixed pairs
        for(QueryResult qr : queryResults) {
            Color color1 = colors.get(qr.index1);
            Color color2 = colors.get(qr.index2);

            if(color1 == Color.idk && color2 == Color.idk && !qr.same) {
                colors.set(qr.index1, Color.none);
                colors.set(qr.index2, Color.none);
            }
        }

        // count the final colors
        int numWhites = 0;
        int numBlacks = 0;
        int numCells = 0;
        for(Color color : colors) {
            switch(color) {
                case white: ++numCells; ++numWhites; break;
                case black: ++numCells; ++numBlacks; break;
                case idk: ++numCells; break;
            }
        }

        if(numWhites * 2 >= numCells) {
            System.out.println("0");
        } else if(numBlacks * 2 >= numCells) {
            System.out.println(colors.indexOf(Color.black));
        } else {
            System.out.println("0 1");
        }
    }
}
