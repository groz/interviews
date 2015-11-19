/**
 * Imagine a matrix of numbers where 0 means water and 1 means land. All adjacent lands form an island.
 * Starting with the m x n space filled with water implement operation
 * int add(int row, int col)
 * that puts 1 into [row, col] position and returns the new number of islands.
 * Additional reading: https://en.wikipedia.org/wiki/Disjoint-set_data_structure
 */

import java.util.ArrayList;

public class Islands {
    final int LAND = 1;
    final int nRows, nCols;
    final int[][] ocean;
    final int[] id;
    final int[] mass;
    int nIslands = 0;

    public Islands(int nRows, int nCols) {
        this.nRows = nRows;
        this.nCols = nCols;
        ocean = new int[nRows][];

        for (int i = 0; i < nRows; ++i) {
            ocean[i] = new int[nCols];
        }

        id = new int[nRows * nCols];
        mass = new int[nRows * nCols];

        for (int i = 0; i < nRows * nCols; ++i) {
            id[i] = i;
            mass[i] = 1;
        }
    }

    int root(int p) {
        if (p != id[p]) {
            id[p] = root(id[p]); // path compression
        }
        return id[p];
    }

    boolean connect(int p, int q) {
        int proot = root(p);
        int qroot = root(q);
        if (proot == qroot)
            return false;

        if (mass[proot] > mass[qroot]) {
            id[qroot] = proot;
            mass[proot] += mass[qroot];
        } else {
            id[proot] = qroot;
            mass[qroot] += mass[proot];
        }
        return true;
    }

    int toIndex(int row, int col) {
        return row * nCols + col;
    }

    public int add(int row, int col) {
        if (ocean[row][col] == LAND)
            return nIslands;

        ocean[row][col] = LAND;

        int current = toIndex(row, col);
        ++nIslands;

        ArrayList<Integer> neighbors = new ArrayList<>();
        if (row > 0 && ocean[row - 1][col] == LAND) neighbors.add(toIndex(row - 1, col));
        if (row < nRows - 1 && ocean[row + 1][col] == LAND) neighbors.add(toIndex(row + 1, col));
        if (col > 0 && ocean[row][col - 1] == LAND) neighbors.add(toIndex(row, col - 1));
        if (col < nCols - 1 && ocean[row][col + 1] == LAND) neighbors.add(toIndex(row, col + 1));

        for (int n : neighbors) {
            if (connect(current, n)) {
                --nIslands;
            }
        }

        return nIslands;
    }

    // test
    public static void main(String[] args) {
        Islands is = new Islands(3, 3);
        System.out.println(is.add(0, 0)); // 1
        System.out.println(is.add(1, 1)); // 2
        System.out.println(is.add(0, 2)); // 3
        System.out.println(is.add(0, 1)); // 1
        System.out.println(is.add(1, 0)); // 1
        System.out.println(is.add(2, 2)); // 2
    }
}
