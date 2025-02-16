import java.io.*;
import java.util.*;

public class Moves {
    static class Point {
        int x, y;
        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }
        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    static int[] dx = {-1, -1, -1, 0, 1, 1, 1, 0};
    static int[] dy = {-1, 0, 1, 1, 1, 0, -1, -1};
    static String[] directions = {"NW", "N", "NE", "E", "SE", "S", "SW", "W"};

    public static void main(String[] args) throws Exception {
        Scanner sc = new Scanner(new File(args[0]));
        int N = sc.nextInt();
        int[][] grid = new int[N][N];
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                grid[i][j] = sc.nextInt();
            }
        }
        sc.close();

        String result = bfs(grid, N);
        if (result.equals("IMPOSSIBLE")) {
            System.out.print("IMPOSSIBLE");
        } else {
            System.out.print('[');
            System.out.print(result);
            System.out.print(']');
        }
    }

    static String bfs(int[][] grid, int N) {
        Queue<Point> queue = new LinkedList<>();
        Map<Point, String> path = new HashMap<>();
        Point start = new Point(0, 0);
        queue.add(start);
        path.put(start, "");

        while (!queue.isEmpty()) {
            Point curr = queue.poll();
            int cx = curr.x;
            int cy = curr.y;
            if (cx == N - 1 && cy == N - 1) {
                return path.get(curr).substring(0, path.get(curr).length() - 1); // Remove the trailing comma
            }
            for (int i = 0; i < 8; i++) {
                int nx = cx + dx[i];
                int ny = cy + dy[i];
                if (nx >= 0 && nx < N && ny >= 0 && ny < N && grid[nx][ny] < grid[cx][cy]) {
                    Point next = new Point(nx, ny);
                    if (!path.containsKey(next)) {
                        queue.add(next);
                        path.put(next, path.get(curr) + directions[i] + ",");
                    }
                }
            }
        }
        return "IMPOSSIBLE";
    }
}
