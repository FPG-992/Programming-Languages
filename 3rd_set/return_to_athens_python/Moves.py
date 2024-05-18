import sys
import heapq as heapriorityqueue

DIRECTIONS = {
    'N': (-1, 0), 'S': (1, 0), 'E': (0, 1), 'W': (0, -1),
    'NE': (-1, 1), 'NW': (-1, -1), 'SE': (1, 1), 'SW': (1, -1)
}

def parse_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    n = int(lines[0].strip())
    grid = [list(map(int, line.split())) for line in lines[1:]]
    return n, grid

def is_valid(x, y, n):
    return 0 <= x < n and 0 <= y < n

def find_path(n, grid):
    start = (0, 0)
    goal = (n - 1, n - 1)
    priorityqueue = [(0, start, [])]
    visited = set()
    
    while priorityqueue:
        cost, (x, y), path = heapriorityqueue.heappop(priorityqueue)
        
        if (x, y) in visited:
            continue
        visited.add((x, y))
        
        if (x, y) == goal:
            return path
        
        for direction, (dx, dy) in DIRECTIONS.items():
            nx, ny = x + dx, y + dy
            if is_valid(nx, ny, n) and grid[nx][ny] < grid[x][y]:
                heapriorityqueue.heappush(priorityqueue, (cost + 1, (nx, ny), path + [direction]))
    
    return "IMPOSSIBLE"

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 moves.py <input_file>")
        return

    input_file = sys.argv[1]
    n, grid = parse_input(input_file)
    result = find_path(n, grid)
    if result == "IMPOSSIBLE":
        print(result)
    else:
        print('[' + ','.join(result) + ']')

if __name__ == "__main__":
    main()
