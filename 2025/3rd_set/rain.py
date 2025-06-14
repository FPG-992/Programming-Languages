import sys

def trap(h):
    left, right = 0, len(h) - 1
    left_max = right_max = 0
    water = 0
    while left < right:
        if h[left] < h[right]:
            if h[left] >= left_max:
                left_max = h[left]
            else:
                water += left_max - h[left]
            left += 1
        else:
            if h[right] >= right_max:
                right_max = h[right]
            else:
                water += right_max - h[right]
            right -= 1
    return water

def main():
    with open(sys.argv[1], encoding="utf-8") as f:
        n = int(f.readline().strip())
        heights = list(map(int, f.readline().split()))
        if n != len(heights):
            sys.exit("Error")

    print(trap(heights))

if __name__ == "__main__":
    main()