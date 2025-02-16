#same logic as in the C solution
import sys

def fairseq(filename):
    with open (filename,'r') as file:
        N = int(file.readline().strip())
        numbers = list(map(int, file.readline().strip().split()))

    total_sum = sum(numbers)
    target = total_sum // 2
    dp = [0] * (total_sum + 1)
    dp[0] = 1

    for num in numbers:
        for i in range(total_sum, num - 1, -1):
            dp[i] |= dp[i - num]

    closest_sum = 0

    for i in range(target, -1, -1):
        if dp[i]:
            closest_sum = i
            break

    result = abs(total_sum - 2 * closest_sum)
    print(result)

if __name__ == '__main__':
    fairseq(sys.argv[1])
