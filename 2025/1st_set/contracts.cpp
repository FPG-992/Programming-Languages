#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>

static const int SHIFT = 99999;
static const int SIZE = 2 * SHIFT + 1;

int main(int argc, char* argv[]) {

    std::ifstream fin(argv[1]);
    if (!fin) {
        std::cerr << "cannot open file\n";
        return 1;
    }

    int M = 0, K = 0;

    fin>>M>>K;

    const int N = M + K;

    std::vector<int> bucket(SIZE, 0);
    std::int64_t baseB = 0;

    for (int i = 0; i < N; ++i) {
        int a = 0, b = 0;
        fin >> a >> b;                         
        const int d = a - b;                  
        bucket[d + SHIFT] += 1;
        baseB += static_cast<std::int64_t>(b);
    }
    fin.close();

    std::int64_t gain = 0;                    
    int remaining = M;                       

    for (int idx = SIZE - 1; idx >= 0 && remaining > 0; --idx) {
        const int cnt = bucket[idx];
        if (cnt == 0) continue;

        const int take   = (cnt < remaining) ? cnt : remaining;
        const int value  = idx - SHIFT;
        gain      += static_cast<std::int64_t>(take) * value;
        remaining -= take;
    }

    const std::int64_t answer = baseB + gain;
    std::cout << answer << '\n';
    return 0;
}