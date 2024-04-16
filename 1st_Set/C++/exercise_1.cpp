#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <vector>

using namespace std;

int main(int argc, char* argv[]){

    int N;

    //open the file named argv[1].txt
    ifstream file(argv[1]);
    
    file>>N;

    vector<int> sequence(N); //initialize the sequence with N elements to store the numbers
    vector<long long> prefix_sum(N+1,0); //initialize the prefix sum with 0 to store the prefix sum
    //y3 = x3 + x2 + x1 + x0 ....

    while(!file.eof()){ //read the sequence and calculate the prefix sum
        for(int i=0; i<N; i++){ //read the sequence
            file>>sequence[i]; //store the sequence
            prefix_sum[i+1] = prefix_sum[i] + sequence[i]; //calculate the prefix sum
        }
    }

    long long totalSum = prefix_sum[N]; //total sum of the sequence, last element of the prefix sum
    long long minSum = totalSum; //initialize the minSum with the total sum, because the maximum difference can't be bigger than the total sum

    //solution of (O(n^2)) | 2 FOR LOOPS
    /* Nested loop to check for all possible subsequence sums and non-subsequence sums
    
    */

    for (int i=1; i<=N; i++){ 
        for (int j=i; j<N; j++){ 
            long long subsequence_sum = prefix_sum[j] - prefix_sum[i-1]; //calculate the subsequence sum
            long long non_subsequence_sum = totalSum - subsequence_sum; //calculate the non-subsequence sum
            long long difference = abs(subsequence_sum - non_subsequence_sum); //calculate the difference
            if (difference < minSum){ //if the difference is smaller than the minSum, update the minSum
                minSum = difference; //update the minSum
            }
        }
    }

    cout<<minSum<<endl; //print the minSum

    //close the file
    file.close();

    //return 0
    return 0;
}