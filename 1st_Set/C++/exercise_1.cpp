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

    vector<int> sequence(N);    
    vector<long long> prefix_sum(N+1,0); 

    while(!file.eof()){
        for(int i=0; i<N; i++){
            file>>sequence[i];
            prefix_sum[i+1] = prefix_sum[i] + sequence[i];
        }
    }

    long long totalSum = prefix_sum[N]; //total sum of the sequence
    long long minSum = totalSum; //initialize the minSum with the total sum

    //solution of (O(n^2)) | 2 FOR LOOPS

    for (int i=1; i<=N; i++){
        for (int j=i; j<N; j++){
            long long subsequence_sum = prefix_sum[j] - prefix_sum[i-1];
            long long non_subsequence_sum = totalSum - subsequence_sum;
            long long difference = abs(subsequence_sum - non_subsequence_sum);
            if (difference < minSum){
                minSum = difference;
            }
        }
    }

    cout<<minSum<<endl;









    //close the file
    file.close();

    //return 0
    return 0;
}