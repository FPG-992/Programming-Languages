#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

/*
The goal is to find the closest sum of a subset of the integers
to the target value which is half the total sum.

We try to find a subsequence as close as possible
to the target value to reduce the complexity of the problem. 

We use past results to find the closest sum to the target value
to avoid recomputing all the possible sums.
*/
using namespace std;

int main(int argc, char* argv[]){

int N; //number of integers

ifstream file(argv[1]); //open file

long long totalsum=0; //create a variable to store the sum of the integers

long long target = 0; //create a variable to store the target value that is half of the sum

long long closestsum = 0; //create a variable to store the closest sum to the target value  found so far

long long mindifference = -1; //create a variable to store the minimum difference between the closest sum and the target value

if(!file.is_open()){ //check if file is not open
    cout << "Error: file not found" << endl; //print error message
    return 1; //exit
}else {
    file>>N; //read the number of integers
}

vector <int> sequence(N); //create a vector of integers

for (int i=0; i<N; i++){ //loop over the integers
file>>sequence[i]; //read the integers
totalsum += sequence[i]; //sum the integers
} //end of loop

file.close(); //close the file

target = totalsum/2; //calculate the target value which is half of the total sum

vector <bool> dp(target+1, false); //create a vector of booleans to store the results
//we initialize it to false and the size is target+1
//because we want to store the results for all the possible sums

dp[0] = true; //it is possible for a sum to be 0 if there are no elements in the subset

//dp array is used to store the results

for (int num : sequence){ //iterate over each integer potentially in the subset to find the closest sum to the target value
    for (int sum = target; sum>=num; --sum){ //prevents same number from being used twice, avoiding revisiting 
        if (dp[sum-num]){ //check if a subsequence with sum equal to sum-num exists, adding num to it will give a sum equal to sum
            dp[sum] = true; //set the value to true because there is a subsequence by adding num equal to sum
        }
    }
}

for (int sum = target; sum>=0; --sum){
    if (dp[sum]){ //check if there is a subsequence with sum equal to sum or closest to sum
        closestsum = sum; //set the closest sum to sum
        break; //exit the loop on the first true sum found
    }
}


if (closestsum > target) {
    mindifference =abs(2 * closestsum - totalsum);
} else {
    mindifference = abs(totalsum - 2 * closestsum);
}

cout<<mindifference<<endl; //print the minimum difference

return 0;
}
