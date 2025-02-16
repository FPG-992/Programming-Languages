#include <iostream>
#include <fstream>
#include <cmath>

//this problem is very similar to the subset sum problem 

using namespace std;

#define MAX_SIZE 1000000

int main(int argc, char* argv[]){

int N; // number of elements in the array

int numbers[MAX_SIZE]; // array of numbers

int total_sum = 0; // total sum of the array


ifstream file(argv[1]); // open the file

if (!file.is_open()){
    cout << "Error opening file" << endl;
    return 1;
}else {
    file>>N;
}

for (int i=0; i<N; i++){
    file>>numbers[i];
    total_sum += numbers[i];
}

bool dp[total_sum+1] = {false};
dp[0] = true; // can always have a sum of 0 (using empty list)

for (int i=0; i<N; i++){
    for (int j=total_sum; j>=numbers[i]; j--){
        dp[j] = dp[j] or dp[j-numbers[i]]; //ekmetaleuomaste thn idiothta pou uparxei sto provlima (subset sum problem) source: video
    }
}

int closest = 0;
int target = total_sum/2;

for (int i=target; i>=0; i--){ //me to pou vriskei to pio kontino, spaei to loop, den uparxei logos gia parapanw search
    if (dp[i]==1){
        closest = i; 
        break;
    }
}

cout<<abs(2*closest - total_sum)<<endl;


return 0;

}