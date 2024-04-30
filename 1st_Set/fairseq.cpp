#include <iostream>
#include <fstream>
#include <cmath>

//this problem is very similar to the subset sum problem but with a few differences
//we will use dp to find the absolute difference of of S' and S

using namespace std;

#define MAX_SIZE 1000000

int main(int argc, char* argv[]){

int N; // number of elements in the array

int numbers[MAX_SIZE]; // array of numbers

ifstream file(argv[1]); // open the file

if (!file.is_open()){
    cout << "Error opening file" << endl;
    return 1;
}else {
    file>>N;
}

for (int i=0; i<N; i++){
    file>>numbers[i];
}

//we've read the file till here. Now we will start the dp process




file.close(); // close the file

}