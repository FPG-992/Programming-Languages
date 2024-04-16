#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

using namespace std;

int main(int argc, char* argv[]){

    int N;

    //open the file named argv[1].txt
    ifstream file(argv[1]);
    
    file>>N;

    //malloc an array for N integers

    int *arr = new int[N];

    //read the integers from the file and store them in the array
    for(int i=0; i<N; i++){
        file>>arr[i];
    }





    //close the file
    file.close();

    //free the memory allocated for the array
    delete[] arr;

    //return 0
    return 0;
}