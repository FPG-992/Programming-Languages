#include <iostream>
#include <fstream>
#include <cmath>

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

//wrea mexri edw exume diavasei ta dedomena apo to arxeio

for (int i=0; i<N; i++){
    cout << numbers[i] << " ";
}




file.close(); // close the file

}