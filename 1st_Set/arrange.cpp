#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>
#include <sstream>

using namespace std;


struct treenode {
    int data;
    struct treenode *left;
    struct treenode *right;
    treenode(int x) : data(x), left(nullptr), right(nullptr) {} 
};

treenode* constructor(istringstream& iss){
    int key; 
    if (!(iss>>key)||key==0){
        return nullptr;
    }
    treenode* node = new treenode(key);
    node->left = constructor(iss);
    node->right = constructor(iss);
    return node;  
}

void inorder(treenode* root, vector<int>& values_in_order){
    if (root==nullptr){
        return;
    }
    inorder(root->left,values_in_order);
    values_in_order.push_back(root->data); //store the values in inorder traversal
    inorder(root->right,values_in_order);
}

void optimize(treenode* root, int& key){
    if (root==nullptr){
        return;
    }
    optimize(root->left, key);
    root->data = key;
    key++;
    optimize(root->right, key);
}

bool swap(treenode* root){
if (!root || (!root->left && !root->right)){
    return false;
}

vector<int> original,swapped; //store the values of the original tree and the swapped tree
inorder(root,original);
swap(root->left,root->right);
inorder(root,swapped);

if (original>swapped){
    return true;
} else {
    swap(root->left,root->right);
    return false;
}
}

void optimize(treenode* root){
if (!root){
    return;
}
optimize(root->left);
optimize(root->right);
swap(root);
}

int main(int argc, char* argv[]){
string n_of_nodes;
string list_of_nodes;

vector <int> values_in_order; //store the values in inorder traversal


ifstream file(argv[1]);

if (!file.is_open()){
    cout<<"Error opening file"<<endl;
    return 1;
}{
    getline(file, n_of_nodes); //read number of nodes
    getline(file, list_of_nodes); //read the list of nodes
}

istringstream iss(list_of_nodes); //we do this because we want to read the string as a stream

treenode* root = constructor(iss); //ftiakse to dentraki mas apo tin lista

optimize(root);

values_in_order.clear();

inorder(root,values_in_order); //print the values in inorder traversal

for (size_t i=0; i<values_in_order.size(); i++){
    cout<<values_in_order[i]<<" ";
}cout<<endl;


file.close();

return 0; 

}