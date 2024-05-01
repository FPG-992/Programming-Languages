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



void inorder(treenode* root){
    if (root==nullptr){
        return;
    }
    inorder(root->left);
    cout<<root->data<<" ";
    inorder(root->right);
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

int main(int argc, char* argv[]){
string n_of_nodes;
string list_of_nodes;

ifstream file(argv[1]);

if (!file.is_open()){
    cout<<"Error opening file"<<endl;
    return 1;
}{
    getline(file, n_of_nodes);
    getline(file, list_of_nodes);
}

istringstream iss(list_of_nodes);

treenode* root = constructor(iss);

file.close();

inorder(root); //print original tree
cout<<endl; 

//print the tree tou vaggelara


return 0; 

}