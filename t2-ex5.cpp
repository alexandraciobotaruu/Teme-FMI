#include <iostream>
using namespace std;
 

struct Node {
    int data;
    Node* left;
    Node* right;
};

class BinarySearchTree {
private:
    Node* root;

public:
    BinarySearchTree() {
        root = nullptr;
    }

    void insertElement(int data) {
        root = insert(root, data);
    }

    Node* insert(Node* node, int data) {
        if (node == nullptr) {
            Node* newNode = new Node();
            newNode->data = data;
            newNode->left = nullptr;
            newNode->right = nullptr;
            return newNode;
        }

        if (data < node->data)
            node->left = insert(node->left, data);
        else if (data > node->data)
            node->right = insert(node->right, data);

        return node;
    }

    void intersect(BinarySearchTree& tree1, BinarySearchTree& tree2) {
        root = intersectNodes(tree1.root, tree2.root);
    }

    Node* intersectNodes(Node* node1, Node* node2) {
        if (node1 == nullptr || node2 == nullptr)
            return nullptr;

        if (node1->data < node2->data)
            return intersectNodes(node1->right, node2);
        else if (node1->data > node2->data)
            return intersectNodes(node1, node2->right);

        Node* newNode = new Node();
        newNode->data = node1->data;
        newNode->left = intersectNodes(node1->left, node2->left);
        newNode->right = intersectNodes(node1->right, node2->right);
        return newNode;
    }

    void displayIntersectedElements() {
        displayInorder(root);
    }

    void displayInorder(Node* node) {
        if (node != nullptr) {
            displayInorder(node->left);
            cout << node->data << " ";
            displayInorder(node->right);
        }
    }
};



 
int main() {
    BinarySearchTree tree1;
    BinarySearchTree tree2;
  
    // Insert elements into tree1
    tree1.insertElement(4);
    tree1.insertElement(2);
    tree1.insertElement(7);

    // Insert elements into tree2
    tree2.insertElement(2);
    tree2.insertElement(4);
    tree2.insertElement(3);

    // Create a new tree for the intersection
    BinarySearchTree intersectedTree;
    intersectedTree.intersect(tree1, tree2);

    // Display the intersected elements
    cout << "Intersected elements: ";
    intersectedTree.displayIntersectedElements();
    cout << endl;

    return 0;
}
