#include <iostream>
#include <vector>
using namespace std;

// Structura pentru o pereche (element, aparitii)
struct Pair {
    int element;
    int occurrences;
};
int partition(Pair arr[], int low, int high) {
    int pivot = arr[high].element;
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (arr[j].element <= pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }

    swap(arr[i + 1], arr[high]);
    return i + 1;
}

void quicksort(Pair arr[], int low, int high) {
    if (low < high) {
        int pivot = partition(arr, low, high);
        quicksort(arr, low, pivot - 1);
        quicksort(arr, pivot + 1, high);
    }
}
Pair* intersectArrays(Pair arr1[], int size1, Pair arr2[], int size2, int& sizeResult) {
    quicksort(arr1, 0, size1 - 1);
    quicksort(arr2, 0, size2 - 1);

    int i = 0, j = 0;
    Pair* result = new Pair[size1 + size2];
    sizeResult = 0;

    while (i < size1 && j < size2) {
        if (arr1[i].element < arr2[j].element) {
            i++;
        } else if (arr1[i].element > arr2[j].element) {
            j++;
        } else {
            result[sizeResult].element = arr1[i].element;
            result[sizeResult].occurrences = min(arr1[i].occurrences, arr2[j].occurrences);
            sizeResult++;
            i++;
            j++;
        }
    }

    return result;
}
int main() {
    Pair arr1[] = {{4, 1}, {2, 2}, {7, 3}};
    int size1 = sizeof(arr1) / sizeof(arr1[0]);

    Pair arr2[] = {{2, 2}, {4, 2}, {3, 1}};
    int size2 = sizeof(arr2) / sizeof(arr2[0]);

    int sizeResult;
    Pair* result = intersectArrays(arr1, size1, arr2, size2, sizeResult);

    cout << "Intersection: ";
    for (int i = 0; i < sizeResult; i++) {
        for (int j = 0; j < result[i].occurrences; j++) {
            cout << result[i].element << " ";
        }
    }
    cout << endl;

    delete[] result;

    return 0;
}
