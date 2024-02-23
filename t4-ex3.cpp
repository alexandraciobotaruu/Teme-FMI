#include <iostream>
#include <unordered_set>
#include <vector>
using namespace std;

vector<int> symmetricDifference(const vector<int>& set1, const vector<int>& set2) {
    unordered_set<int> hashTable;
    vector<int> result;

    // Adăugăm elementele din set1 în tabelul de dispersie
    for (vector<int>::const_iterator it = set1.begin(); it != set1.end(); ++it) {
        hashTable.insert(*it);
    }

    // Parcurgem set2 și verificăm dacă elementele există în tabelul de dispersie
    // Dacă nu există, le adăugăm în rezultat
    for (vector<int>::const_iterator it = set2.begin(); it != set2.end(); ++it) {
        if (hashTable.find(*it) == hashTable.end()) {
            result.push_back(*it);
        }
    }

    // Parcurgem set1 și verificăm dacă elementele există în tabelul de dispersie
    // Dacă nu există, le adăugăm în rezultat
    for (vector<int>::const_iterator it = set1.begin(); it != set1.end(); ++it) {
        if (hashTable.find(*it) == hashTable.end() && find(result.begin(), result.end(), *it) == result.end()) {
            result.push_back(*it);
        }
    }

    return result;
}

int main() {
    vector<int> set1;
    set1.push_back(1);
    set1.push_back(2);
    set1.push_back(3);
    set1.push_back(4);
    set1.push_back(5);

    vector<int> set2;
    set2.push_back(4);
    set2.push_back(5);
    set2.push_back(6);
    set2.push_back(7);
    set2.push_back(8);

    vector<int> diff = symmetricDifference(set1, set2);

    cout << "Diferenta simetrica: ";
    for (vector<int>::const_iterator it = diff.begin(); it != diff.end(); ++it) {
        cout << *it << " ";
    }
    cout << endl;

    return 0;
}
