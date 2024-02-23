#include <iostream>
#include <vector>

using namespace std;

void enumerate_3_cycles(vector<vector<bool> >& graph) {
    int n = graph.size();
    // Verificăm fiecare triplet de noduri posibile
    for (int u = 0; u < n; u++) {
        for (int v = u + 1; v < n; v++) {
            if (graph[u][v]) {
                for (int w = v + 1; w < n; w++) {
                    if (graph[v][w] && graph[w][u]) {
                        cout << "(" << u << ", " << v << ", " << w << ")" << endl;
                    }
                }
            }
        }
    }
}

int main() {
    // Definim graful K6 minus muchia (1,2)
    vector<vector<bool> > graph;
    graph.resize(6, vector<bool>(6, false));
    graph[0][1] = true;
    graph[0][2] = true;
    graph[0][3] = true;
    graph[0][4] = true;
    graph[0][5] = true;
    graph[1][3] = true;
    graph[1][4] = true;
    graph[1][5] = true;
    graph[2][3] = true;
    graph[2][4] = true;
    graph[2][5] = true;
    graph[3][4] = true;
    graph[3][5] = true;
    graph[4][5] = true;

    enumerate_3_cycles(graph);

    return 0;
}
