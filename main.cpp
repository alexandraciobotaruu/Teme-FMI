#include "gestioneaza.hpp"

int main(){
    Produs* Margherita[5];
    int i=0;
    int k=1;
    while (k==1){
        cout<<"\n 1 Pentru a adauga un produs";
        int comanda;
        cin>> comanda;
        switch(comanda){
            case 1:{
            Margherita[i]= new Produs();
           //// cin >> *Margherita[i];
            i++;
            break;
        }
    }
    cout<<"Stiu ca nu am terminat, dar momentan nu ma mai pot concentra... O sa mai incerc sa lucrez pana la prezentare si clar pana la colocviu :)))";

}
}