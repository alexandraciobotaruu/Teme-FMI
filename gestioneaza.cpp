#include "gestioneaza.hpp"

template <class M>
void Meniu<M>::adauga_produs(){
    Prod.push_back(new Produs);

   ///// *Prod.back().Citire();
}

template <class M>
void Meniu<M>::afisare_meniu(){
    cout<<"1. Adauga un nou produs citind de la tastatura tipul produsului si apoi campurile specifice ei\n";
    cout<<"2. Afiseaza meniul\n";

    int x;
    cout<< "Obtiunea? (1 sau 2)  ";
    cin>>x;
    while(x>=1 && x<=2){
        switch (x){
            case 1: { 
                string s;
                cout<<" Ce tip de produs doriti sa citici? Cu carne sau vegetarian?"
                cin >> s;
                if (s="cu carne") adauga_produs();
                else adauga_produs();
                break;
            }
            case 2:{
                for(int i=0; i<Prod.size();i++) 
                    cout << Prod[i];
                break;

            }
             cout<<"1. Adauga un nou produs citind de la tastatura tipul produsului si apoi campurile specifice ei\n";
             cout<<"2. Afiseaza meniul\n";
             cout<<"Alta optiune?  ";
             cin>>x;
             
        }

    }
}  