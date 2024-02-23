#include <iostream>
#include <vector>
#include <cstring>

using namespace std;
///////**** Clasa Abstracta ******///////////////
class Calc_pt{
public:
    virtual float calc_pret()=0;
};

class Produs{
protected:
    const float pret_furnizor;
    string nume;
    static int n; // n= nr ingrediente pe produs
    vector <string> produs;
    vector <float> pret_unit_total;
public:
    Produs();
    Produs& operator=(const Produs& p); //supraincarcare operator =
    Produs(const Produs&); 

    virtual ostream& Afisare(ostream&)const;
    virtual istream& Citire(istream&);
    string getNume(){return this->nume;}
    float calc_pret(){
        float pret_total=0;
        vector <float> final;
        for(int i=0; i!=pret_unit_total.size(); i++){
                pret_total=pret_total + pret_furnizor + pret_unit_total[i];
                final.push_back(pret_total);
        }
        cout<<" Pretul produsului este: " <<pret_total;
    }
};

///////**** Clasa Specializare ******///////////////
