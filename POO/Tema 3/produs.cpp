#include "produs.hpp"


istream& Produs::Citire(istream& in){
    int n_0=0; // verificarm nr de ingrediente
    string ingredient;
    int cant;
    float pret_unitar;
    cout<<"Dati numele produsului: ";
    in>>this->nume;
    cout<<"Dati continutul produslui: ";
    for (n_0=0; n_0<n; n_0++){
        cout<< "Dati numele ingredientului: ";
        in >> ingredient;
        this -> produs.push_back(ingredient);
        cout << "Dati cantitatea ingredientului: ";
        in >> cant;
        cout << " Dati pretul unitar al ingredientului: ";
        in >> pret_unitar;
        this -> pret_unit_total.push_back(pret_unitar);
    }
    return in;

}
ostream& Produs::Afisare(ostream& out)const{
    out<<"Numele produsului este: "<<this->nume<<"\n";
    out<<"Ingredientele produsului sunt:";
    for (int i=0; i<1; i++)
    {
        out << this->produs[i];
    }
    return out;
}
Produs::Produs() :pret_furnizor(n)
{
    this->nume="Anonim";
}
Produs& Produs::operator=(const Produs& p)
{
    if(this!=&p)
    {
        this->nume=p.nume;
    }
    return *this;
}
Produs::Produs(const Produs& p) : pret_furnizor(n)
{
    this -> nume=p.nume;
    this -> n=p.n;
}

