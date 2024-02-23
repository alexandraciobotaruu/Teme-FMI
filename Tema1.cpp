#include <iostream>
#include<cstring>
#include <vector>

using namespace std;

class Punct2D
{
  int x,y;
public:
    Punct2D();
    Punct2D(int x1);
    Punct2D(int x1, int y1);
    friend istream& operator>>(istream&, Punct2D&);
    friend ostream& operator<<(ostream&, Punct2D&);
    Punct2D& operator+=(int);
    Punct2D(const Punct2D&);
    Punct2D& operator=(const Punct2D&);
};

class Figura{
    char * denumire;
    int nrPuncte;
    Punct2D *P;
public:
    Figura();
    Figura(char p[255]);
    Figura(const Figura&);
    ~Figura();
    char* get_denumire() {return denumire;}
    Figura& operator=(const Figura&);
  //  const Figura operator+(const Punct2D&);
    friend const Figura operator+(const Punct2D&, const Figura&);
    const Figura operator+(const Figura&);
    int operator==(Figura&);
    friend istream& operator>>(istream &,  Figura&);
    friend ostream& operator<<(ostream &, Figura&);
    void sortare_figura();
};


int main(){
    Punct2D p1(-1,1),p2(2),p3[4], p4[2];
    for( int i=0;i<4;i++) 
         cin>>p3[i]; 
    cout<< p1 <<" "<<p2<< " "<<p3<<" "<<p4;
    p4[0] = p1;
    p4[1] = p2; 
    Figura f1(“triunghi dreptunghic”,3), f2(“Segment”,2); 

    return 0;
}

Punct2D::Punct2D(){x = 0; y = 0;}
Punct2D::Punct2D(int x1){x=x1;}
Punct2D::Punct2D(int x1, int y1){x=x1; y=y1;}
      //constructorii 
Punct2D::Punct2D(const Punct2D& ob)
{
    x = ob.x;
    y = ob.y;
}    
     //constructorul de copiere 
Punct2D& Punct2D::operator=(const Punct2D& ob)
{
    if(this != &ob)
    {
        x = ob.x;
        y = ob.y;
    }
    return *this;
} 
// atribuire spatiu constructor dupa distrugere 
Punct2D& Punct2D::operator+=(int x1)
{
    x = x1;
    return *this;
}
// adaugare 

istream& operator>>(istream& in, Punct2D& ob)
{
    in >> ob.x;
    in >> ob.y;
    return in;
}
ostream& operator<<(ostream& out, Punct2D& ob)
{
    out<<"("<<ob.x<<","<<ob.y<<")"<<endl;
 
    return out;
}



Figura::Figura(){denumire = ""; nrPuncte= 0; P=NULL;} //constructor 1
Figura::Figura(char p[255]){denumire = p; nrPuncte = 0; P=NULL;}
Figura::Figura(const Figura& f)
{
    denumire = new char[strlen(f.denumire) + 1];
    strcpy(denumire, f.denumire);
    nrPuncte = f.nrPuncte;
}
Figura::~Figura()
{
    delete[] denumire;
    denumire=NULL;
    if( P != NULL)
    {
        delete[] P;
        P = NULL;
    }
}
/*
Figura::Figura(const Figura& f)
{
        denumire = f.denumire;
        nrPuncte = f.nrPuncte;
        P = new Punct2D[nrPuncte];
        for(int i = 0; i<nrPuncte; i ++)
        {
            P[i] = f.P[i];
        }
}
*/

Figura& Figura::operator=(const Figura& f)
{
    if(this != &f)
    {
        denumire = f.denumire;
        nrPuncte = f.nrPuncte;
        P = new Punct2D[nrPuncte];
        for(int i = 0; i<nrPuncte; i ++)
        {
            P[i] = f.P[i];
        }
    }
        return *this;
}
istream& operator>>(istream& in, Figura& f)
{
    char aux[255];
    in>>aux;
    f.denumire = new char[strlen(aux)+1];
    strcpy(f.denumire, aux);
    in >> f.nrPuncte;
    return in;
}
ostream& operator<<(ostream& out, Figura& f)
{
    out<<f.denumire<<" "<<f.nrPuncte<<endl;
    return out;
}
/*const Figura Figura::operator+(const Punct2D& p)
{
    Figura F;
    F.denumire = this-> denumire;
    F.nrPuncte=this-> nrPuncte +1;
    F.P = new Punct2D[F.nrPuncte];
    for(int  i= 0; i < F.nrPuncte - 1; i++)
    {
        F.P[i] = P[i];
    }
    F.P[F.nrPuncte - 1] = p;
    return F;
}
*
istream& operator>>(istream& in, Figura& f)
{
    in >>f.denumire>>f.nrPuncte;
    f.P = new Punct2D[f.nrPuncte];
    for(int i = 0; i < f.nrPuncte; i++)
    {
        in >>f.P[i];
    }
    return in;
}
ostream& operator<<(ostream& out, Figura& f)
{
    out<<f.denumire<<endl;
    for(int  i = 0; i < f.nrPuncte; i ++)
    {
        out<<f.P[i];
    }
    out << endl;
    return out;
}

/// rest

void Figura::sortare_figura()
{
    if (nrPuncte=2)
       denumire ="Segment";
    if (nrPuncte=3)
       denumire ="Triunghi dreptunghic";

}
/*
const Figura operator+(const Punct2D& p, const Figura& f)
{
    Figura F;
    F.denumire = f.denumire;
    F.nrPuncte = f.nrPuncte + 1;
    F.P = new Punct2D[f.nrPuncte];
    for(int  i= 0; i < f.nrPuncte - 1; i++)
    {
        F.P[i] = f.P[i];
    }
    F.P[f.nrPuncte - 1] = p;
    return F;
}

int Figura::operator==(Figura& f)
{
    if (denumire != f.denumire) return 0;
    if (nrPuncte != f.nrPuncte) return 0;
    sortare_figura();
    f.sortare_figura();
    for(int i = 0; i<nrPuncte; i++)
    return 1;
}
*/
/*
const Figura Figura::operator+(const Figura& f)
{
    if (this->denumire != f.denumire) {cout<<endl<<"figuri diferite, nu se pot concatena listele de studenti\n"; }

        Figura F;
        F.denumire = this-> denumire;
        F.P = new Punct2D[this->nrPuncte + f.nrPuncte + 1];
        for(int  i= 0; i < this->nrPuncte; i++) F.P[i] = P[i];
        for(int  i= 0; i < F.nrPuncte; i++) F.P[this->nrPuncte + i] = f.P[i];
        F.nrPuncte = this->nrPuncte + f.nrPuncte;
    return F;
}
*/