#include "produs.hpp"

template <class M>
class Meniu{
protected:
   vector <Produs> Prod;
public:
   void adauga_produs();
   void afisare_meniu();
   Meniu& operator+=(i);
};