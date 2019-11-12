
#include <factory/factory.h>

typedef void Var;           // Variable
typedef void CF;            // CanonicalForm
typedef void Fac;           // CFFactor = Factor<CF>
typedef void FacList;       // CFFList = List<CFFactor>
typedef void Iter;          // ListIterator<CFFactor>

// -----------------------------------------------------------------------------
// Variable

extern "C" 
void free_var (Var *ptr)
{
  Variable *varp = (Variable*) ptr;
  delete varp;
}

extern "C" 
Var *new_var(int level)
{
  Variable *varp = new Variable(level);
  return (void*)varp;
}

// -----------------------------------------------------------------------------
// CanonicalForm
extern "C" 
void free_cf (CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  delete cfp;
}

extern "C" 
CF *copy_cf(CF *ptr)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr;
  CanonicalForm *cfp2 = new CanonicalForm(*cfp1);
  return (void*)cfp2;
}

// -----------------------------------------------------------------------------
// Factor

extern "C" 
void free_fac (CFFactor *ptr)
{
  CFFactor *facp = (CFFactor*) ptr;
  delete facp;
}

extern "C" 
Fac *copy_fac(Fac *ptr)
{
  CFFactor *facp1 = (CFFactor*) ptr;
  CFFactor *facp2 = new CFFactor(*facp1);
  return (void*)facp2;
}

extern "C" 
int get_fac_expo(Fac *ptr)
{
  CFFactor *facp = (CFFactor*) ptr;
  return facp->exp();
}

extern "C" 
CF *get_factor(Fac *ptr)
{
  CFFactor *facp = (CFFactor*) ptr;
  CanonicalForm *cfp = new CanonicalForm( facp->factor() );
  return (void*)cfp;
}

// -----------------------------------------------------------------------------
// Iterator

extern "C" 
void free_iter(Iter *ptr)
{
  ListIterator<CFFactor> *iterp = (ListIterator<CFFactor> *) ptr;
  delete iterp;
}

extern "C"
Iter *next_iter(Iter *ptr)
{
  ListIterator<CFFactor> *iterp1 = (ListIterator<CFFactor> *) ptr;
  (*iterp1) ++; 
  return iterp1;
}

// -----------------------------------------------------------------------------
// List<Factor>

extern "C" 
void free_faclist (FacList *ptr)
{
  CFFList *listp = (CFFList*) ptr;
  delete listp;
}

extern "C" 
int get_list_length(FacList *ptr)
{
  CFFList *listp = (CFFList*) ptr;
  return listp->length();
}

extern "C" 
void flatten_faclist (FacList *ptr, Fac **tgt_arr)
{
  CFFList *listp = (CFFList*) ptr;
  ListIterator<CFFactor> iter = ListIterator<CFFactor>(*listp);
  int n = listp->length();
  for(int i=0 ; i<n ; i++)
  {
    CFFactor *cffp = new CFFactor( iter.getItem() );
    tgt_arr[i] = (void*)cffp;
    iter++;
  }
}

extern "C" 
FacList *my_factorize (CF *ptr)
{
  CanonicalForm *cfp  = (CanonicalForm*) ptr;
  CFFList *listp = new CFFList( factorize( *cfp ) );  
  return (void*)listp;
}

// -----------------------------------------------------------------------------
// CanonicalForm

extern "C" 
CF *empty_cf()
{
  CanonicalForm *cfp = new CanonicalForm();
  return (void*)cfp;
}

extern "C" 
CF *const_cf(int kst)
{
  CanonicalForm *cfp = new CanonicalForm(kst);
  return (void*)cfp;
}

extern "C" 
CF *var_cf(Var *var)
{
  Variable *varp = (Variable*)var;
  CanonicalForm *cfp = new CanonicalForm(*varp);
  return (void*)cfp;
}

extern "C" 
CF *var_pow_cf(Var *var, int expo)
{
  Variable *varp = (Variable*)var;
  CanonicalForm *cfp = new CanonicalForm(*varp,expo);
  return (void*)cfp;
}

// -----------------------------------------------------------------------------

extern "C" 
int is_zero(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->isZero());
}

extern "C" 
int is_one(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->isOne());
}

extern "C" 
int is_imm(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->isImm());
}

extern "C" 
int is_univariate(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->isUnivariate());
}

extern "C" 
int in_ZZ(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inZ());
}

extern "C" 
int in_QQ(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inQ());
}

extern "C" 
int in_GF(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inGF());
}

extern "C" 
int in_FF(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inFF());
}

// -----------------------------------------------------------------------------

extern "C" 
int in_BaseDomain(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inBaseDomain());
}

extern "C" 
int in_CoeffDomain(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inCoeffDomain());
}

extern "C" 
int in_PolyDomain(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inPolyDomain());
}

// -----------------------------------------------------------------------------

extern "C" 
int smallint_value(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->intval());
}

extern "C" 
int degree_of(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->degree());
}

extern "C" 
int level_of(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->level());
}

// -----------------------------------------------------------------------------

extern "C" 
CF *index_poly(CF *ptr, int idx)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr;
  CanonicalForm *cfp2 = new CanonicalForm( (*cfp1)[idx] );
  return cfp2;
}

// -----------------------------------------------------------------------------

extern "C" 
CF *numer(CF *ptr)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr;
  CanonicalForm *cfp2 = new CanonicalForm( cfp1->num() );
  return cfp2;
}

extern "C" 
CF *denom(CF *ptr)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr;
  CanonicalForm *cfp2 = new CanonicalForm( cfp1->den() );
  return cfp2;
}

// -----------------------------------------------------------------------------

extern "C" 
CF *plus(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1) + (*cfp2) );
  return cfp3;
}

extern "C" 
CF *minus(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1) - (*cfp2) );
  return cfp3;
}

extern "C" 
CF *times(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1) * (*cfp2) );
  return cfp3;
}

// -----------------------------------------------------------------------------

/*
void gmp_numerator ( const CanonicalForm & f, mpz_ptr result );

void gmp_denominator ( const CanonicalForm & f, mpz_ptr result );

int gf_value (const CanonicalForm & f );

CanonicalForm make_cf ( const mpz_ptr n );

CanonicalForm make_cf ( const mpz_ptr n, const mpz_ptr d, bool normalize );

CanonicalForm make_cf_from_gf ( const int z );
*/

