
#include <factory/factory.h>

// -----------------------------------------------------------------------------
// types

typedef void Var;           // Variable
typedef void CF;            // CanonicalForm
typedef void Fac;           // CFFactor = Factor<CF>
typedef void FacList;       // CFFList = List<CFFactor>
typedef void Iter;          // ListIterator<CFFactor>

// -----------------------------------------------------------------------------
// constants

extern "C" int level_base()  { return LEVELBASE;  }
extern "C" int level_trans() { return LEVELTRANS; }
extern "C" int level_quot()  { return LEVELQUOT;  }
extern "C" int level_expr()  { return LEVELEXPR;  }

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

extern "C" 
Var *root_of(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  Variable *varp = new Variable( rootOf( *cfp ) );
  return (void*)varp;
}

extern "C"
int has_mipo(Var *ptr)
{
  Variable *varp = (Variable*) ptr;
  return hasMipo( *varp );
}

extern "C"
CF *get_mipo(Var *ptr1 , Var *ptr2)
{
  Variable *varp1 = (Variable*) ptr1;
  Variable *varp2 = (Variable*) ptr2;
  CanonicalForm *cfp = new CanonicalForm( getMipo( *varp1 , *varp2 ) );
  return cfp;
}

extern "C"
void set_mipo(Var *ptr1 , CF *ptr2)
{
  Variable      *varp = (Variable     *) ptr1;
  CanonicalForm *cfp  = (CanonicalForm*) ptr2;
  setMipo( *varp, *cfp );
}

extern "C"
void set_reduce(Var *ptr, int flag)
{
  Variable *varp = (Variable*) ptr;
  setReduce( *varp , flag);
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
// some simple predicates

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

// -----------------------------------------------------------------------------
// base domain predicates

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
// domain predicates

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

extern "C" 
int in_Extension(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inExtension());
}

extern "C" 
int in_QuotDomain(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->inQuotDomain());
}

// -----------------------------------------------------------------------------
// imm_value, degree, level

extern "C" 
long int smallint_value(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return (cfp->intval());     // returns a long!!!
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

extern "C" 
CF *map_into(CF *ptr)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr;
  CanonicalForm *cfp2 = new CanonicalForm( cfp1->mapinto() );
  return cfp2;
}

extern "C" 
CF *substitute(CF *ptr1, Var *ptrv, CF *ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  Variable      *varp = (Variable*     ) ptrv;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1)( *cfp2 , *varp ) );
  return cfp3;
}

// -----------------------------------------------------------------------------
// numerator and denominator

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
// binary operations

extern "C" 
int is_equal(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;  
  return ( (*cfp1) == (*cfp2) );
}

extern "C" 
CF *plus_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1) + (*cfp2) );
  return cfp3;
}

extern "C" 
CF *minus_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1) - (*cfp2) );
  return cfp3;
}

extern "C" 
CF *times_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( (*cfp1) * (*cfp2) );
  return cfp3;
}

extern "C" 
CF *pow_cf(CF *ptr1, int n)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp3 = new CanonicalForm( power( *cfp1 , n ) );
  return cfp3;
}

extern "C" 
CF *div_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( div( *cfp1 , *cfp2 ) );
  return cfp3;
}

extern "C" 
CF *mod_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( mod( *cfp1 , *cfp2 ) );
  return cfp3;
}

extern "C" 
CF *gcd_poly_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( gcd_poly( *cfp1 , *cfp2 ) );
  return cfp3;
}

extern "C" 
CF *reduce_cf(CF *ptr1, CF* ptr2)
{
  CanonicalForm *cfp1 = (CanonicalForm*) ptr1;
  CanonicalForm *cfp2 = (CanonicalForm*) ptr2;
  CanonicalForm *cfp3 = new CanonicalForm( reduce( *cfp1 , *cfp2 ) );
  return cfp3;
}

// -----------------------------------------------------------------------------
// GMP conversion

extern "C" 
int get_gf_value(CF *ptr)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  return gf_value( *cfp );
}

extern "C" 
void get_gmp_numerator(CF *ptr, mpz_ptr tgt)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  gmp_numerator( *cfp , tgt );
}

extern "C" 
void get_gmp_denominator(CF *ptr, mpz_ptr tgt)
{
  CanonicalForm *cfp = (CanonicalForm*) ptr;
  gmp_denominator( *cfp , tgt );
}

extern "C" 
CF *make_ZZ_from_gmp(mpz_srcptr src)
{
  mpz_t copy;
  mpz_init_set(copy,src);
  // make_cf DOES NOT COPY, just copies the header (mpz_t structure)...
  // ...so we have to copy it here because our src is ephemeral.
  CanonicalForm *cfp = new CanonicalForm( make_cf(copy) );     
  return cfp;
}

extern "C" 
CF *make_QQ_from_gmp(mpz_srcptr src1, mpz_srcptr src2, int normalize_flag )
{
  mpz_t copy1;
  mpz_t copy2;
  mpz_init_set(copy1,src1);
  mpz_init_set(copy2,src2);
  CanonicalForm *cfp = new CanonicalForm( make_cf( copy1 , copy2 , normalize_flag ) );
  return cfp;
}

extern "C" 
CF *make_GF(int z)
{
  CanonicalForm *cfp = new CanonicalForm( make_cf_from_gf(z) );
  return cfp;
}


/*  
void gmp_numerator ( const CanonicalForm & f, mpz_ptr result );

void gmp_denominator ( const CanonicalForm & f, mpz_ptr result );

int gf_value (const CanonicalForm & f );

CanonicalForm make_cf ( const mpz_ptr n );

CanonicalForm make_cf ( const mpz_ptr n, const mpz_ptr d, bool normalize );

CanonicalForm make_cf_from_gf ( const int z );
*/


// -----------------------------------------------------------------------------

extern "C"
int get_characteristic()
{
  return getCharacteristic();
}

extern "C"
void set_characteristic1(int c)
{
  setCharacteristic(c);
}

/*
void setCharacteristic( int c ); // -> Fp && Q
void setCharacteristic( int c, int n ); // -> PrimePower
void setCharacteristic( int c, int n, char name ); // -> GF(q)

int getCharacteristic();
*/

// -----------------------------------------------------------------------------
