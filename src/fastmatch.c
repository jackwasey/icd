/*
 *  fastmatch: fast implementation of match() in R using semi-permanent hash tables
 *
 *  Copyright (C) 2010, 2011  Simon Urbanek
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 */

// JW including this file as there is an incompatible different version in github (this is from CRAN version 1.0.4)

/* for speed (should not really matter in this case as most time is spent in the hashing) */
#define USE_RINTERNALS 1
#include <Rinternals.h>

/* for malloc/free since we handle our hash table memory separately from R */
#include <stdlib.h>
/* for hashing for pointers we need intptr_t */
#include <stdint.h>

/* match5 to fall-back to R's internal match for types we don't support */
SEXP match5(SEXP itable, SEXP ix, int nmatch, SEXP incomp, SEXP env);

/* ".match.hash" symbol - cached on first use */
SEXP hs;

typedef int hash_index_t;

typedef struct hash {
  int m, k, els, type;
  void *src;
  SEXP prot, parent;
  struct hash *next;
  hash_index_t ix[1];
} hash_t;

/* create a new hash table with the given source and length.
 we store only the index - values are picked from the source
 so you must make sure the source is still alive when used */
static hash_t *new_hash(void *src, hash_index_t len) {
  hash_t *h;
  hash_index_t m = 2, k = 1, desired = len * 2; /* we want a maximal load of 50% */
while (m < desired) { m *= 2; k++; }
h = (hash_t*) calloc(1, sizeof(hash_t) + (sizeof(hash_index_t) * m));
if (!h) Rf_error("unable to allocate %.2Mb for a hash table", (double) sizeof(hash_index_t) * (double) m / (1024.0 * 1024.0));
h->m = m;
h->k = k;
h->src = src;
return h;
}

/* free the hash table (and all chained hash tables as well) */
static void free_hash(hash_t *h) {
  if (h->next) free_hash(h->next);
  if (h->prot) R_ReleaseObject(h->prot);
  free(h);
}

/* R finalized for the hash table object */
static void hash_fin(SEXP ho) {
  hash_t *h = (hash_t*) EXTPTR_PTR(ho);
  if (h) free_hash(h);
}

/* pi-hash fn */
#define HASH(X) (3141592653U * ((unsigned int)(X)) >> (32 - h->k))

/* add the integer value at index i (0-based!) to the hash */
static void add_hash_int(hash_t *h, hash_index_t i) {
  int *src = (int*) h->src;
  int val = src[i++], addr;
  addr = HASH(val);
#ifdef PROFILE_HASH
  int oa = addr;
#endif
  while (h->ix[addr] && src[h->ix[addr] - 1] != val) {
    addr++;
    if (addr == h->m) addr = 0;
  }
#ifdef PROFILE_HASH
  if (addr != oa) printf("%d: dist=%d (addr=%d, oa=%d)\n", val, addr - oa, addr, oa);
#endif
  if (!h->ix[addr])
    h->ix[addr] = i;
}

/* to avoid aliasing rules issues use a union */
union dint_u {
  double d;
  unsigned int u[2];
};

/* add the double value at index i (0-based!) to the hash */
static void add_hash_real(hash_t *h, hash_index_t i) {
  double *src = (double*) h->src;
  union dint_u val;
  int addr;
  /* double is a bit tricky - we nave to nomalize 0.0, NA and NaN */
  val.d = (src[i] == 0.0) ? 0.0 : src[i];
  if (R_IsNA(val.d)) val.d = NA_REAL;
  else if (R_IsNaN(val.d)) val.d = R_NaN;
  addr = HASH(val.u[0]+ val.u[1]);
#ifdef PROFILE_HASH
  int oa = addr;
#endif
  while (h->ix[addr] && src[h->ix[addr] - 1] != val.d) {
    addr++;
    if (addr == h->m) addr = 0;
  }
#ifdef PROFILE_HASH
  if (addr != oa) printf("%g: dist=%d (addr=%d, oa=%d)\n", val.d, addr - oa, addr, oa);
#endif
  if (!h->ix[addr])
    h->ix[addr] = i + 1;
}

/* add the pointer value at index i (0-based!) to the hash */
static void add_hash_ptr(hash_t *h, hash_index_t i) {
  int addr;
  void **src = (void**) h->src;
  intptr_t val = (intptr_t) src[i++];
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
  addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
  addr = HASH(val);
#endif
#ifdef PROFILE_HASH
  int oa = addr;
#endif
  while (h->ix[addr] && (intptr_t) src[h->ix[addr] - 1] != val) {
    addr++;
    if (addr == h->m) addr = 0;
  }
#ifdef PROFILE_HASH
  if (addr != oa) printf("%p: dist=%d (addr=%d, oa=%d)\n", val, addr - oa, addr, oa);
#endif
  if (!h->ix[addr])
    h->ix[addr] = i;
}

/* NOTE: we are returning a 1-based index ! */
static int get_hash_int(hash_t *h, int val, int nmv) {
  int *src = (int*) h->src;
  int addr;
  addr = HASH(val);
  while (h->ix[addr]) {
    if (src[h->ix[addr] - 1] == val)
      return h->ix[addr];
    addr ++;
    if (addr == h->m) addr = 0;
  }
  return nmv;
}

/* NOTE: we are returning a 1-based index ! */
static int get_hash_real(hash_t *h, double val, int nmv) {
  double *src = (double*) h->src;
  int addr;
  union dint_u val_u;
  /* double is a bit tricky - we nave to normalize 0.0, NA and NaN */
  if (val == 0.0) val = 0.0;
  if (R_IsNA(val)) val = NA_REAL;
  else if (R_IsNaN(val)) val = R_NaN;
  val_u.d = val;
  addr = HASH(val_u.u[0] + val_u.u[1]);
  while (h->ix[addr]) {
    if (src[h->ix[addr] - 1] == val)
      return h->ix[addr];
    addr++;
    if (addr == h->m) addr = 0;
  }
  return nmv;
}

/* NOTE: we are returning a 1-based index ! */
static int get_hash_ptr(hash_t *h, void *val_ptr, int nmv) {
  void **src = (void **) h->src;
  intptr_t val = (intptr_t) val_ptr;
  int addr;
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
  addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
  addr = HASH(val);
#endif
  while (h->ix[addr]) {
    if ((intptr_t) src[h->ix[addr] - 1] == val)
      return h->ix[addr];
    addr ++;
    if (addr == h->m) addr = 0;
  }
  return nmv;
}

static SEXP asCharacter(SEXP s, SEXP env)
{
  SEXP call, r;
  PROTECT(call = lang2(install("as.character"), s));
  PROTECT(r = eval(call, env));
  UNPROTECT(2);
  return r;
}

SEXP fmatch_core(SEXP x, SEXP y, int nmv) {
  SEXP a;
  SEXPTYPE type;
  hash_t *h = 0;
  int n = LENGTH(x), np = 0, y_to_char = 0, y_factor = 0;

  /* edge-cases of 0 length */
  if (n == 0) return allocVector(INTSXP, 0);
  if (LENGTH(y) == 0) { /* empty table -> vector full of nmv */
  int *ai;
    a = allocVector(INTSXP, n);
    ai = INTEGER(a);
    for (np = 0; np < n; np++) ai[np] = nmv;
    return a;
  }

  /* implicitly convert factors/POSIXlt to character */
  if (OBJECT(x)) {
    if (inherits(x, "factor")) {
      x = PROTECT(asCharacterFactor(x));
      np++;
    } else if (inherits(x, "POSIXlt")) {
      x = PROTECT(asCharacter(x, R_GlobalEnv)); /* FIXME: match() uses env properly - should we switch to .External ? */
  np++;
    }
  }

  /* for y we may need to do that later */
  y_factor = OBJECT(y) && inherits(y, "factor");
  y_to_char = y_factor || (OBJECT(y) && inherits(y, "POSIXlt"));

  /* coerce to common type - in the order of SEXP types */
  if(TYPEOF(x) >= STRSXP || TYPEOF(y) >= STRSXP)
    type = STRSXP;
  else
    type = (TYPEOF(x) < TYPEOF(y)) ? TYPEOF(y) : TYPEOF(x);

  /* we only support INT/REAL/STR */
  if (type != INTSXP && type != REALSXP && type != STRSXP) {
    Rf_warning("incompatible type, fastmatch() is falling back to match()");
    return match5(y, x, nmv, NULL, R_BaseEnv);
  }

  if (y_to_char && type != STRSXP) /* y = factor -> character -> type must be STRSXP */
  type = STRSXP;

  /* coerce x - not y yet because we may get away with the existing cache */
  if (TYPEOF(x) != type) {
    x = PROTECT(coerceVector(x, type));
    np++;
  }

  /* find existing cache(s) */
  if (!hs) hs = Rf_install(".match.hash");
  a = Rf_getAttrib(y, hs);
  if (a != R_NilValue) { /* if there is a cache, try to find the matching type */
  h = (hash_t*) EXTPTR_PTR(a);
    /* could the object be out of sync ? If so, better remove the hash and ignore it */
    if (h->parent != y) {
#if HASH_VERBOSE
      Rprintf(" - DISCARDING hash, its parent and the bearer don't match, taking no chances.\n");
#endif
      h = 0;
      Rf_setAttrib(y, hs, R_NilValue);
    }
    while (h && h->type != type) h = h->next;
  }
  /* if there is no cache or not of the needed coerced type, create one */
  if (a == R_NilValue || !h) {
    h = new_hash(DATAPTR(y), LENGTH(y));
    h->type = type;
    h->parent = y;
#if HASH_VERBOSE
    Rprintf(" - creating new hash for type %d\n", type);
#endif
    if (a == R_NilValue) { /* if there is no cache attribute, create one */
  a = R_MakeExternalPtr(h, R_NilValue, R_NilValue);
      Rf_setAttrib(y, hs, a);
      Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("match.hash"));
      R_RegisterCFinalizer(a, hash_fin);
    } else { /* otherwise append the new cache */
  hash_t *lh = (hash_t*) EXTPTR_PTR(a);
      while (lh->next) lh = lh->next;
      lh->next = h;
#if HASH_VERBOSE
      Rprintf("   (appended to the cache list)\n");
#endif
    }

    if (TYPEOF(y) != type) {
#if HASH_VERBOSE
      if (y_to_char)
        Rprintf("   (need to convert table factor/POSIXlt to strings\n");
      else
        Rprintf("   (need to coerce table to %d)\n", type);
#endif
      y = y_to_char ? (y_factor ? asCharacterFactor(y) : asCharacter(y, R_GlobalEnv)) : coerceVector(y, type);
      h->src = DATAPTR(y); /* this is ugly, but we need to adjust the source since we changed it */
  h->prot = y; /* since the coerced object is temporary, we let the hash table handle its life span */
  R_PreserveObject(y);
    }
    /* make sure y doesn't go away while we create the hash */
    /* R_PreserveObject(y);     */
    /* spawn a thread to create the hash */
    /* nope - so far we do it serially */

    { /* create the hash table */
    int i, n = LENGTH(y);
      if (type == INTSXP)
        for(i = 0; i < n; i++)
          add_hash_int(h, i);
      else if (type == REALSXP)
        for(i = 0; i < n; i++)
          add_hash_real(h, i);
      else
        for(i = 0; i < n; i++)
          add_hash_ptr(h, i);
    }
  }

  { /* query the hash table */
    int i, n = LENGTH(x);
    SEXP r = allocVector(INTSXP, n);
    int *v = INTEGER(r);
    if (type == INTSXP) {
      int *k = INTEGER(x);
      for (i = 0; i < n; i++)
        v[i] = get_hash_int(h, k[i], nmv);
    } else if (type == REALSXP) {
      double *k = REAL(x);
      for (i = 0; i < n; i++)
        v[i] = get_hash_real(h, k[i], nmv);
    } else {
      SEXP *k = (SEXP*) DATAPTR(x);
      for (i = 0; i < n; i++)
        v[i] = get_hash_ptr(h, k[i], nmv);
    }
    if (np) UNPROTECT(np);
    return r;
  }
}

SEXP fmatch(SEXP x, SEXP y, SEXP nonmatch) {
  int nmv = asInteger(nonmatch);
  return fmatch_core(x, y, nmv);
}

SEXP fmatch_zero(SEXP x, SEXP y) {
  return fmatch_core(x, y, 0L);
}

SEXP fmatch_str(SEXP x, SEXP y) {
  SEXP a;
  hash_t *h = 0;
  int n = LENGTH(x), np = 0, y_to_char = 0, y_factor = 0;
  const int nmv = 0L;

  /* edge-cases of 0 length */
  if (n == 0) return allocVector(INTSXP, 0);
  if (LENGTH(y) == 0) { // empty table -> vector full of nmv
    int *ai;
    a = allocVector(INTSXP, n);
    ai = INTEGER(a);
    for (np = 0; np < n; np++) ai[np] = nmv;
    return a;
  }

  /* find existing cache(s) */
  if (!hs) hs = Rf_install(".match.hash");
  a = Rf_getAttrib(y, hs);
  if (a != R_NilValue) { /* if there is a cache, try to find the matching type */
  h = (hash_t*) EXTPTR_PTR(a);
    /* could the object be out of sync ? If so, better remove the hash and ignore it */
    if (h->parent != y) {
      h = 0;
      Rf_setAttrib(y, hs, R_NilValue);
    }
    while (h && h->type != STRSXP) h = h->next;
  }
  /* if there is no cache or not of the needed coerced type, create one */
  if (a == R_NilValue || !h) {
    h = new_hash(DATAPTR(y), LENGTH(y));
    h->type = STRSXP;
    h->parent = y;
    if (a == R_NilValue) { /* if there is no cache attribute, create one */
  a = R_MakeExternalPtr(h, R_NilValue, R_NilValue);
      Rf_setAttrib(y, hs, a);
      Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("match.hash"));
      R_RegisterCFinalizer(a, hash_fin);
    } else { /* otherwise append the new cache */
  hash_t *lh = (hash_t*) EXTPTR_PTR(a);
      while (lh->next) lh = lh->next;
      lh->next = h;
    }

    /* create the hash table */
    {
      int i, n = LENGTH(y);
      for(i = 0; i != n; i++)
        add_hash_ptr(h, i);
    }
  }

  /* query the hash table */
  {
    int i, n = LENGTH(x);
    SEXP r = allocVector(INTSXP, n);
    int *v = INTEGER(r);
    SEXP *k = (SEXP*) DATAPTR(x);
    for (i = 0; i != n; ++i)
      v[i] = get_hash_ptr(h, k[i], nmv);

    if (np) UNPROTECT(np);
    return r;
  }
}

// cut down to just find string and return boolean as soon as it is found,
// basically doing a hashed %in% for strings.
//
//  No bool in C! 0 = false 1 = true
int ffind_str(SEXP x, SEXP y) {
  SEXP a;
  hash_t *h = 0;
  int n = LENGTH(x), np = 0, y_to_char = 0, y_factor = 0;

  /* edge-cases of 0 length */
  if (n == 0) return 0;
  if (LENGTH(y) == 0) return 0;

  /* find existing cache(s) */
  if (!hs) hs = Rf_install(".match.hash");
  a = Rf_getAttrib(y, hs);
  if (a != R_NilValue) { /* if there is a cache, try to find the matching type */
  h = (hash_t*) EXTPTR_PTR(a);
    /* could the object be out of sync ? If so, better remove the hash and ignore it */
    if (h->parent != y) {
      h = 0;
      Rf_setAttrib(y, hs, R_NilValue);
    }
    while (h && h->type != STRSXP) h = h->next;
  }
  /* if there is no cache or not of the needed coerced type, create one */
  if (a == R_NilValue || !h) {
    h = new_hash(DATAPTR(y), LENGTH(y));
    h->type = STRSXP;
    h->parent = y;
    if (a == R_NilValue) { /* if there is no cache attribute, create one */
  a = R_MakeExternalPtr(h, R_NilValue, R_NilValue);
      Rf_setAttrib(y, hs, a);
      Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("match.hash"));
      R_RegisterCFinalizer(a, hash_fin);
    } else { /* otherwise append the new cache */
  hash_t *lh = (hash_t*) EXTPTR_PTR(a);
      while (lh->next) lh = lh->next;
      lh->next = h;
    }

    /* create the hash table */
    {
      int i, n = LENGTH(y);
      for(i = 0; i != n; i++)
        add_hash_ptr(h, i);
    }
  } // end created hash table

  /* query the hash table */
  {
    int i = 0, n = LENGTH(x);
    SEXP *k = (SEXP*) DATAPTR(x);
    //for (i = 0; i != n; ++i) {
    while (i != n && get_hash_ptr(h, k[i], 0L) == 0L) ++i;

    if (np) UNPROTECT(np);

    //SEXP r = allocVector(LGLSXP, 1);
    //LOGICAL(r)[0] = i != n;
    //return r;
    //Rprintf("i = %u, n = %u\n", i, n);
    return i != n;
  }
}

// now assume x is length 1
int ffind_one_str(SEXP x, SEXP y) {
  SEXP a;
  hash_t *h = 0;
  int y_to_char = 0, y_factor = 0;

  /* edge-cases of 0 length */
  if (LENGTH(y) == 0) return 0;

  /* find existing cache(s) */
  if (!hs) hs = Rf_install(".match.hash");
  a = Rf_getAttrib(y, hs);
  if (a != R_NilValue) { /* if there is a cache, try to find the matching type */
  h = (hash_t*) EXTPTR_PTR(a);
    /* could the object be out of sync ? If so, better remove the hash and ignore it */
    if (h->parent != y) {
      h = 0;
      Rf_setAttrib(y, hs, R_NilValue);
    }
    while (h && h->type != STRSXP) h = h->next;
  }
  /* if there is no cache or not of the needed coerced type, create one */
  if (a == R_NilValue || !h) {
    h = new_hash(DATAPTR(y), LENGTH(y));
    h->type = STRSXP;
    h->parent = y;
    if (a == R_NilValue) { /* if there is no cache attribute, create one */
  a = R_MakeExternalPtr(h, R_NilValue, R_NilValue);
      Rf_setAttrib(y, hs, a);
      Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("match.hash"));
      R_RegisterCFinalizer(a, hash_fin);
    } else { /* otherwise append the new cache */
  hash_t *lh = (hash_t*) EXTPTR_PTR(a);
      while (lh->next) lh = lh->next;
      lh->next = h;
    }

    /* create the hash table */
    {
      int i, n = LENGTH(y);
      for(i = 0; i != n; i++)
        add_hash_ptr(h, i);
    }
  } // end created hash table

  /* query the hash table */
  {
    SEXP *k = (SEXP*) DATAPTR(x);
    return get_hash_ptr(h, k[0], 0L);
  }
}
