#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP c_smote(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mlr_MstepMultinomial(SEXP, SEXP);
extern SEXP _mlr_timesTwo(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_smote", (DL_FUNC) &c_smote, 4},
    {"_mlr_MstepMultinomial", (DL_FUNC) &_mlr_MstepMultinomial, 2},
    {"_mlr_timesTwo",         (DL_FUNC) &_mlr_timesTwo,         1},
    {NULL, NULL, 0}
};

void R_init_mlr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
