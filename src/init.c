/* generated by Brian Peterson on 2018-01-24 using tools::package_native_routine_registration_skeleton(".") */

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP CM2_1F(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CM2_CC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CM3_1F(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CM3_CC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CM3_Simaan(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CM4_1F(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CM4_CC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP M3_1F(SEXP, SEXP, SEXP, SEXP);
extern SEXP M3_CC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP M3_CCoefficients(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP M3_Simaan(SEXP, SEXP);
extern SEXP M3_T23(SEXP, SEXP);
extern SEXP M3HOOIiterator(SEXP, SEXP, SEXP, SEXP);
extern SEXP M3innprod(SEXP, SEXP, SEXP);
extern SEXP M3mat2vec(SEXP, SEXP);
extern SEXP M3port(SEXP, SEXP, SEXP);
extern SEXP M3port_grad(SEXP, SEXP, SEXP);
extern SEXP M3sample(SEXP, SEXP, SEXP, SEXP);
extern SEXP M3timesDiag(SEXP, SEXP, SEXP);
extern SEXP M3timesFull(SEXP, SEXP, SEXP, SEXP);
extern SEXP M3vec2mat(SEXP, SEXP);
extern SEXP M4_1f(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP M4_CC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP M4_CCoefficients(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP M4_MFresid(SEXP, SEXP, SEXP);
extern SEXP M4_T12(SEXP, SEXP, SEXP);
extern SEXP M4HOOIiterator(SEXP, SEXP, SEXP, SEXP);
extern SEXP M4innprod(SEXP, SEXP, SEXP);
extern SEXP M4mat2vec(SEXP, SEXP);
extern SEXP M4port(SEXP, SEXP, SEXP);
extern SEXP M4port_grad(SEXP, SEXP, SEXP);
extern SEXP M4sample(SEXP, SEXP, SEXP);
extern SEXP M4timesDiag(SEXP, SEXP, SEXP);
extern SEXP M4timesFull(SEXP, SEXP, SEXP, SEXP);
extern SEXP M4vec2mat(SEXP, SEXP);
extern SEXP VM2(SEXP, SEXP, SEXP, SEXP);
extern SEXP VM3(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP VM3kstat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP VM4(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"CM2_1F",           (DL_FUNC) &CM2_1F,            7},
    {"CM2_CC",           (DL_FUNC) &CM2_CC,            6},
    {"CM3_1F",           (DL_FUNC) &CM3_1F,           11},
    {"CM3_CC",           (DL_FUNC) &CM3_CC,           18},
    {"CM3_Simaan",       (DL_FUNC) &CM3_Simaan,       11},
    {"CM4_1F",           (DL_FUNC) &CM4_1F,           12},
    {"CM4_CC",           (DL_FUNC) &CM4_CC,           17},
    {"M3_1F",            (DL_FUNC) &M3_1F,             4},
    {"M3_CC",            (DL_FUNC) &M3_CC,             7},
    {"M3_CCoefficients", (DL_FUNC) &M3_CCoefficients,  7},
    {"M3_Simaan",        (DL_FUNC) &M3_Simaan,         2},
    {"M3_T23",           (DL_FUNC) &M3_T23,            2},
    {"M3HOOIiterator",   (DL_FUNC) &M3HOOIiterator,    4},
    {"M3innprod",        (DL_FUNC) &M3innprod,         3},
    {"M3mat2vec",        (DL_FUNC) &M3mat2vec,         2},
    {"M3port",           (DL_FUNC) &M3port,            3},
    {"M3port_grad",      (DL_FUNC) &M3port_grad,       3},
    {"M3sample",         (DL_FUNC) &M3sample,          4},
    {"M3timesDiag",      (DL_FUNC) &M3timesDiag,       3},
    {"M3timesFull",      (DL_FUNC) &M3timesFull,       4},
    {"M3vec2mat",        (DL_FUNC) &M3vec2mat,         2},
    {"M4_1f",            (DL_FUNC) &M4_1f,             6},
    {"M4_CC",            (DL_FUNC) &M4_CC,             8},
    {"M4_CCoefficients", (DL_FUNC) &M4_CCoefficients,  8},
    {"M4_MFresid",       (DL_FUNC) &M4_MFresid,        3},
    {"M4_T12",           (DL_FUNC) &M4_T12,            3},
    {"M4HOOIiterator",   (DL_FUNC) &M4HOOIiterator,    4},
    {"M4innprod",        (DL_FUNC) &M4innprod,         3},
    {"M4mat2vec",        (DL_FUNC) &M4mat2vec,         2},
    {"M4port",           (DL_FUNC) &M4port,            3},
    {"M4port_grad",      (DL_FUNC) &M4port_grad,       3},
    {"M4sample",         (DL_FUNC) &M4sample,          3},
    {"M4timesDiag",      (DL_FUNC) &M4timesDiag,       3},
    {"M4timesFull",      (DL_FUNC) &M4timesFull,       4},
    {"M4vec2mat",        (DL_FUNC) &M4vec2mat,         2},
    {"VM2",              (DL_FUNC) &VM2,               4},
    {"VM3",              (DL_FUNC) &VM3,              10},
    {"VM3kstat",         (DL_FUNC) &VM3kstat,         10},
    {"VM4",              (DL_FUNC) &VM4,              11},
    {NULL, NULL, 0}
};

void R_init_PerformanceAnalytics(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

