#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(ntf)(void *);
extern void F77_NAME(r_grid_simple)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(r_kernel_gaussian)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"ntf",               (DL_FUNC) &F77_NAME(ntf),                1},
    {"r_grid_simple",     (DL_FUNC) &F77_NAME(r_grid_simple),     11},
    {"r_kernel_gaussian", (DL_FUNC) &F77_NAME(r_kernel_gaussian), 16},
    {NULL, NULL, 0}
};

void R_init_rtorf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}