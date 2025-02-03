#include "stdafx.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "optimization.h"

using namespace alglib;

extern "C"
{
  __declspec(thread) void  (*ns_cb)(int n, double * x, double * fi, double ** jac, void* ptr);
  __declspec(thread) double ns_f;

  void ns_jac(const real_1d_array& x, real_1d_array& fi, real_2d_array& jac, void* ptr)
  {
    ns_cb(x.length(), x.c_ptr()->ptr.p_double, fi.c_ptr()->ptr.p_double, jac.c_ptr()->ptr.pp_double, ptr);
  };

  void ns_frep(const real_1d_array& x, double func, void* ptr)
  {
    ns_f = func;
  };

  __declspec(dllexport) double alglib_NonSmoothBoundedMinimize(void* Func, int n, double* X, double* LowBound, double* UpBound, double Epsilon, double Radius, double Penalty, void* Data)
  {
    minnsstate state;
    minnsreport rep;
    real_1d_array x;
    real_1d_array lb;
    real_1d_array ub;

    ns_cb = (void (*)(int n, double* x, double* fi, double** jac, void* ptr)) Func;
    ns_f = fp_nan;
    x.attach_to_ptr(n, X);

    if (LowBound && UpBound)
    {
      lb.attach_to_ptr(n, LowBound);
      ub.attach_to_ptr(n, UpBound);
    }

    minnscreate(x, state);
    minnssetalgoags(state, Radius, Penalty);
    if (LowBound && UpBound)
      minnssetbc(state, lb, ub);
    minnssetcond(state, Epsilon, 0);
    minnssetxrep(state, true);
    minnsoptimize(state, ns_jac, ns_frep, Data);
    minnsresults(state, x, rep);

    ns_cb = NULL;
    for (int i = 0; i < n; ++i)
      X[i] = x[i];

    return ns_f;
  }

}