#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif
#include "Rts.h"

#ifdef _WIN32
StgWord64 time_ns(void)
{
  LARGE_INTEGER t;
  static double fq_recip;
  static LARGE_INTEGER t0;

  if (fq_recip == 0.0) {
    LARGE_INTEGER fq;
    QueryPerformanceFrequency(&fq);
    fq_recip = 1000000000.0 / (double)fq.QuadPart;
    QueryPerformanceCounter(&t0);
  }

  QueryPerformanceCounter(&t);
  return (StgWord64)((t.QuadPart - t0.QuadPart) * fq_recip);
}

StgDouble time_s(void)
{
  LARGE_INTEGER t;
  static double fq_recip;
  static LARGE_INTEGER t0;

  if (fq_recip == 0.0) {
    LARGE_INTEGER fq;
    QueryPerformanceFrequency(&fq);
    fq_recip = 1.0 / (double)fq.QuadPart;
    QueryPerformanceCounter(&t0);
  }

  QueryPerformanceCounter(&t);
  return ((t.QuadPart - t0.QuadPart) * fq_recip);
}
#else
StgWord64 time_ns(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return 1000000000ull * ts.tv_sec + ts.tv_nsec;
}

StgDouble time_s(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec*1e-9;
}
#endif

StgWord64 time_rdtsc(void)
{
  StgWord32 hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((StgWord64) lo) | (((StgWord64) hi)<<32);
}
