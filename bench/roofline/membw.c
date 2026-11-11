// sk-v27 LW2.CON-2 (S11 BENCH) roofline: single-thread memory bandwidth on this host.
// One fresh process; a 1 GiB buffer (far beyond the SLC) is touched once, then read
// (64-bit sum, NEON-vectorised by clang -O3) and copied (memcpy), R times each; prints the
// best and median GB/s (10^9 B/s) of each. The parsers under test are single-threaded and
// read their input once, so this read figure is the ceiling for any per-byte parse rate.
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
static double now(void){struct timespec t;clock_gettime(CLOCK_MONOTONIC_RAW,&t);return t.tv_sec+t.tv_nsec*1e-9;}
static int cmp(const void*a,const void*b){double x=*(double*)a,y=*(double*)b;return (x>y)-(x<y);}
int main(int argc,char**argv){
  size_t n=(size_t)1<<30; int R=argc>1?atoi(argv[1]):9;
  uint64_t*a=malloc(n),*b=malloc(n); memset(a,1,n); memset(b,2,n);
  double rd[64],cp[64]; volatile uint64_t sink=0;
  for(int r=0;r<R;r++){
    double t=now(); uint64_t s=0; size_t m=n/8; for(size_t i=0;i<m;i++) s+=a[i]; sink+=s; rd[r]=n/(now()-t)/1e9;
    t=now(); memcpy(b,a,n); cp[r]=2.0*n/(now()-t)/1e9; sink+=b[r];
  }
  qsort(rd,R,sizeof*rd,cmp); qsort(cp,R,sizeof*cp,cmp);
  printf("read_best_GBps=%.2f read_median_GBps=%.2f copy_best_GBps=%.2f copy_median_GBps=%.2f reps=%d sink=%llu\n",
         rd[R-1],rd[R/2],cp[R-1],cp[R/2],R,(unsigned long long)(sink&1));
  return 0;
}
