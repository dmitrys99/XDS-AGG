#include "time.h"

enum { ttuUnknown, ttuHiRes, ttuClock } TimerToUse = ttuUnknown;
LARGE_INTEGER PerfFreq;      // ticks per second
int PerfFreqAdjust; // in case Freq is too big
int OverheadTicks;   // overhead  in calling timer

void DunselFunction() { return; }

void DetermineTimer()
{
   void (*pFunc)() = DunselFunction;

   // Assume the worst
   TimerToUse = ttuClock;
   if ( QueryPerformanceFrequency(&PerfFreq) )
      {
      // We can use hires timer, determine overhead
      TimerToUse = ttuHiRes;
      OverheadTicks = 200;
      for ( int i=0; i < 20; i++ )
         {
         LARGE_INTEGER b,e;
         int Ticks;
         QueryPerformanceCounter(&b);
         (*pFunc)();
         QueryPerformanceCounter(&e);
         Ticks = e.LowPart - b.LowPart;
         if ( Ticks >= 0 && Ticks < OverheadTicks )
            OverheadTicks = Ticks;
         }
      // See if Freq fits in 32 bits; if not lose some precision
      PerfFreqAdjust = 0;
      int High32 = PerfFreq.HighPart;
      while ( High32 )
         {
         High32 >>= 1;
         PerfFreqAdjust++;
         }
      }
   return;
}

double DoBench(void(*funcp)())
{
   double time;      /* Elapsed time */

   // Let any other stuff happen before we start
   MSG msg;
   PeekMessage(&msg,NULL,NULL,NULL,PM_NOREMOVE);
   Sleep(0);

   if ( TimerToUse == ttuUnknown )
      DetermineTimer();

   if ( TimerToUse == ttuHiRes )
      {
      LARGE_INTEGER tStart, tStop;
      LARGE_INTEGER Freq = PerfFreq;
      int Oht = OverheadTicks;
      int ReduceMag = 0;
      SetThreadPriority(GetCurrentThread(),
         THREAD_PRIORITY_TIME_CRITICAL);
      QueryPerformanceCounter(&tStart);
        (*funcp)();   //call the actual function being timed
      QueryPerformanceCounter(&tStop);
      SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_NORMAL);
      // Results are 64 bits but we only do 32
      unsigned int High32 = tStop.HighPart - tStart.HighPart;
      while ( High32 )
         {
         High32 >>= 1;
         ReduceMag++;
         }
      if ( PerfFreqAdjust || ReduceMag )
         {
         if ( PerfFreqAdjust > ReduceMag )
            ReduceMag = PerfFreqAdjust;
         tStart.QuadPart = Int64ShrlMod32(tStart.QuadPart, ReduceMag);
         tStop.QuadPart = Int64ShrlMod32(tStop.QuadPart, ReduceMag);
         Freq.QuadPart = Int64ShrlMod32(Freq.QuadPart, ReduceMag);
         Oht >>= ReduceMag;
         }

      // Reduced numbers to 32 bits, now can do the math
      if ( Freq.LowPart == 0 )
         time = 0.0;
      else
         time = ((double)(tStop.LowPart - tStart.LowPart
            - Oht))/Freq.LowPart;
      }
   else
      {
        long stime, etime;
      SetThreadPriority(GetCurrentThread(),
          THREAD_PRIORITY_TIME_CRITICAL);
      stime = clock();
      (*funcp)();
      etime = clock();
        SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_NORMAL);
      time = ((double)(etime - stime)) / CLOCKS_PER_SEC;
      }

  return (time);
}
