
//#include <R.h>
//#include <R_ext/Utils.h> // for R_CheckUserInterrupt(void)

#include <signal.h>

#ifdef WIN32
extern int UserBreak;
#endif

void do_interrupt(void)
{
#ifdef WIN32
    UserBreak = 1;
    //raise(SIGBREAK);
#else
    raise(SIGINT);
#endif
}
