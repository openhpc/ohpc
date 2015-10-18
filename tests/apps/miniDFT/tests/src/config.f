#if defined(__PGI) || defined(__CRAY)
#define DECLARE_IARGC external
#else
#define DECLARE_IARGC intrinsic
#endif
