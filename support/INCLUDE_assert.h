
#ifdef __STDC__
#define _(params) params
#else
#define _(params) ()
#endif
#ifdef assert
#undef assert
#endif
#ifdef NDEBUG
#define assert(expr) ((void) 0)
#else
void _assert _((char *));
#define _STR(x) _VAL(x)
#define _VAL(x) #x
#define assert(expr) ((expr) ? (void)0 \
: _assert( __FILE__":" _STR(__LINE__)"\n" #expr ))
#endif
#undef _
