#if defined __cplusplus && defined __has_cpp_attribute
    #if __has_cpp_attribute(fallthrough) && __cplusplus >= __has_cpp_attribute(fallthrough)
        #define XTC68_FALLTHROUGH [[fallthrough]]
    #endif
#elif defined __STDC_VERSION__ && defined __has_c_attribute
    #if __has_c_attribute(fallthrough) && __STDC_VERSION__ >= __has_c_attribute(fallthrough)
        #define XTC68_FALLTHROUGH [[fallthrough]]
    #endif
#endif
#if !defined XTC68_FALLTHROUGH && defined __has_attribute
    #if __has_attribute(__fallthrough__)
        #define XTC68_FALLTHROUGH __attribute__((__fallthrough__))
    #endif
#endif
#if !defined XTC68_FALLTHROUGH
#define XTC68_FALLTHROUGH do {} while(0)
#endif
