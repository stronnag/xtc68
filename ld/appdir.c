#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <libgen.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>

#if defined(WIN32)
#include <windows.h>
#elif defined(__APPLE__)
#include <mach-o/dyld.h>
#endif

char *get_binary_path(void) {
  char *dest = calloc(1,PATH_MAX);
  uint32_t size;
#if defined(__linux__)
  realpath("/proc/self/exe", dest);
#elif defined(WIN32)
  GetModuleFileName(NULL, dest, PATH_MAX);
#elif defined(__APPLE__)
  size = PATH_MAX;
  _NSGetExecutablePath(dest, &size);
#elif defined(__FreeBSD__)
  realpath("/proc/curproc/file", dest);
#endif
  size = strlen(dest);
  if(size > 0) {
    return dest;
  } else {
    free(dest);
    return NULL;
  }
}

#ifdef APPDIR_TEST
int main(int argc, char **argv) {
  char* p = get_binary_path();
  if(p != NULL) {
    printf("Path = %s\n", dirname(p));
    free(p);
  }
  return 0;
}
#endif
