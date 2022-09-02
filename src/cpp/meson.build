cpp_src = files('version.c', 'cexp.c', 'cccp.c', 'appdir.c',
		'config.h', 'tm.h', 'tm-m68k.h')

cpp_copts= ['-O2', '-Wall', '-Werror', '-Wextra', '-pedantic',
	    '-DXTC68', '-I.', '-DCPLUSPLUS']

if get_option('relbuild')
  message ('cpp: not setting PREFIX')
else
  cpp_copts += '-DPREFIX=' + get_option('prefix')
endif

cpp_ldopts = []
if host_machine.system() == 'windows'
  cpp_ldopts += '-lwsock32'
endif
