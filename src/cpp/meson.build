cpp_src = files('version.c', 'cexp.c', 'cccp.c', 'appdir.c',
		'config.h', 'tm.h', 'tm-m68k.h')

cpp_copts= ['-O2', '-Wall', '-Werror', '-Wextra', '-pedantic',
	    '-DXTC68', '-I.', '-DCPLUSPLUS']
cpp_copts += '-DPREFIX="' + get_option('prefix') + '"'

cpp_ldopts = []
if host_machine.system() == 'windows'
  cpp_ldopts += '-lwsock32'
endif
