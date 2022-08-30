ld_src = files('ldold.c', 'appdir.c')
ld_copts = ['-O2', '-Wall', '-Werror', '-pedantic',
	    '-DXTC68', '-fsigned-char' ]

if get_option('relbuild')
  message ('ld: not setting PREFIX')
else
  ld_copts += '-DPREFIX="' + get_option('prefix') + '"'
endif

ld_ldopts = []
