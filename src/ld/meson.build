ld_src = files('ldold.c', 'appdir.c')
ld_copts = ['-O2', '-Wall', '-Werror', '-pedantic',
	    '-DXTC68', '-fsigned-char' ]

ld_copts += '-DPREFIX="' + get_option('prefix') + '"'

ld_ldopts = []
