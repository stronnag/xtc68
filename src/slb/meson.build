slb_src = files('slbanal.c', 'slbdecl.c', 'slbdis.c', 'slbmain.c', 'slbmodul.c',
		'slborder.c', 'slbutil.c', 'slb.h', 'slbproto.h')

slb_copts = ['-O2', '-Wall', '-Werror', '-pedantic', '-DXTC68', '-I.']
slb_ldopts = []
if host_machine.system() == 'windows'
  slb_ldopts += '-lwsock32'
endif

install_data(['qdos-ranlib', 'qdos-ar'],
	     install_dir: join_paths(get_option('prefix'), 'bin'))
