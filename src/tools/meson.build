tools_qls_src = files('qls.c')
tools_qcp_src = files('qcp.c')
tools_copts = ['-O2', '-Wall', '-Werror', '-pedantic']
tools_ldopts = []

if host_machine.system() == 'windows'
  tools_ldopts += '-lwsock32'
endif
