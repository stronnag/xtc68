project('xtc68', 'c', version : '0.0.2')

subdir('src/as68')
subdir('src/cc')
subdir('src/c68')
subdir('src/cpp')
subdir('src/ld')
subdir('src/slb')
subdir('src/tools')
subdir('support')

incdir = include_directories('src/include')

executable(
  'as68',
  as68_src,
  c_args: as68_copts,
  link_args: as68_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'qcc',
  qcc_src,
  c_args: qcc_copts,
  link_args: qcc_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'c68',
  c68_src,
  c_args: c68_copts,
  link_args: c68_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'qcpp',
  cpp_src,
  c_args: cpp_copts,
  link_args: cpp_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'qld',
  ld_src,
  c_args: ld_copts,
  link_args: ld_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'slb',
  slb_src,
  c_args: slb_copts,
  link_args: slb_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'qcp',
  tools_qcp_src,
  c_args: tools_copts,
  link_args: tools_ldopts,
  include_directories : incdir,
  install: true,
)

executable(
  'qls',
  tools_qls_src,
  c_args: tools_copts,
  link_args: tools_ldopts,
  include_directories : incdir,
  install: true,
)
