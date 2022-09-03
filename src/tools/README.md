# uqlx copy tools

These files were previously found in the `utils` directory of UQLX; where they've bit-rotted into uselessness.

The files here support sqlux, in particular, the discovery and maintenance of 'XTcc' data tokens.

## qcp

`qcp` copies `xtc68` executable(s) into a UQLX directory.

* For `xtc68` compiled files, the dataspace is taken from the `XTcc` token at the end of the file.
* For other executables, the dataspace must be provided.
* If `outfile` is omitted, the dataspace is set for an extant file.
* Multiple input files may be provided; if there is more than one, the output must be a directory.

```
$ qcp -h
  qcp [-x dataspace] infile(s)... [outfile]

```
## qls

`qls` lists the files in a sqlux/QDOS datastore. Timestamps are from the local (Unix) file system in RFC 3339 format. Dataspace is from (xtc68) 'XTcc' tokens only.

Output is `name`, `size`, `dataspace`, `type` and `timestamp`. `type` is '1' for executable files, otherwise `0`. Directory names are also listed with any metadata.

Only regular files are shown, with "as-is" separators. There is directory recursion if `-R` is provided. Wildcard matching is also provided;

```
qcp [-x|-X] [-R] inspec...
where:
 -x : only show executable files (with dataspace)
 -X : only show non-executable files (with zero dataspace)
 -R : Recurse into sub-directories

```
Providing -x and -X is _almost_ the same as providing neither (providing both does not list non-recursed directories).

Note that if you are using a shell that automagically expands wildcards, you must escape any wildcards you don't want expanded by the shell.

In this example, if the wildcard was not escaped, the shell would have expanded the directory names and `qls` would have listed them (and would not have seen the wildcard as the shell would have already consumed it).

```
# Linux, bash
$ qls ~/Projects/ql/q\*

/home/jrh/Projects/ql:
qldata/
qlsoft/
qvm/

$ qls ~/Projects/ql/exe/q\*

/home/jrh/Projects/ql/exe:
Qspread                                 49818   20480   1 1998-05-27 20:28:24
qascade                                 16644     100   1 2022-08-30 14:29:36
qbzip2                                  57620   75256   1 1998-05-27 20:28:24
qclient                                  4238      50   1 1998-05-27 20:28:23
qcmd                                    14026     786   1 1998-05-27 20:28:24
qdhry                                   17920   11466   1 1999-08-28 20:05:54
qed                                      7916    1536   1 1998-05-27 20:28:09
qeyes                                    6834      50   1 1998-05-27 20:28:12
qfax                                    73370     928   1 1999-06-25 20:28:09
qfaxclient                               6920      50   1 1998-05-27 20:28:10
qfaxthing                                4782      50   1 1998-05-27 20:28:10
qfaxview                                 9336      50   1 1998-05-27 20:28:10
qfm                                     21786     100   1 2000-01-03 11:08:03
qfork                                    2230      50   1 1998-05-27 20:28:12
qfv                                     93186     736   1 2021-08-29 21:16:50
qgif2ps                                 33734    1176   1 1998-05-27 20:28:11
qless                                    7356     100   1 2000-01-03 11:08:03
qlf                                     20550    1672   1 1998-05-27 20:28:13
qlist                                   23154     592   1 1998-05-27 20:28:25
qlmailer                                34426     254   1 2000-02-25 19:32:36
qlrt                                    49470    2122   1 1998-07-19 00:20:34
qlst                                    23068     592   1 1998-05-27 20:28:25
qlterm                                   7688    1024   1 1998-05-27 20:28:12
qlunzip                                 92006   40336   1 1998-05-27 20:28:13
qmac                                    34024    4096   1 1998-08-06 19:31:18
qnet                                   193608   24812   1 1999-12-24 20:59:32
qnntpt                                  24512     540   1 2000-02-25 19:32:33
qonfrig                                 22828     600   1 1998-05-27 20:28:12
qparse                                  32402     226   1 1998-05-27 20:28:22
qpop3                                   39528    1482   1 1999-10-01 22:12:15
qtpi                                   116412     100   1 2022-08-28 17:53:14
qudp                                    18436     686   1 1998-05-27 20:28:20
quill                                   52418    1280   1 1998-05-27 20:28:23
qunpic                                 112644   33964   1 1998-05-27 20:28:13
qunpic_nolzw                            77100    4428   1 1998-05-27 20:28:20
qvcnvt                                  24258     722   1 2000-01-02 21:23:42
qvm                                     89932     918   1 2022-08-30 19:49:33
qwaff                                   10592      50   1 1999-02-20 11:44:46
qwklist                                 23068     592   1 1998-05-27 20:28:25
qy2k                                    21144     760   1 2000-01-07 20:21:59

$ ls ~/Projects/ql/exe | wc -l
282
## !! really !!

```
