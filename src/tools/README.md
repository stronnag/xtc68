# uqlx copy tools

These files may also be found in the `utils` directory of UQLX; where they've bit-rotted into uselessness.

## qcp

`qcp` copies an `xtc68` executable into a UQLX directory, setting the data-space in the hidden UQLX 'executable files` hidden directory `.-UQLX-`.

* For `xtc68` compiled files, the dataspace is taken from the `XTcc` token at the end of the file.
* For other executables, the dataspace must be provided.
* If `outfile` is omitted, the dataspace is set for an extant file.
* Multiple input files may be provided; if there is more than one, the output must be a directory.

```
$ qcp -h
  qcp [-x dataspace] infile... [outfile]

```
## qls

`qls` lists the executable files in a UQLX/QDOS datastore. Timestamps are from the local (Unix) file system in RFC 3339 format.

```
$ qls ~/mdv1
qeyes                                    6834     100   1 2021-08-24 10:42:11
QTPI                                   116514     100   1 2021-08-25 13:37:49
status_x                                  546     160   1 2021-08-23 20:59:25
uemacs                                 120206    8192   1 2021-08-23 21:00:15
perl                                   283100    3784   1 2021-08-24 13:32:47
jobs_x                                    712     500   1 2021-08-23 19:10:54
show_x                                   2584     500   1 2021-08-23 21:06:03
```
