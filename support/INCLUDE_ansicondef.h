#ifndef _ANSICON_H_
#define _ANSICON_H_
#define VT52 0x01
#define VT100 0x02
#define ANSI1 0x04
#define ANSI2 0x08
#define ANSI3 0x10
#define ANSI4 0x20
typedef struct {
char emulation;
char csi;
} ANSICONDEF,*PANSICONDEF;
extern ANSICONDEF __ANSICONF__;
extern char _ANSIChr_[],_ANSICtl_[],_DECGraf_[];
extern int ANSI_conwrite();
#endif
