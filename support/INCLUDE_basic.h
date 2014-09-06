
#ifndef _BASIC_H
#define _BASIC_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define BV_START 0
#define BV_BFBAS 0x00
#define BV_BUFP 0x04
#define BV_TKBAS 0x08
#define BV_TKP 0x0c
#define BV_PFBAS 0x10
#define BV_PFP 0x14
#define BV_NTBAS 0x18
#define BV_NTP 0x1c
#define BV_NLBAS 0x20
#define BV_NLP 0x24
#define BV_VVBAS 0x28
#define BV_VVP 0x2c
#define BV_CHBAS 0x30
#define BV_CHP 0x34
#define BV_RTBAS 0x38
#define BV_RTP 0x3c
#define BV_LNBAS 0x40
#define BV_LNP 0x44
#define BV_CHANGE 0x48
#define BV_BTP 0x48
#define BV_BTBAS 0x4c
#define BV_TGP 0x50
#define BV_TGBAS 0x54
#define BV_RIP 0x58
#define BV_RIBAS 0x5c
#define BV_SSP 0x60
#define BV_SSBAS 0x64
#define BV_EBDPT 0x64
#define BV_LINUM 0x68
#define BV_LENGTH 0x6a
#define BV_STMNT 0x6c
#define BV_CONT 0x6d
#define BV_INLIN 0x6e
#define BV_SING 0x6f
#define BV_INDEX 0x70
#define BV_VVFREE 0x72
#define BV_SSSAV 0x76
#define BV_RAND 0x80
#define BV_COMCH 0x84
#define BV_NXLIN 0x88
#define BV_NXSTM 0x8a
#define BV_COMLN 0x8b
#define BV_STOPN 0x8c
#define BV_EDIT 0x8e
#define BV_BRK 0x8f
#define BV_UNRVL 0x90
#define BV_CNSTM 0x91
#define BV_CNLNO 0x92
#define BV_DALNO 0x94
#define BV_DASTM 0x96
#define BV_DAITM 0x97
#define BV_CNIND 0x98
#define BV_CBINL 0x9a
#define BV_LSANY 0x9b
#define BV_LSBEF 0x9c
#define BV_LSBAS 0x9e
#define BV_LSAFT 0xa0
#define BV_LENLN 0xa2
#define BV_MAXLN 0xa4
#define BV_TOTLN 0xa6
#define BV_AUTO 0xaa
#define BV_PRINT 0xab
#define BV_EDLIN 0xac
#define BV_EDINC 0xae
#define BV_TKPOS 0xb0
#define BV_PTEMP 0xb4
#define BV_UNDO 0xb8
#define BV_ARROW 0xb9
#define BV_LSFIL 0xba
#define BV_WRLNO 0xbc
#define BV_WRSTM 0xbe
#define BV_WRINL 0xbf
#define BV_WHERR 0xc0
#define BV_ERROR 0xc2
#define BV_ERLIN 0xc6
#define BV_WVNUM 0xc8
#define BV_WVBAS 0xca
#define BV_END 0x100
struct _NT_ENT {
char name_type;
char var_type;
short name_ptr;
long value_ptr;
};
#define NT_unset 0
#define NT_stack 1
#define NT_var 2
#define NT_array 3
#define NT_proc_sb 4
#define NT_fun_sb 5
#define NT_rep 6
#define NT_for 7
#define NT_proc_mc 8
#define NT_fun_mc 9
#define VT_null 0
#define VT_string 1
#define VT_float 2
#define VT_int 3
struct _NL_ENT {
char length;
char name[1];
};
struct BV_AREA {
char * bv_bfbas;
char * bv_bfp;
char * bv_tkbas;
char * bv_tkp;
char * bv_pfbas;
char * bv_pfp;
struct _NT_ENT * bv_ntbas;
struct _NT_ENT * bv_ntp;
struct _NL_ENT * bv_nlbas;
struct _NL_ENT * bv_nlp;
char * bv_vvbas;
char * bv_vvp;
char * bv_chbas;
char * bv_chp;
char * bv_rtbas;
char * bv_rtp;
char * bv_lnbas;
char * bv_lnp;
};
#define AT(chanid,row,col) sd_pos(chanid,(timeout_t)(-1),col,row)
#define BORDER(chanid,width,colour) sd_bordr(chanid,(timeout_t)(-1),colour,width)
#define CSIZE(chanid,width,height) sd_setsz(chanid,(timeout_t)(-1),width,height)
#define CURSOR(chanid,x,y) sd_pixp(chanid,(timeout_t)(-1),x,y)
#define CURSOR_OFF(chanid) sd_curs(chanid,(timeout_t)(-1))
#define CURSOR_ON(chanid) sd_cure(chanid,(timeout_t)(-1))
#define INK(chanid,colour) sd_setin(chanid,(timeout_t)(-1),colour)
#define KEYROW(row) keyrow(row)
#define MODE(val) mt_dmode(mode,(timeout_t)(-1))
#define OVER(chanid,mode) sd_setmd(chanid,(timeout_t)(-1),mode)
#define PAPER(chanid,colour) sd_setpa(chaind,(timeout_t)(-1),colour)
#define POSITION(chanid) fs_pos(chan_id,0,1)
#define SET_POSITION(chanid,pos) fs_pos(chanid,pos,0)
#define STRIP(chanid,colour) sd_setst(chanid,(timeout_t)(-1),colour)
#define TAB(chanid,col) sd_tab(chanid,(timeout_t)(-1),col)
#define UNDER(chanid,mode) sd_setul(chanid,(timeout_t)(-1),mode)
void ADATE _P_((long));
void BLOCK _P_((long,int,int,int,int,int));
int CLS _P_((long,int));
long DATE _P_((void));
#ifdef STRFUNC
char *DATE$ _P_((long,char *));
char *DAY$ _P_((long,char *));
int INKEY$ _P_((long,int));
#endif
int PAN _P_((long,int,int));
void REPORT _P_((long,int));
int SCROLL _P_((long,int,int));
void SDATE _P_((long));
int WINDOW _P_((long,int,int,int,int));
#undef _P_
#endif
