#ifndef _CHANNELS_H
#define _CHANNELS_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
struct qdos_queue {
struct qdos_queue *q_nextq;
char * q_end;
char * q_nextin;
char * q_nxtout;
char q_queue[2];
};
struct chan_defb {
unsigned long ch_len;
char * ch_drivr;
jobid_t ch_owner;
char * ch_rflag;
unsigned short ch_tag;
char ch_stat;
char ch_actn;
jobid_t ch_jobwt;
};
struct ser_cdefb {
struct chan_defb ser_cdef;
unsigned short ser_chno;
unsigned char ser_par;
char ser_txhs;
char ser_prot;
char filler;
struct qdos_queue ser_rxq;
char ser_dum1[80];
struct qdos_queue ser_txq;
char ser_dum2[80];
};
#define SER_CDEFBSIZE 0xE4
struct net_cdefb {
struct chan_defb net_cdef;
unsigned char net_hedr;
unsigned char net_self;
unsigned char net_blkl;
unsigned char net_blkh;
unsigned char net_type;
unsigned char net_nbyt;
unsigned char net_dchk;
unsigned char net_hchk;
char net_data[255];
char net_rpnt;
};
#define NET_CDEFBSIZE 0x120
struct pipe_cdefb {
struct chan_defb ch_cdef;
struct qdos_queue * ch_qin;
struct qdos_queue * ch_qout;
};
#define PIPE_CDEFBSIZE 0x20
struct scrn_info {
unsigned short sd_xmin;
unsigned short sd_ymin;
unsigned short sd_xsize;
unsigned short sd_ysize;
unsigned short sd_borwd;
unsigned short sd_xpos;
unsigned short sd_ypos;
unsigned short sd_xinc;
unsigned short sd_yinc;
char * sd_font[2];
char * sd_scrb;
unsigned long sd_pmask;
unsigned long sd_smask;
unsigned long sd_imask;
char sd_cattr;
char sd_curf;
colour_t sd_pcolr;
colour_t sd_icolr;
colour_t sd_bcolr;
unsigned char sd_nlsta;
unsigned char sd_fmod;
struct QLFLOAT sd_yorg;
struct QLFLOAT sd_xorg;
struct QLFLOAT sd_scal;
char * sd_fbuf;
char * sd_fuse;
short sd_linel;
short sd_dummy;
};
struct scrn_xinfo {
unsigned short sd_xhits;
unsigned short sd_yhits;
unsigned short sd_xhito;
unsigned short sd_yhito;
unsigned short sd_xouts;
unsigned short sd_youts;
unsigned short sd_xouto;
unsigned short sd_youto;
char * sd_prwlb;
#define sd_pprwn sd_prwlb;
char * sd_prwlt;
char * sd_sewlt;
char * sd_wsave;
unsigned long sd_wssiz;
char * sd_wwdef;
char sd_wlstt;
unsigned char sd_prwin;
char sd_wmode;
unsigned char sd_mysav;
char sd_wmove;
char filler[3];
};
struct qram_xcdef {
struct QLRECT qx_hitrect;
struct QLRECT qx_outrect;
char * qx_pllbu;
char * qx_pwind;
char * qx_plltd;
char * qx_swind;
char * qx_wsave;
unsigned long qx_wssize;
char * qx_pwwdef;
char qx_lock;
unsigned char qx_ppwin;
char qx_smode;
unsigned char qx_mysave;
char qx_wmove;
};
struct scr_cdefb {
struct chan_defb scr_cdef;
struct scrn_info scr_info;
};
struct scr_xcdefb {
struct chan_defb scr_cdef;
struct scrn_xinfo scr_xinfo;
struct scrn_info scr_info;
};
struct scr_qram_cdefb {
struct chan_defb scr_cdef;
struct qram_xcdef scr_xcdef;
struct scrn_info scr_info;
};
#define SCR_CDEFBSIZE 0x70
#define SCR_QRCDEFBSIZE 0xA0
#define CA_UNDERLINE 0x1
#define CA_FLASH 0x2
#define CA_TRANS 0x4
#define CA_XOR 0x8
#define CA_DOUBLE_HEIGHT 0x10
#define CA_EXT_WIDTH 0x20
#define CA_DBLE_WIDTH 0x40
#define CA_GRAF_POS_CHAR 0x80
struct con_cdefb {
struct chan_defb con_cdef;
struct scrn_info con_info;
union {
struct {
long sdu_linel;
struct qdos_queue sdu_kbd;
} sd_js;
struct qdos_queue sd_jm;
} con_end;
};
struct con_qram_cdefb {
struct chan_defb con_cdef;
struct qram_xcdef scr_xcdef;
struct scrn_info con_info;
union {
struct {
long sdu_linel;
struct qdos_queue sdu_kbd;
} sd_js;
struct qdos_queue sd_jm;
} con_end;
};
struct fs_cdefb {
struct chan_defb fs_cdef;
struct fs_cdefb *fs_next;
unsigned char fs_access;
unsigned char fs_drive;
unsigned short fs_filnr;
unsigned short fs_nblok;
unsigned short fs_nbyte;
unsigned short fs_eblok;
unsigned short fs_ebyte;
char * fs_cblock;
unsigned char fs_updt;
char fs_res1;
long fs_res2;
struct QLSTR fs_name;
char fs_pad[106];
};
#define FSCDEF_SIZE 0xA0
struct physdef_block {
unsigned long fd_len;
long fd_spare;
long fd_owner;
char * fd_rflag;
char * fs_drivr;
unsigned char fs_drivn;
unsigned short fs_random;
char fs_name[10];
unsigned char fs_files;
};
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#ifdef __LIBRARY__
struct chan_defb * _getcdb _P_((chanid_t));
int _Getsddata _P_((chanid_t,struct scrn_info **));
struct QLDDEV_LINK *_isfscdb _P_((struct chan_defb *));
#endif
#undef _P_
#endif
