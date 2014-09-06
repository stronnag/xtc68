#ifndef _QPTR_H
#define _QPTR_H
#include <stdarg.h>
#ifndef _SYS_TYPES_H
#include <sys_types.h>
#endif
#ifndef _QLIB_H
#include <sys/qlib.h>
#endif
#define CL_MBORD 0000
#define CL_MBACK 0007
#define CL_MHIGH 0000
#define CL_BBORD 0004
#define CL_MFILL 0134
#define CL_MVERT 0237
#define CL_MSHADE 0037
#define CL_MOUTL 0004
#define CL_MIUNAV 0004
#define CL_MPUNAV 0007
#define CL_MIAVBL 0000
#define CL_MPAVBL 0007
#define CL_MISLCT 0007
#define CL_MPSLCT 0000
#define CL_MISLC1 0000
#define CL_MPSLC1 0004
#define CL_BIAVBL 0000
#define CL_BPAVBL 0007
#define CL_BISLCT 0007
#define CL_BPSLCT 0000
#define CL_IBORD 0004
#define CL_IBACK 0007
#define CL_IINK 0000
#define CL_ILOW 0002
#define CL_SBORD 0007
#define CL_SBACK 0004
#define CL_SINK 0000
#define CL_SLOW 0007
#define CL_WBORD 0004
#define CL_WBACK 0007
#define CL_WINK 0000
#define CL_WHIGH 0000
#define CL_WIUNAV 0004
#define CL_WPUNAV 0007
#define CL_WIAVBL 0000
#define CL_WPAVBL 0007
#define CL_WISLCT 0000
#define CL_WPSLCT 0004
#define CL_WRROW 0004
#define CL_WBARB 0007
#define CL_WBARS 0000
#define CL_ERPAP 0002
#define CL_ERINK 0007
#define DEFXY (short) -1,(short) -1
typedef unsigned char qchar_t;
typedef struct WM_sprite WM_sprite_t;
typedef struct WM_sprite WM_blob_t;
typedef struct WM_sprite WM_pattern_t;
typedef struct WM_wwork WM_wwork_t;
typedef struct WM_wstat WM_wstat_t;
typedef struct WM_wscale WM_wscale_t;
typedef struct WM_appl WM_appl_t;
typedef struct WM_appw WM_appw_t;
typedef struct WM_info WM_info_t;
typedef struct WM_infw WM_infw_t;
typedef struct WM_litm WM_litm_t;
typedef struct WM_prec WM_prec_t;
typedef struct WM_splst WM_splst_t;
typedef struct WM_action WM_action_t;
typedef struct WM_pwcb WM_pwcb_t;
typedef struct WM_mobj WM_mobj_t;
typedef struct WM_rowl WM_rowl_t;
typedef struct WM_mctrl WM_mctrl_t;
typedef struct QD_text {
short len;
qchar_t chrs[1];
} QD_text_t;
#define QD_TEXT(l) struct {short len; qchar_t chrs[(l)+1];}
#define QD_TEXTN(n,l) struct n {short len; qchar_t chrs[(l)+1];}
#define QD_TEXTI(i,t) QD_TEXT (sizeof(t)-1) i = {(sizeof(t)-1),t}
#define QD_STXT(s,t,l) strncpy ((char *)((s).chrs),t,(size_t)l); \
(s).chrs[l]=0; \
(s).len=strlen((char *)((s).chrs))
#define NO_WCLEAR -32768
#define TYP_TEXT (char) 0
#define TYP_SPRITE (char) 2
#define TYP_BLOB (char) 4
#define TYP_PATT (char) 6
#define PT_KYSTK 0x01
#define PT_KYPRS 0x02
#define PT_KEYUP 0x04
#define PT_MOVE 0x08
#define PT_OUTWN 0x10
#define PT_INWIN 0x20
#define PT_WQURY 0x80
#define PT_WMOVR 0x81
#define PT_WSIZR 0x82
#define PT_SPLIT 0x0100
#define PT_JOIN 0x0200
#define PT_PAN 0x0400
#define PT_SCROL 0x0800
#define PT_DO 0x010000
#define PT_CAN 0x020000
#define PT_HELP 0x040000
#define PT_WMOVE 0x080000
#define PT_WSIZE 0x100000
#define PT_ZZZZ 0x200000
#define PT_WAKE 0x400000
#define PT__KYST 0
#define PT__KYPR 1
#define PT__KYUP 2
#define PT__MOVE 3
#define PT__OUTW 4
#define PT__INWN 5
#define PT__SPLT 8
#define PT__JOIN 9
#define PT__PAN 10
#define PT__SCRL 11
#define PT__DO 16
#define PT__CAN 17
#define PT__HELP 18
#define PT__WMOV 19
#define PT__WSIZ 20
#define PT__ZZZZ 21
#define PT__WAKE 22
#define K_HIT 1
#define K_DO 2
#define K_CANCEL 3
#define K_HELP 4
#define K_MOVE 5
#define K_SIZE 6
#define K_SLEEP 7
#define K_WAKE 8
#define K_TAB 9
#define WSI_UNAV 0x10
#define WSI_AVBL 0x00
#define WSI_SLCT 0x80
#define WSI_MKUN 0x11
#define WSI_MKAV 0x01
#define WSI_MKSL 0x81
#define WSI__CHG 0x00
#define WW_XARROW 12
#define WW_YARROW 6
#define WW_XBAR 8
#define WW_YBAR 5
typedef struct WM_wsiz
{
short xsize;
short ysize;
short xorg;
short yorg;
} WM_wsiz_t;
typedef struct WM_wattr
{
short flag;
short borw;
short borc;
short papr;
} WM_wattr_t;
typedef struct WM_swdef
{
short xsize;
short ysize;
short xorg;
short yorg;
short flag;
short borw;
short borc;
short papr;
WM_sprite_t * sprite;
} WM_swdef_t;
struct WM_wwork
{
WM_wstat_t * wstat;
WM_wscale_t * wscale;
long chid;
WM_prec_t * pprec;
long psave;
long spar1;
short spar2;
char spar3;
char pulld;
void * splst;
short xsize;
short ysize;
short xorg;
short yorg;
short flag;
short borw;
short borc;
short papr;
void * sprite;
short curw;
short curc;
short uback;
short uink;
WM_blob_t * ublob;
WM_pattern_t * upatt;
short aback;
short aink;
WM_blob_t * ablob;
WM_pattern_t * apatt;
short sback;
short sink;
WM_blob_t * sblob;
WM_pattern_t * spatt;
void * help;
short ninfo;
short ninob;
WM_infw_t * pinfo;
short nlitm;
WM_litm_t * plitm;
short nappl;
WM_appl_t * pappl;
};
struct WM_infw
{
short xsize;
short ysize;
short xorg;
short yorg;
short flag;
short borw;
short borc;
short papr;
WM_info_t * pobl;
};
struct WM_info
{
short xsize;
short ysize;
short xorg;
short yorg;
char type;
char spar;
union
{ struct
{ short ink;
char cwid;
char chgt;
} t;
void * comb;
} attr;
void * pobj;
};
struct WM_litm
{
short xsize;
short ysize;
short xorg;
short yorg;
char xjst;
char yjst;
char type;
unsigned char skey;
void *pobj;
short item;
WM_action_t *pact;
};
struct WM_appl
{ WM_appw_t *pappw;
};
struct WM_appw
{
short xsize;
short ysize;
short xorg;
short yorg;
short flag;
short borw;
short borc;
short papr;
void *pspr;
WM_action_t *draw;
WM_action_t *hit;
WM_action_t *ctrl;
short nxsc;
short nysc;
char skey;
char spr1;
short spr2;
WM_pwcb_t *xpwcb;
short xinsz;
short xinsp;
short xiciw;
short xicic;
short xiback;
short xiink;
WM_blob_t *xiblob;
WM_blob_t *xipatt;
short xpsac;
short xpsbc;
short xpssc;
WM_pwcb_t *ypwcb;
short yinsz;
short yinsp;
short yiciw;
short yicic;
short yiback;
short yiink;
WM_blob_t *yiblob;
WM_pattern_t *yipatt;
short ypsac;
short ypsbc;
short ypssc;
void *pstat;
};
struct WM_menw
{
short xsize;
short ysize;
short xorg;
short yorg;
short flag;
short borw;
short borc;
short papr;
void *pspr;
WM_action_t *draw;
WM_action_t *hit;
WM_action_t *ctrl;
short nxsc;
short nysc;
char skey;
char spr1;
short spr2;
WM_pwcb_t *xpwcb;
short xinsz;
short xinsp;
short xiciw;
short xicic;
short xiback;
short xiink;
WM_blob_t *xiblob;
WM_pattern_t *xipatt;
short xpsac;
short xpsbc;
short xpssc;
WM_pwcb_t *ypwcb;
short yinsz;
short yinsp;
short yiciw;
short yicic;
short yiback;
short yiink;
WM_blob_t *yiblob;
WM_pattern_t *yipatt;
short ypsac;
short ypsbc;
short ypssc;
char *mstt;
short curw;
short curc;
short uback;
short uink;
WM_blob_t *ublob;
WM_pattern_t *upatt;
short aback;
short aink;
WM_blob_t *ablob;
WM_pattern_t *apatt;
short sback;
short sink;
WM_blob_t *sblob;
WM_pattern_t *spatt;
short ncol;
short nrow;
short xoff;
short yoff;
union
{ struct
{ short _size;
short _spce;
} c;
WM_splst_t *xspc;
}xs;
union
{ struct
{ short _size;
short _spce;
} c;
WM_splst_t *xspc;
}ys;
WM_mobj_t *xind;
WM_mobj_t *yind;
WM_rowl_t *rowl;
};
struct WM_splst
{
short size;
short spce;
};
struct WM_rowl
{
WM_mobj_t *rows;
WM_mobj_t *rowe;
};
struct WM_mobj
{
char xjst;
char yjst;
char type;
char skey;
void *pobj;
short item;
WM_action_t *pact;
};
struct WM_action
{
short __jsr_______;
void *__interface_;
void *__function__;
};
#define JSR 0x4EB9
struct WM_wstat
{
WM_wwork_t *wwork;
void *wdef;
long chid;
short swnr;
short xpos;
short ypos;
char kstk;
char kprs;
long evnt;
short xsiz;
short ysiz;
short xorg;
short yorg;
short ptpsx;
short ptpsy;
short wmode;
short spar0;
long spar1;
void *ciact;
short citem;
short cibrw;
short cipap;
short cispr;
short cihxs;
short cihys;
short cihxo;
short cihyo;
char litem[40];
};
struct WM_prec
{
chanid_t chid;
unsigned short swnr;
signed short xpos;
signed short ypos;
unsigned char kstk;
unsigned char kprs;
unsigned long evnt;
signed short xsiz;
signed short ysiz;
signed short xorg;
signed short yorg;
};
#define WM_PWCB(n) struct {short nsec; struct{short pos;short stt;short siz;}s[n];}
struct WM_pwcb
{
short nsec;
struct
{ short pos;
short stt;
short siz;
} s[1];
};
struct WM_mctrl
{
short psit;
short hitp;
short barl;
short evnt;
};
#define WM_WSCALE(n) struct {short xmin,ymin,xmax,ymax,xinc,yinc; char s8[n][4];}
struct WM_wscale {short xmin,ymin,xmax,ymax,xinc,yinc; char s8[1][4];};
struct WM_sprite
{ short form;
char time;
char adapt;
short xsize;
short ysize;
short xorg;
short yorg;
long patt;
long mask;
long next;
};
#define WM_blob WM_sprite
#define WM_pattern WM_sprite
#define FORM_QL4 0x0100
#define FORM_QL8 0x0101
#define SP_patt 12
#define SP_mask 16
#define SP_next 20
extern WM_sprite_t wm_sprite_arrow;
extern WM_sprite_t wm_sprite_cf1;
extern WM_sprite_t wm_sprite_cf2;
extern WM_sprite_t wm_sprite_cf3;
extern WM_sprite_t wm_sprite_cf4;
extern WM_sprite_t wm_sprite_f1;
extern WM_sprite_t wm_sprite_f2;
extern WM_sprite_t wm_sprite_f3;
extern WM_sprite_t wm_sprite_f4;
extern WM_sprite_t wm_sprite_f5;
extern WM_sprite_t wm_sprite_f6;
extern WM_sprite_t wm_sprite_f7;
extern WM_sprite_t wm_sprite_f8;
extern WM_sprite_t wm_sprite_f9;
extern WM_sprite_t wm_sprite_f10;
extern WM_sprite_t wm_sprite_hand;
extern WM_sprite_t wm_sprite_insg;
extern WM_sprite_t wm_sprite_insl;
extern WM_sprite_t wm_sprite_left;
extern WM_sprite_t wm_sprite_move;
extern WM_sprite_t wm_sprite_null;
extern WM_sprite_t wm_sprite_size;
extern WM_sprite_t wm_sprite_sleep;
extern WM_sprite_t wm_sprite_wake;
extern WM_sprite_t wm_sprite_zero;
#define mes_arrow wm_sprite_arrow
#define mes_cf1 wm_sprite_cf1
#define mes_cf2 wm_sprite_cf2
#define mes_cf3 wm_sprite_cf3
#define mes_cf4 wm_sprite_cf4
#define mes_f1 wm_sprite_f1
#define mes_f2 wm_sprite_f2
#define mes_f3 wm_sprite_f3
#define mes_f4 wm_sprite_f4
#define mes_f5 wm_sprite_f5
#define mes_f6 wm_sprite_f6
#define mes_f7 wm_sprite_f7
#define mes_f8 wm_sprite_f8
#define mes_f9 wm_sprite_f9
#define mes_f10 wm_sprite_f10
#define mes_hand wm_sprite_hand
#define mes_insg wm_sprite_insg
#define mes_insl wm_sprite_insl
#define mes_left wm_sprite_left
#define mes_move wm_sprite_move
#define mes_null wm_sprite_null
#define mes_size wm_sprite_size
#define mes_slee wm_sprite_sleep
#define mes_wake wm_sprite_wake
#define mes_zero wm_sprite_zero
#define iop_flim _iop_flim
#define iop_lblb _iop_lblb
#define iop_outl _iop_outl
#define iop_pick _iop_pick
#define iop_pinf _iop_pinf
#define iop_rptr _iop_rptr
#define iop_rpxl _iop_rpxl
#define iop_rspw _iop_rspw
#define iop_slnk _iop_slnk
#define iop_svpw _iop_svpw
#define iop_spry _iop_spry
#define iop_sptr _iop_sptr
#define iop_swdf _iop_swdf
#define iop_wblb _iop_wblb
#define iop_wrst _iop_wrst
#define iop_wsav _iop_wsav
#define iop_wspt _iop_wspt
#define wm_actli _wm_actli
#define wm_actme _wm_actme
#define wm_ctlaw _wm_ctlaw
#define wm_drwaw _wm_drwaw
#define wm_hitaw _wm_hitaw
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
int iop_flim _P_((chanid_t,timeout_t,WM_wsiz_t *));
int iop_lblb _P_((chanid_t,timeout_t,short,short,short,short,\
WM_blob_t *,WM_pattern_t *));
int iop_outl _P_((chanid_t,timeout_t,short,short,short,void *));
int iop_pick _P_((chanid_t,timeout_t,jobid_t));
void * iop_pinf _P_((chanid_t,timeout_t,long *));
int iop_rptr _P_((chanid_t,timeout_t,short *,short *,unsigned short,\
void *));
int iop_rpxl _P_((chanid_t,timeout_t,short *,short *,short,short *));
int iop_rspw _P_((chanid_t,timeout_t,WM_wsiz_t *,short,short,int,\
void *));
void * iop_slnk _P_((chanid_t,timeout_t,void *,short,short));
int iop_svpw _P_((chanid_t,timeout_t,WM_wsiz_t *,short,short,\
short,short,void **));
int iop_spry _P_((chanid_t,timeout_t,short,short,WM_blob_t *,\
WM_pattern_t *,long));
int iop_sptr _P_((chanid_t,timeout_t,short *,short *,char));
int iop_swdf _P_((chanid_t,timeout_t,void *));
int iop_wblb _P_((chanid_t,timeout_t,short,short,WM_blob_t *,\
WM_pattern_t *));
int iop_wrst _P_((chanid_t,timeout_t,void *,char));
int iop_wsav _P_((chanid_t,timeout_t,void *,long));
int iop_wspt _P_((chanid_t,timeout_t,short,short,WM_sprite_t *));
#define wm_chwin _wm_chwin
#define wm_drbdr _wm_drbdr
#define wm_ename _wm_ename
#define wm_erstr _wm_erstr
#define wm_fsize _wm_fsize
#define wm_idraw _wm_idraw
#define wm_index _wm_index
#define wm_ldraw _wm_ldraw
#define wm_mdraw _wm_mdraw
#define wm_mhit _wm_mhit
#define wm_msect _wm_msect
#define wm_pansc _wm_pansc
#define wm_prpos _wm_prpos
#define wm_pulld _wm_pulld
#define wm_rname _wm_rname
#define wm_rptr _wm_rptr
#define wm_rptrt _wm_rptrt
#define wm_setup _wm_setup
#define wm_smenu _wm_smenu
#define wm_stlob _wm_stlob
#define wm_stiob _wm_stiob
#define wm_swapp _wm_swapp
#define wm_swdef _wm_swdef
#define wm_swinf _wm_swinf
#define wm_swlit _wm_swlit
#define wm_swsec _wm_swsec
#define wm_unset _wm_unset
#define wm_upbar _wm_upbar
#define wm_wdraw _wm_wdraw
#define wm_wrset _wm_wrset
#define wm_clbdr _wm_clbdr
#define wm_cluns _wm_cluns
#define wm_drbdr _wm_drbdr
#define wm_findv _wm_findv
#define wm__pnsc (WM_action_t *)_wm__pnsc
#define wm__mhit (WM_action_t *)_wm__mhit
int wm_chwin _P_((WM_wwork_t *,short *,short *));
int wm_drbdr _P_((WM_wwork_t *));
int wm_ename _P_((chanid_t,QD_text_t *));
int wm_erstr _P_((long,QD_text_t *));
int wm_fsize _P_((short *,short *,void *));
int wm_idraw _P_((WM_wwork_t *,long));
int wm_index _P_((WM_wwork_t *,WM_swdef_t *));
int wm_ldraw _P_((WM_wwork_t *,char));
int wm_mdraw _P_((WM_wwork_t *,WM_swdef_t *,char));
int wm_mhit _P_((WM_wwork_t *,WM_appw_t *,short,short,short,short));
int wm_msect _P_((WM_wwork_t *,WM_appw_t *,short,short,short,short,\
WM_mctrl_t *));
int wm_pansc _P_((WM_wwork_t *,WM_appw_t *,WM_mctrl_t *));
int wm_prpos _P_((WM_wwork_t *,short,short));
int wm_pulld _P_((WM_wwork_t *,short,short));
int wm_rname _P_((chanid_t,QD_text_t *));
int wm_rptr _P_((WM_wwork_t *));
int wm_rptrt _P_((WM_wwork_t *,short,unsigned short));
int wm_setup _P_((chanid_t,short,short,void *,WM_wstat_t *,\
WM_wwork_t **,long));
int wm_smenu _P_((short,short,WM_wstat_t *,void **,WM_wwork_t **));
int wm_stlob _P_((WM_wwork_t *,void *,short));
int wm_stiob _P_((WM_wwork_t *,void *,short,short));
chanid_t wm_swapp _P_((WM_wwork_t *,short,long));
chanid_t wm_swdef _P_((WM_wwork_t *,WM_appw_t *,chanid_t));
chanid_t wm_swinf _P_((WM_wwork_t *,short,long));
chanid_t wm_swlit _P_((WM_wwork_t *,short,long));
chanid_t wm_swsec _P_((WM_wwork_t *,WM_appw_t *,short,short,long));
int wm_unset _P_((WM_wwork_t *));
int wm_upbar _P_((WM_wwork_t *,WM_swdef_t *,short,short));
int wm_wdraw _P_((WM_wwork_t *));
int wm_wrset _P_((WM_wwork_t *));
int wm_clbdr _P_((WM_wwork_t *));
int wm_cluns _P_((WM_wwork_t *));
int wm_drbdr _P_((WM_wwork_t *));
void * wm_findv _P_((chanid_t));
void * _wm__mhit _P_((void));
void * _wm__pnsc _P_((void));
#define bt_frame _bt_frame
#define bt_free _bt_free
#define bt_prpos _bt_prpos
#define trap15 _Trap15
int bt_frame _P_((chanid_t,WM_swdef_t *));
int bt_free _P_((void));
int bt_prpos _P_((WM_wwork_t *));
int trap15 _P_((void *,...));
int wm_actli _P_((void));
int wm_actme _P_((void));
int wm_ctlaw _P_((void));
int wm_drwaw _P_((void));
int wm_hitaw _P_((void));
#define AHIT_NORMAL 65535
void consetup_qpac _P_((chanid_t,WINDOWDEF_t *));
int consetup_qpac_move _P_((chanid_t));
int consetup_qpac_resize _P_((chanid_t));
int consetup_qpac_sleep _P_((chanid_t));
int consetup_qpac_poll _P_((chanid_t));
int outlmove _P_((chanid_t));
#undef _P_
#endif
