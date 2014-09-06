#ifndef _QDOS_H
#define _QDOS_H
#ifndef _QLIB_H
#include <sys/qlib.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
#define ERR_OK 0
#define ERR_NC -1
#define ERR_NJ -2
#define ERR_OM -3
#define ERR_OR -4
#define ERR_BO -5
#define ERR_NO -6
#define ERR_NF -7
#define ERR_EX -8
#define ERR_IU -9
#define ERR_EF -10
#define ERR_DF -11
#define ERR_BN -12
#define ERR_TE -13
#define ERR_FF -14
#define ERR_BP -15
#define ERR_FE -16
#define ERR_XP -17
#define ERR_OV -18
#define ERR_NI -19
#define ERR_RO -20
#define ERR_BL -21
#define ERR_RW -22
#define mt_aclck _mt_aclck
#define mt_activ _mt_activ
#define mt_alchp _mt_alchp
#define mt_alloc _mt_alloc
#define mt_alres _mt_alres
#define mt_baud _mt_baud
#define mt_cjob _mt_cjob
#define mt_dmode _mt_dmode
#define mt_free _mt_free
#define mt_frjob _mt_frjob
#define mt_inf _mt_inf
#define mt_ipcom _mt_ipcom
#define mt_jinf _mt_jinf
#define mt_lnkfr _mt_lnkfr
#define mt_lxint _mt_lxint
#define mt_rxint _mt_rxint
#define mt_lpoll _mt_lpoll
#define mt_rpoll _mt_rpoll
#define mt_lschd _mt_lschd
#define mt_rschd _mt_rschd
#define mt_liod _mt_liod
#define mt_riod _mt_riod
#define mt_ldd _mt_ldd
#define mt_rdd _mt_rdd
#define mt_prior _mt_prior
#define mt_rclck _mt_rclck
#define mt_rechp _mt_rechp
#define mt_reljb _mt_reljb
#define mt_reres _mt_reres
#define mt_rjob _mt_rjob
#define mt_sclck _mt_sclck
#define mt_shrink _mt_shrink
#define mt_susjb _mt_susjb
#define mt_trans _mt_trans
#define mt_trapv _mt_trapv
void mt_aclck _P_((long));
int mt_activ _P_((jobid_t,unsigned char,timeout_t));
char * mt_alchp _P_((long,long *,jobid_t));
char * mt_alloc _P_((char **,long *));
char * mt_alres _P_((long));
void mt_baud _P_((long));
jobid_t mt_cjob _P_((long,long,char *,jobid_t,char **));
void mt_dmode _P_((short *,short *));
long mt_free _P_((void));
int mt_frjob _P_((jobid_t,int));
jobid_t mt_inf _P_((char **,long *));
int mt_ipcom _P_((void *));
int mt_jinf _P_((jobid_t *,jobid_t *,long *,char **));
void mt_lnkfr _P_((char *,char **,long));
void mt_lxint _P_((QL_LINK_t *));
void mt_rxint _P_((QL_LINK_t *));
void mt_lpoll _P_((QL_LINK_t *));
void mt_rpoll _P_((QL_LINK_t *));
void mt_lschd _P_((QL_LINK_t *));
void mt_rschd _P_((QL_LINK_t *));
void mt_liod _P_((QLD_LINK_t *));
void mt_riod _P_((QLD_LINK_t *));
void mt_ldd _P_((QLDDEV_LINK_t *));
void mt_rdd _P_((QLDDEV_LINK_t *));
int mt_prior _P_((jobid_t,unsigned char));
long mt_rclck _P_((void));
void mt_rechp _P_((void *));
JOBHEADER_t * mt_reljb _P_((jobid_t));
int mt_reres _P_((void *));
int mt_rjob _P_((jobid_t,int));
void mt_sclck _P_((long));
int mt_shrink _P_((void *,long));
int mt_susjb _P_((jobid_t,timeout_t,char *));
int mt_trans _P_((void *,void *));
int mt_trapv _P_((long,QLVECTABLE_t *));
#define io_close _io_close
#define io_delete _io_delete
#define io_format _io_format
#define io_open _io_open
#define io_open_qlstr _io_open_qlstr
int io_close _P_((chanid_t));
int io_delete _P_((const char *));
int io_format _P_((const char *,short *,short *));
chanid_t io_open _P_((const char *,long));
chanid_t io_open_qlstr _P_((QLSTR_t *,long));
#define fs_check _fs_check
#define fs_date _fs_date
#define fs_flush _fs_flush
#define fs_headr _fs_headr
#define fs_heads _fs_heads
#define fs_load _fs_load
#define fs_mdinf _fs_mdinf
#define fs_mkdir _fs_mkdir
#define fs_pos _fs_pos
#define fs_posab _fs_posab
#define fs_posre _fs_posre
#define fs_rename _fs_rename
#define fs_save _fs_save
#define fs_trunc _fs_trunc
#define fs_vers _fs_vers
#define fs_xinf _fs_xinf
#define io_edlin _io_edlin
#define io_fbyte _io_fbyte
#define io_fline _io_fline
#define io_fstrg _io_fstrg
#define io_pend _io_pend
#define io_sbyte _io_sbyte
#define io_sstrg _io_sstrg
int fs_check _P_((chanid_t,timeout_t));
int fs_date _P_((chanid_t,timeout_t,char,long *));
int fs_flush _P_((chanid_t,timeout_t));
int fs_headr _P_((chanid_t,timeout_t,void *,short));
int fs_heads _P_((chanid_t,timeout_t,void *,short));
long fs_load _P_((chanid_t,char *,unsigned long));
int fs_mdinf _P_((chanid_t,timeout_t,void *,short *,short *));
int fs_mkdir _P_((chanid_t));
long fs_pos _P_((chanid_t,long,int));
int fs_posab _P_((chanid_t,timeout_t,unsigned long *));
int fs_posre _P_((chanid_t,timeout_t,long *));
int fs_rename _P_((const void *,const void *));
int fs_save _P_((chanid_t,void *,unsigned long));
int fs_trunc _P_((chanid_t,timeout_t));
int fs_vers _P_((chanid_t,timeout_t,long *));
int fs_xinf _P_((chanid_t,timeout_t,struct ext_mdinf *));
int io_edlin _P_((chanid_t,timeout_t,char **,short,short,short *));
int io_fbyte _P_((chanid_t,timeout_t,char *));
int io_fline _P_((chanid_t,timeout_t,void *,short));
int io_fstrg _P_((chanid_t,timeout_t,void *,short));
int io_pend _P_((chanid_t,timeout_t));
int io_sbyte _P_((chanid_t,timeout_t,unsigned char));
int io_sstrg _P_((chanid_t,timeout_t,void *,short));
#define io_fdate _io_fdate
#define io_fvers _io_fvers
#define io_fxinf _io_fxinf
#define io_mkdir _io_mkdir
#define io_rename _io_rename
#define io_trunc _io_trunc
int io_fdate _P_((chanid_t,timeout_t,char,long *));
int io_fvers _P_((chanid_t,timeout_t,long *));
int io_fxinf _P_((chanid_t,timeout_t,struct ext_mdinf *));
int io_mkdir _P_((chanid_t));
int io_rename _P_((const char *,const char *));
int io_trunc _P_((chanid_t,timeout_t));
#define sd_bordr _sd_bordr
#define sd_chenq _sd_chenq
#define sd_clear _sd_clear
#define sd_clrbt _sd_clrbt
#define sd_clrln _sd_clrln
#define sd_clrrt _sd_clrrt
#define sd_clrtp _sd_clrtp
#define sd_cure _sd_cure
#define sd_curs _sd_curs
#define sd_donl _sd_donl
#define sd_extop _sd_extop
#define sd_fill _sd_fill
#define sd_fount _sd_fount
#define sd_ncol _sd_ncol
#define sd_nl _sd_nl
#define sd_nrow _sd_nrow
#define sd_pan _sd_pan
#define sd_panln _sd_panln
#define sd_panrt _sd_panrt
#define sd_pcol _sd_pcol
#define sd_pixp _sd_pixp
#define sd_pos _sd_pos
#define sd_prow _sd_prow
#define sd_pxenq _sd_pxenq
#define sd_recol _sd_recol
#define sd_scrbt _sd_scrbt
#define sd_scrol _sd_scrol
#define sd_scrtp _sd_scrtp
#define sd_setfl _sd_setfl
#define sd_setin _sd_setin
#define sd_setmd _sd_setmd
#define sd_setpa _sd_setpa
#define sd_setst _sd_setst
#define sd_setsz _sd_setsz
#define sd_setul _sd_setul
#define sd_tab _sd_tab
#define sd_wdef _sd_wdef
int sd_bordr _P_((chanid_t,timeout_t,colour_t,short));
int sd_chenq _P_((chanid_t,timeout_t,QLRECT_t *));
int sd_clear _P_((chanid_t,timeout_t));
int sd_clrbt _P_((chanid_t,timeout_t));
int sd_clrln _P_((chanid_t,timeout_t));
int sd_clrrt _P_((chanid_t,timeout_t));
int sd_clrtp _P_((chanid_t,timeout_t));
int sd_cure _P_((chanid_t,timeout_t));
int sd_curs _P_((chanid_t,timeout_t));
int sd_donl _P_((chanid_t,timeout_t));
int sd_extop _P_((chanid_t,timeout_t,int (*)(void),long,long,void *));
int sd_fill _P_((chanid_t,timeout_t,colour_t,QLRECT_t *));
int sd_fount _P_((chanid_t,timeout_t,char *,char *));
int sd_ncol _P_((chanid_t,timeout_t));
int sd_nl _P_((chanid_t,timeout_t));
int sd_nrow _P_((chanid_t,timeout_t));
int sd_pan _P_((chanid_t,timeout_t,short));
int sd_panln _P_((chanid_t,timeout_t,short));
int sd_panrt _P_((chanid_t,timeout_t,short));
int sd_pcol _P_((chanid_t,timeout_t));
int sd_pixp _P_((chanid_t,timeout_t,short,short));
int sd_pos _P_((chanid_t,timeout_t,short,short));
int sd_prow _P_((chanid_t,timeout_t));
int sd_pxenq _P_((chanid_t,timeout_t,QLRECT_t *));
int sd_recol _P_((chanid_t,timeout_t,char *));
int sd_scrbt _P_((chanid_t,timeout_t,short));
int sd_scrol _P_((chanid_t,timeout_t,short));
int sd_scrtp _P_((chanid_t,timeout_t,short));
int sd_setfl _P_((chanid_t,timeout_t,char));
int sd_setin _P_((chanid_t,timeout_t,colour_t));
int sd_setmd _P_((chanid_t,timeout_t,short));
int sd_setpa _P_((chanid_t,timeout_t,colour_t));
int sd_setst _P_((chanid_t,timeout_t,colour_t));
int sd_setsz _P_((chanid_t,timeout_t,short,short));
int sd_setul _P_((chanid_t,timeout_t,char));
int sd_tab _P_((chanid_t,timeout_t,short));
int sd_wdef _P_((chanid_t,timeout_t,colour_t,short,QLRECT_t *));
#define sd_arc _sd_arc
#define sd_elipse _sd_elipse
#define sd_flood _sd_flood
#define sd_gcur _sd_gcur
#define sd_line _sd_line
#define sd_point _sd_point
#define sd_scale _sd_scale
#define sd_iarc _sd_iarc
#define sd_ielipse _sd_ielipse
#define sd_igcur _sd_igcur
#define sd_iline _sd_iline
#define sd_ipoint _sd_ipoint
#define sd_iscale _sd_iscale
int sd_arc _P_((chanid_t,timeout_t,double,double,double,double,double));
int sd_elipse _P_((chanid_t,timeout_t,double,double,double,double,double));
int sd_flood _P_((chanid_t,timeout_t,long));
int sd_gcur _P_((chanid_t,timeout_t,double,double,double,double));
int sd_line _P_((chanid_t,timeout_t,double,double,double,double) );
int sd_point _P_((chanid_t,timeout_t,double,double));
int sd_scale _P_((chanid_t,timeout_t,double,double,double));
int sd_iarc _P_((chanid_t,timeout_t,long,long,long,long,long));
int sd_ielipse _P_((chanid_t,timeout_t,long,long,long,long,long));
int sd_igcur _P_((chanid_t,timeout_t,long,long,long,long));
int sd_iline _P_((chanid_t,timeout_t,long,long,long,long));
int sd_ipoint _P_((chanid_t,timeout_t,long,long));
int sd_iscale _P_((chanid_t,timeout_t,long,long,long));
#define cn_date _cn_date
#define cn_day _cn_day
#define cn_itobb _cn_itobb
#define cn_itobw _cn_itobw
#define cn_itobl _cn_itobl
#define cn_itod _cn_itod
#define cn_itohb _cn_itohb
#define cn_itohw _cn_itohw
#define cn_itohl _cn_itohl
#define io_qeof _io_qeof
#define io_qin _io_qin
#define io_qout _io_qout
#define io_qset _io_qset
#define io_qtest _io_qtest
#define io_serio _io_serio
#define io_serq _io_serq
#define mm_alchp _mm_alchp
#define mm_alloc _mm_alloc
#define mm_lnkfr _mm_lnkfr
#define mm_rechp _mm_rechp
#define ut_con _ut_con
#define ut_cstr _ut_cstr
#define ut_err _ut_err
#define ut_err0 _ut_err0
#define ut_link _ut_link
#define ut_mint _ut_mint
#define ut_mtext _ut_mtext
#define ut_scr _ut_scr
#define ut_unlnk _ut_unlnk
#define ut_window _ut_window
char * cn_date _P_((char *,long));
char * cn_day _P_((char *,long));
void cn_itobb _P_((char *,char *));
void cn_itobw _P_((char *,short *));
void cn_itobl _P_((char *,long *));
int cn_itod _P_((char *,short *));
void cn_itohb _P_((char *,char *));
void cn_itohw _P_((char *,short *));
void cn_itohl _P_((char *,long *));
int io_qeof _P_((char *));
int io_qin _P_((char *,int));
int io_qout _P_((char *,char *));
void io_qset _P_((char *,long));
int io_qtest _P_((char *,char *,long *));
int io_serio _P_((chanid_t,timeout_t,int,long *,long *,char **,int ** ));
int io_serq _P_((chanid_t,timeout_t,int,long *,long *,char **));
char * mm_alchp _P_((long *));
char * mm_alloc _P_((char **,long *));
void mm_lnkfr _P_((char *,char **,long));
void mm_rechp _P_((char *));
chanid_t ut_con _P_((WINDOWDEF_t *));
int ut_cstr _P_((const QLSTR_t *,const QLSTR_t *,int));
void ut_err _P_((long,chanid_t));
void ut_err0 _P_((long));
void ut_link _P_((void *,void *));
int ut_mint _P_((chanid_t,int));
int ut_mtext _P_((chanid_t,const QLSTR_t *));
chanid_t ut_scr _P_((WINDOWDEF_t *));
void ut_unlnk _P_((void *,void *));
chanid_t ut_window _P_((const char *,char *));
#undef _P_
#endif
