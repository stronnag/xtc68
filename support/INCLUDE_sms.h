#ifndef _SMS_H
#define _SMS_H
#ifndef _QLIB_H
#include <sys/qlib.h>
#endif
#define ERR_OK 0
#define ERR_NC -1
#define ERR_IJOB -2
#define ERR_IMEM -3
#define ERR_ORNG -4
#define ERR_BFFL -5
#define ERR_ICHN -6
#define ERR_FDNF -7
#define ERR_FEX -8
#define ERR_FDIU -9
#define ERR_EOF -10
#define ERR_DRFL -11
#define ERR_INAM -12
#define ERR_TRNE -13
#define ERR_PRTY -13
#define ERR_FMTF -14
#define ERR_IPAR -15
#define ERR_MCHK -16
#define ERR_IEXP -17
#define ERR_OVFL -18
#define ERR_NIMP -19
#define ERR_RDO -20
#define ERR_ISYN -21
#define ERR_RWF -22
typedef struct {
short ldm_type;
short ldm_group;
short ldm_lang;
long ldm_next;
} SMS_LDM;
#define LDM_TYPE_PREF 0
#define LDM_TYPE_KEYB 1
#define LDM_TYPE_PTRTAB 2
#define LDM_TYPE_MSGTAB 3
#define LDM_LANG_FRENCH 33
#define LDM_LANG_ENGLISH 44
#define LDM_LANG_GERMAN 49
#define LDM_CAR_FRENCH"F   "
#define LDM_CAR_ENGLISH"GB  "
#define LDM_CAR_GERMAN"D   "
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
#define sms_achp _sms_achp
#define sms_achp_acsi _sms_achp_acsi
#define sms_acjb _sms_acjb
#define sms_alhp _sms_alhp
#define sms_arpa _sms_arpa
#define sms_artc _sms_artc
#define sms_comm _sms_comm
#define sms_crjb _sms_crjb
#define sms_dmod _sms_dmod
#define sms_exv _sms_exv
#define sms_frjb _sms_frjb
#define sms_frtp _sms_frtp
#define sms_hdop _sms_hdop
#define sms_info _sms_info
#define sms_injb _sms_injb
#define sms_lexi _sms_lexi
#define sms_lenq _sms_lenq
#define sms_lfsd _sms_lfsd
#define sms_liod _sms_liod
#define sms_lpol _sms_lpol
#define sms_lset _sms_lset
#define sms_lshd _sms_lshd
#define sms_rchp _sms_rchp
#define sms_rehp _sms_rehp
#define sms_rexi _sms_rexi
#define sms_rfsd _sms_rfsd
#define sms_riod _sms_riod
#define sms_rpol _sms_rpol
#define sms_rshd _sms_rshd
#define sms_rmjb _sms_rmjb
#define sms_rrpa _sms_rrpa
#define sms_rrtc _sms_rrtc
#define sms_schp _sms_schp
#define sms_spjb _sms_spjb
#define sms_srtc _sms_srtc
#define sms_ssjb _sms_ssjb
#define sms_trns _sms_trns
#define sms_usjb _sms_usjb
char * sms_achp _P_((long,long *,jobid_t));
char * sms_achp_acsi _P_((long,long *,jobid_t));
int sms_acjb _P_((jobid_t,unsigned char,timeout_t));
void * sms_alhp _P_((void **,long *));
void * sms_arpa _P_((long));
void sms_artc _P_((long));
void sms_cach _P_((long));
void sms_comm _P_((long));
jobid_t sms_crjb _P_((long,long,void *,jobid_t,void **));
void sms_dmod _P_((short *,short *));
int sms_exv _P_((long,QLVECTABLE_t *));
void sms_fprm _P_((long *,char **,long *));
int sms_frjb _P_((jobid_t,int));
long sms_frtp _P_((void));
int sms_hdop _P_((void *));
jobid_t sms_info _P_((void **,long *));
int sms_injb _P_((jobid_t *,jobid_t *,long *,void **));
void sms_iopr _P_((short));
void sms_lenq _P_((long *,char *));
void sms_lexi _P_((QL_LINK_t *));
void sms_lfsd _P_((QLDDEV_LINK_t *));
void sms_liod _P_((QLD_LINK_t *));
void sms_lldm _P_((void *));
void sms_lpol _P_((QL_LINK_t *));
void sms_lset _P_((long *,char *));
void sms_lshd _P_((QL_LINK_t *));
void * sms_mptr _P_((long));
void sms_pset _P_((long));
void sms_rchp _P_((void *));
void sms_rehp _P_((void *,void **,long));
void sms_rexi _P_((QL_LINK_t *));
void sms_rfsd _P_((QLDDEV_LINK_t *));
void sms_riod _P_((QLD_LINK_t *));
void sms_rpol _P_((QL_LINK_t *));
void sms_rshd _P_((QL_LINK_t *));
int sms_rmjb _P_((jobid_t,int));
int sms_rrpa _P_((void *));
long sms_rrtc _P_((void));
void * sms_schp _P_((long,long *,void *));
int sms_sevt _P_((jobid_t,event_t));
int sms_spjb _P_((jobid_t,unsigned char));
void sms_srtc _P_((long));
int sms_ssjb _P_((jobid_t,timeout_t,char *));
int sms_trns _P_((const void *,const void *));
JOBHEADER_t * sms_usjb _P_((jobid_t));
int sms_wevt _P_((event_t *,timeout_t));
#define ioa_clos _ioa_clos
#define ioa_delf _ioa_delf
#define ioa_frmt _ioa_frmt
#define ioa_open _ioa_open
int ioa_clos _P_((chanid_t));
int ioa_cnam _P_((chanid_t,char *,short));
int ioa_delf _P_((const char *));
int ioa_frmt _P_((const char *,int *,int *));
chanid_t ioa_open _P_((const char *,int));
int ioa_sown _P_((chanid_t,jobid_t));
#define iob_elin _iob_elin
#define iob_fbyt _iob_fbyt
#define iob_flin _iob_flin
#define iob_fmul _iob_fmul
#define iob_test _iob_test
#define iob_sbyt _iob_sbyt
#define iob_smul _iob_smul
#define iob_suml _iob_suml
#define iof_chek _iof_chek
#define iof_date _iof_date
#define iof_flsh _iof_flsh
#define iof_load _iof_load
#define iof_minf _iof_minf
#define iof_mkdr _iof_mkdr
#define iof_posa _iof_posa
#define iof_posr _iof_posr
#define iof_rhdr _iof_rhdr
#define iof_rnam _iof_rnam
#define iof_save _iof_save
#define iof_shdr _iof_shdr
#define iof_trnc _iof_trnc
#define iof_vers _iof_vers
#define iof_xinf _iof_xinf
int iob_elin _P_((chanid_t,timeout_t,char **,short,short,short *));
int iob_fbyt _P_((chanid_t,timeout_t,char *));
int iob_flin _P_((chanid_t,timeout_t,void *,short));
int iob_fmul _P_((chanid_t,timeout_t,void *,short));
int iob_test _P_((chanid_t,timeout_t));
int iob_sbyt _P_((chanid_t,timeout_t,unsigned char));
int iob_smul _P_((chanid_t,timeout_t,void *,short));
int iob_suml _P_((chanid_t,timeout_t,void *,short));
int iof_chek _P_((chanid_t,timeout_t));
int iof_date _P_((chanid_t,timeout_t,char,long *));
int iof_flsh _P_((chanid_t,timeout_t));
long iof_load _P_((chanid_t,char *,unsigned long));
int iof_minf _P_((chanid_t,timeout_t,void *,short *,short *));
int iof_mkdr _P_((chanid_t));
int iof_posa _P_((chanid_t,timeout_t,unsigned long *));
int iof_posr _P_((chanid_t,timeout_t,long *));
int iof_rhdr _P_((chanid_t,timeout_t,void *,short));
int iof_rnam _P_((const void *,const void *));
int iof_save _P_((chanid_t,void *,unsigned long));
int iof_shdr _P_((chanid_t,timeout_t,void *,short));
int iof_trnc _P_((chanid_t,timeout_t));
int iof_vers _P_((chanid_t,timeout_t,long *));
int iof_xinf _P_((chanid_t,timeout_t,struct ext_mdinf *));
#define iow_blok _iow_blok
#define iow_chrq _iow_chrq
#define iow_clra _iow_clra
#define iow_clrb _iow_clrb
#define iow_clrl _iow_clrl
#define iow_clrr _iow_clrr
#define iow_clrt _iow_clrt
#define iow_dcur _iow_dcur
#define iow_defb _iow_defb
#define iow_defw _iow_defw
#define iow_donl _iow_donl
#define iow_ecur _iow_ecur
#define iow_font _iow_font
#define iow_font_def _iow_font_def
#define iow_ncol _iow_ncol
#define iow_newl _iow_newl
#define iow_nrow _iow_nrow
#define iow_pana _iow_pana
#define iow_panl _iow_panl
#define iow_panr _iow_panr
#define iow_pcol _iow_pcol
#define iow_pixq _iow_pixq
#define iow_prow _iow_prow
#define iow_rclr _iow_rclr
#define iow_scol _iow_scol
#define iow_scra _iow_scra
#define iow_scrb _iow_scrb
#define iow_scrt _iow_scrt
#define iow_scur _iow_scur
#define iow_sfla _iow_sfla
#define iow_sink _iow_sink
#define iow_sova _iow_sova
#define iow_spap _iow_spap
#define iow_spix _iow_spix
#define iow_ssiz _iow_ssiz
#define iow_sstr _iow_sstr
#define iow_sula _iow_sula
#define iow_xtop _iow_xtop
int iow_blok _P_((chanid_t,timeout_t,colour_t,QLRECT_t *));
int iow_chrq _P_((chanid_t,timeout_t,QLRECT_t *));
int iow_clra _P_((chanid_t,timeout_t));
int iow_clrb _P_((chanid_t,timeout_t));
int iow_clrl _P_((chanid_t,timeout_t));
int iow_clrr _P_((chanid_t,timeout_t));
int iow_clrt _P_((chanid_t,timeout_t));
int iow_dcur _P_((chanid_t,timeout_t));
int iow_defb _P_((chanid_t,timeout_t,colour_t,short));
int iow_defw _P_((chanid_t,timeout_t,colour_t,short,QLRECT_t *));
int iow_donl _P_((chanid_t,timeout_t));
int iow_ecur _P_((chanid_t,timeout_t));
int iow_font _P_((chanid_t,timeout_t,void *,void *));
int iow_font_def _P_((chanid_t,timeout_t,void *,void *));
int iow_ncol _P_((chanid_t,timeout_t));
int iow_newl _P_((chanid_t,timeout_t));
int iow_nrow _P_((chanid_t,timeout_t));
int iow_pana _P_((chanid_t,timeout_t,short));
int iow_panl _P_((chanid_t,timeout_t,short));
int iow_panr _P_((chanid_t,timeout_t,short));
int iow_pcol _P_((chanid_t,timeout_t));
int iow_pixq _P_((chanid_t,timeout_t,QLRECT_t *));
int iow_prow _P_((chanid_t,timeout_t));
int iow_rclr _P_((chanid_t,timeout_t,char *));
int iow_scol _P_((chanid_t,timeout_t,short));
int iow_scra _P_((chanid_t,timeout_t,short));
int iow_scrb _P_((chanid_t,timeout_t,short));
int iow_scrt _P_((chanid_t,timeout_t,short));
int iow_scur _P_((chanid_t,timeout_t,short,short));
int iow_sfla _P_((chanid_t,timeout_t,char));
int iow_sink _P_((chanid_t,timeout_t,colour_t));
int iow_sova _P_((chanid_t,timeout_t,short));
int iow_spap _P_((chanid_t,timeout_t,colour_t));
int iow_spix _P_((chanid_t,timeout_t,short,short));
int iow_ssiz _P_((chanid_t,timeout_t,short,short));
int iow_sstr _P_((chanid_t,timeout_t,colour_t));
int iow_sula _P_((chanid_t,timeout_t,char));
int iow_xtop _P_((chanid_t,timeout_t,int (*)(void),long,long,void *));
#define iog_arc _iog_arc
#define iog_arc_i _iog_arc_i
#define iog_dot _iog_dot
#define iog_dot_i _iog_dot_i
#define iog_elip _iog_elip
#define iog_elip_i _iog_elip_i
#define iog_fill _iog_fill
#define iog_line _iog_line
#define iog_line_i _iog_line_i
#define iog_scal _iog_scal
#define iog_scal_i _iog_scal_i
#define iog_sgcr _iog_sgcr
#define iog_sgu_i _iog_sgcr_i
int iog_arc _P_((chanid_t,timeout_t,double,double,double,double,double));
int iog_arc_i _P_((chanid_t,timeout_t,long,long,long,long,long));
int iog_dot _P_((chanid_t,timeout_t,double,double));
int iog_dot_i _P_((chanid_t,timeout_t,long,long));
int iog_elip _P_((chanid_t,timeout_t,double,double,double,double,double));
int iog_elip_i _P_((chanid_t,timeout_t,long,long,long,long,long));
int iog_fill _P_((chanid_t,timeout_t,long));
int iog_line _P_((chanid_t,timeout_t,double,double,double,double) );
int iog_line_i _P_((chanid_t,timeout_t,long,long,long,long));
int iog_scal _P_((chanid_t,timeout_t,double,double,double));
int iog_scal_i _P_((chanid_t,timeout_t,long,long,long));
int iog_sgcr _P_((chanid_t,timeout_t,double,double,double,double));
int iog_sgu_i _P_((chanid_t,timeout_t,long,long,long,long));
#define cv_datil _cv_datil
#define cv_fpdec _cv_fpdec
#define cv_ibbinb _cv_ibbinb
#define cv_ibhex _cv_ibhex
#define cv_ilbin _cv_ilbin
#define cv_ildat _cv_ildat
#define cv_ilday _cv_ilday
#define cv_ilhex _cv_ilhex
#define cv_iwbin _cv_iwbin
#define cv_iwdec _cv_iwdec
#define cv_iwhex _cv_iwhex
#define ioq_qbyt _ioq_qvyt
#define ioq_pbyt _ioq_pbyt
#define ioq_seof _ioq_seof
#define ioq_setq _ioq_setq
#define ioq_test _ioq_test
#define iou_dnam _iou_dnam
#define iou_ssio _iou_ssio
#define iou_ssq _iou_ssq
#define mem_achp _mem_achp
#define mem_alhp _mem_alhp
#define mem_llst _mem_list
#define mem_rchp _mem_rchp
#define mem_recp _mem_recp
#define mem_rlst _mem_rlst
#define opw_con _opw_con
#define opw_scr _opw_scr
#define opw_wind _opw_wind
#define ut_wtext _ut_wtext
#define ut_cstr _ut_cstr
#define ut_werms _ut_werms
#define ut_wersy _ut_wersy
#define ut_wint _ut_wint
long cv_datil _P_((short *));
void cv_ibbinb _P_((char *,char *));
void cv_ibhex _P_((char *,char *));
void cv_ilbin _P_((char *,long *));
char * cv_ildat _P_((char *,long));
char * cv_ilday _P_((char *,long));
void cv_ilhex _P_((char *,long *));
void cv_iwbin _P_((char *,short *));
int cv_iwdec _P_((char *,short *));
void cv_iwhex _P_((char *,short *));
int ioq_qbyt _P_((char *,int));
int ioq_pbyt _P_((char *,char *));
int ioq_seof _P_((char *));
void ioq_setq _P_((char *,long));
int ioq_test _P_((char *,char *,long *));
int iou_ssio _P_((chanid_t,timeout_t,int,long *,long *,char **,int ** ));
int iou_ssq _P_((chanid_t,timeout_t,int,long *,long *,char **));
char * mem_achp _P_((long *));
char * mem_alhp _P_((char **,long *));
void mem_llst _P_((void *,void *));
void mem_rchp _P_((char *));
void mem_recp _P_((char *,char **,long));
void mem_rlst _P_((void *,void *));
chanid_t opw_con _P_((WINDOWDEF_t *));
chanid_t opw_scr _P_((WINDOWDEF_t *));
chanid_t opw_wind _P_((const char *,char *));
int ut_wtext _P_((chanid_t,QLSTR_t *));
int ut_cstr _P_((const QLSTR_t *,const QLSTR_t *,int));
void ut_werms _P_((long,chanid_t));
void ut_wersy _P_((long));
int ut_wint _P_((chanid_t,int));
#undef _P_
#endif
