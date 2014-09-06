
#ifndef _NL_TYPES_H
#define _NL_TYPES_H
#ifndef NL_SETMAX
#define NL_SETMAX 1024
#define NL_MSGMAX 32767
#define NL_TEXTMAX 1024
#endif
#define NL_MAXPATHLEN 34
#define NL_PATH"NLSPATH"
#define NL_LANG"LANG"
#define NL_DEF_LANG"english"
#define NL_SETD 1
#define NL_MAX_OPENED 10
struct cat_msg {
int msg_nr;
int msg_len;
long msg_off;
char *msg_ptr;
struct cat_msg *msg_next;
};
struct cat_set {
int set_nr;
int set_msg_nr;
struct cat_msg *set_msg;
struct cat_set *set_next;
};
struct m_cat_set {
int first_msg;
int last_msg;
};
struct set_info {
int no_sets;
struct m_cat_set sn[1];
};
#define CMD_SET"set"
#define CMD_SET_LEN 3
#define CMD_DELSET"delset"
#define CMD_DELSET_LEN 6
#define CMD_QUOTE"quote"
#define CMD_QUOTE_LEN 5
#define XOPEN_DIRECTORY"/usr/lib/locale/Xopen/LC_MESSAGES"
#define DFLT_MSG"\01"
#define M_EXTENSION".m"
#define DEF_NLSPATH"./%N"
struct cat_hdr {
long hdr_magic;
int hdr_set_nr;
int hdr_mem;
long hdr_off_msg_hdr;
long hdr_off_msg;
};
struct cat_set_hdr {
int shdr_set_nr;
int shdr_msg_nr;
int shdr_msg;
};
struct cat_msg_hdr{
int msg_nr;
int msg_len;
int msg_ptr;
};
#define CAT_MAGIC 0xFF88FF89
typedef int nl_item ;
typedef struct {
char type;
int set_nr;
union {
struct malloc_data {
struct cat_set_hdr *sets;
struct cat_msg_hdr *msgs;
char *data;
} m;
struct gettxt_data {
struct set_info *sets;
int size;
int fd;
char *link;
} g;
} info;
} nl_catd_t;
typedef nl_catd_t *nl_catd;
#define MKMSGS'M'
#define MALLOC'm'
#define BIN_MKMSGS"mkmsgs"
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
int catclose _P_((nl_catd));
char * catgets _P_((nl_catd,int,int,char *));
nl_catd catopen _P_((const char *,int));
char * gettxt _P_((const char *,const char *));
#undef _P_
#endif
