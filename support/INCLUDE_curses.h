#ifndef _CURSES
#define _CURSES
#include <stdio.h>
#include <stdarg.h>
#include <unctrl.h>
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define OK (0)
#define ERR (-1)
typedef unsigned char bool;
#undef TRUE
#define TRUE ((bool)1)
#undef FALSE
#define FALSE ((bool)0)
#ifdef SMALL_CURSES
typedef unsigned short chtype;
#define A_STANDOUT (0x0100)
#define A_UNDERLINE (0x0200)
#define A_REVERSE (0x0400)
#define A_BLINK (0x0800)
#define A_DIM (0x1000)
#define A_BOLD (0x2000)
#define A_INVIS (0x4000)
#define A_PROTECT (0x8000)
#define A_CHARTEXT (0x00ff)
#define A_ATTRIBUTES (0xff00)
#define A_NORMAL (0x0000)
#else
typedef unsigned long chtype;
#define A_STANDOUT (0x00010000L)
#define A_UNDERLINE (0x00020000L)
#define A_REVERSE (0x00040000L)
#define A_BLINK (0x00080000L)
#define A_DIM (0x00100000L)
#define A_BOLD (0x00200000L)
#define A_INVIS (0x00400000L)
#define A_PROTECT (0x00800000L)
#define A_ALTCHARSET (0x01000000L)
#define COLOR_PAIR(c) (((chtype)(c))<<25)
#define A_CHARTEXT (0x0000ffffL)
#define A_ATTRIBUTES (0xffff0000L)
#define A_NORMAL (0x00000000L)
#define A_COLOR (0xfe000000L)
#define PAIR_NUMBER(a) ((a)>>25)
#define COLOR_BLACK 0
#define COLOR_RED 1
#define COLOR_MAGENTA 2
#define COLOR_YELLOW 3
#define COLOR_BLUE 4
#define COLOR_GREEN 5
#define COLOR_CYAN 6
#define COLOR_WHITE 7
#define ACS_ULCORNER (_acs_ch['l'])
#define ACS_LLCORNER (_acs_ch['m'])
#define ACS_URCORNER (_acs_ch['k'])
#define ACS_LRCORNER (_acs_ch['j'])
#define ACS_RTEE (_acs_ch['u'])
#define ACS_LTEE (_acs_ch['t'])
#define ACS_TTEE (_acs_ch['w'])
#define ACS_BTEE (_acs_ch['v'])
#define ACS_HLINE (_acs_ch['q'])
#define ACS_VLINE (_acs_ch['x'])
#define ACS_PLUS (_acs_ch['n'])
#define ACS_S1 (_acs_ch['o'])
#define ACS_S9 (_acs_ch['s'])
#define ACS_DIAMOND (_acs_ch['\''])
#define ACS_CKBOARD (_acs_ch['a'])
#define ACS_DEGREE (_acs_ch['f'])
#define ACS_PLMINUS (_acs_ch['g'])
#define ACS_BULLET (_acs_ch['b'])
#define ACS_LARROW (_acs_ch[','])
#define ACS_RARROW (_acs_ch['+'])
#define ACS_DARROW (_acs_ch['.'])
#define ACS_UARROW (_acs_ch['-'])
#define ACS_BOARD (_acs_ch['h'])
#define ACS_LANTERN (_acs_ch['l'])
#define ACS_BLOCK (_acs_ch['0'])
#endif
#define KEY_BREAK 0401
#define KEY_DOWN 0402
#define KEY_UP 0403
#define KEY_LEFT 0404
#define KEY_RIGHT 0405
#define KEY_HOME 0406
#define KEY_BACKSPACE 0407
#define KEY_F0 0410
#define KEY_F(n) (KEY_F0+(n))
#define KEY_DL 0510
#define KEY_IL 0511
#define KEY_DC 0512
#define KEY_IC 0513
#define KEY_EIC 0514
#define KEY_CLEAR 0515
#define KEY_EOS 0516
#define KEY_EOL 0517
#define KEY_SF 0520
#define KEY_SR 0521
#define KEY_NPAGE 0522
#define KEY_PPAGE 0523
#define KEY_STAB 0524
#define KEY_CTAB 0525
#define KEY_CATAB 0526
#define KEY_ENTER 0527
#define KEY_SRESET 0530
#define KEY_RESET 0531
#define KEY_PRINT 0532
#define KEY_LL 0533
#define KEY_A1 0534
#define KEY_A3 0535
#define KEY_B2 0536
#define KEY_C1 0537
#define KEY_C3 0540
#define KEY_BTAB 0541
#define KEY_BEG 0542
#define KEY_CANCEL 0543
#define KEY_CLOSE 0544
#define KEY_COMMAND 0545
#define KEY_COPY 0546
#define KEY_CREATE 0547
#define KEY_END 0550
#define KEY_EXIT 0551
#define KEY_FIND 0552
#define KEY_HELP 0553
#define KEY_MARK 0554
#define KEY_MESSAGE 0555
#define KEY_MOVE 0556
#define KEY_NEXT 0557
#define KEY_OPEN 0560
#define KEY_OPTIONS 0561
#define KEY_PREVIOUS 0562
#define KEY_REDO 0563
#define KEY_REFERENCE 0564
#define KEY_REFRESH 0565
#define KEY_REPLACE 0566
#define KEY_RESTART 0567
#define KEY_RESUME 0570
#define KEY_SAVE 0571
#define KEY_SBEG 0572
#define KEY_SCANCEL 0573
#define KEY_SCOMMAND 0574
#define KEY_SCOPY 0575
#define KEY_SCREATE 0576
#define KEY_SDC 0577
#define KEY_SDL 0600
#define KEY_SELECT 0601
#define KEY_SEND 0602
#define KEY_SEOL 0603
#define KEY_SEXIT 0604
#define KEY_SFIND 0605
#define KEY_SHELP 0606
#define KEY_SHOME 0607
#define KEY_SIC 0610
#define KEY_SLEFT 0611
#define KEY_SMESSAGE 0612
#define KEY_SMOVE 0613
#define KEY_SNEXT 0614
#define KEY_SOPTIONS 0615
#define KEY_SPREVIOUS 0616
#define KEY_SPRINT 0617
#define KEY_SREDO 0620
#define KEY_SREPLACE 0621
#define KEY_SRIGHT 0622
#define KEY_SRESUME 0623
#define KEY_SSAVE 0624
#define KEY_SSUPEND 0625
#define KEY_SUNDO 0626
#define KEY_SUSPEND 0627
#define KEY_UNDO 0630
#define KEY_MOUSE 0631
#define KEY_MIN KEY_BREAK
#define KEY_MAX 0640
#define MOUSE_X_POS (Mouse_status.x)
#define MOUSE_Y_POS (Mouse_status.y)
#define A_BUTTON_CHANGED
#define MOUSE_MOVED
#define MOUSE_POS_REPORT
#define BUTTON_CHANGED(x)
#define BUTTON_STATUS(x)
#define BUTTON1_RELEASED 0x0001L
#define BUTTON1_PRESSED 0x0002L
#define BUTTON1_CLICKED 0x0004L
#define BUTTON1_DOUBLE_CLICKED 0x0008L
#define BUTTON1_TRIPPLE_CLICKED 0x0010L
#define BUTTON2_RELEASED 0x0020L
#define BUTTON2_PRESSED 0x0040L
#define BUTTON2_CLICKED 0x0080L
#define BUTTON2_DOUBLE_CLICKED 0x0100L
#define BUTTON2_TRIPPLE_CLICKED 0x0200L
#define BUTTON3_RELEASED 0x0400L
#define BUTTON3_PRESSED 0x0800L
#define BUTTON3_CLICKED 0x1000L
#define BUTTON3_DOUBLE_CLICKED 0x2000L
#define BUTTON3_TRIPPLE_CLICKED 0x4000L
#define BUTTON_RELEASED 0
#define BUTTON_PRESSED 1
#define BUTTON_CLICKED 2
#define BUTTON_DOUBLE_CLICKED 3
#define BUTTON_TRIPPLE_CLICKED 4
#define ALL_MOUSE_EVENTS 0x7fffL
#define REPORT_MOUSE_POSITION
typedef struct _mouse {
int x;
int y;
unsigned buttons[3];
unsigned events;
} MOUSE_STATUS;
typedef struct _scrn SCREEN;
typedef struct _window WINDOW;
#define crmode() cbreak()
#define nocrmode() nocbreak()
#define garbagedlines(w,b,n) touchline((w),(b),(n))
#define getmode()
#define fixterm() reset_prog_mode()
#define resetterm() reset_shell_mode()
#define saveterm() def_prog_mode()
#define setterm(term) setupterm((term),1,(int*)0)
#ifndef SMALL_CURSES
extern int COLORS;
extern int COLOR_PAIRS;
#endif
extern int COLS;
extern int LINES;
extern WINDOW *curscr;
extern WINDOW *stdscr;
extern MOUSE_STATUS Mouse_status;
#ifndef SMALL_CURSES
extern chtype * _acs_ch;
#endif
extern int addch _P_((chtype ch));
extern int echochar _P_((chtype ch));
extern int mvaddch _P_((int y,int x,chtype c));
extern int mvwaddch _P_((WINDOW *win,int y,int x,chtype c));
extern int waddch _P_((WINDOW *win,chtype ch));
extern int wechochar _P_((WINDOW *win,chtype ch));
extern int addchnstr _P_((chtype *chstr,int n));
extern int addchstr _P_((chtype *chstr));
extern int waddchnstr _P_((WINDOW *win,chtype *chstr,int n));
extern int waddchstr _P_((WINDOW *win,chtype *chstr));
extern int mvaddchnstr _P_((int y,int x,chtype *chstr,int n));
extern int mvaddchstr _P_((int y,int x,chtype *chstr));
extern int mvwaddchnstr _P_((WINDOW *win,int y,int x,chtype *chstr,int n));
extern int mvwaddchstr _P_((WINDOW *win,int y,int x,chtype *chstr));
extern int addnstr _P_((char *str,int n));
extern int addstr _P_((char *str));
extern int waddnstr _P_((WINDOW *win,char *str,int n));
extern int waddstr _P_((WINDOW *win,char *str));
extern int mvaddnstr _P_((int y,int x,char *s,int n));
extern int mvaddstr _P_((int y,int x,char *s));
extern int mvwaddnstr _P_((WINDOW *win,int y,int x,char *s,int n));
extern int mvwaddstr _P_((WINDOW *win,int y,int x,char *s));
extern int attroff _P_((chtype a));
extern int attron _P_((chtype a));
extern int attrset _P_((chtype a));
extern int wattroff _P_((WINDOW *win,chtype attr));
extern int wattron _P_((WINDOW *win,chtype attr));
extern int wattrset _P_((WINDOW *win,chtype attr));
extern int standend _P_((void));
extern int wstandend _P_((WINDOW *win));
extern int standout _P_((void));
extern int wstandout _P_((WINDOW *win));
#ifndef _BEEP
extern int beep _P_((void));
#define _BEEP
#endif
extern int flash _P_((void));
extern int bkgd _P_((chtype ch));
extern void bkgdset _P_((chtype ch));
extern int wbkgd _P_((WINDOW *win,chtype ch));
extern void wbkgdset _P_((WINDOW *win,chtype ch));
extern int border _P_((chtype ls,chtype rs,chtype ts,chtype bs,chtype tl,chtype tr,chtype bl,chtype br));
extern int wborder _P_((WINDOW *win,chtype ls,chtype rs,chtype ts,chtype bs,chtype tl,chtype tr,chtype bl,chtype br));
extern int box _P_((WINDOW *win,chtype v,chtype h));
extern int hline _P_((chtype ch,int n));
extern int whline _P_((WINDOW *win,chtype ch,int n));
extern int vline _P_((chtype ch,int n));
extern int wvline _P_((WINDOW *win,chtype ch,int n));
extern int erase _P_((void));
extern int werase _P_((WINDOW *win));
extern int clear _P_((void));
extern int wclear _P_((WINDOW *win));
extern int clrtobot _P_((void));
extern int wclrtobot _P_((WINDOW *win));
extern int clrtoeol _P_((void));
extern int wclrtoeol _P_((WINDOW *win));
#ifndef SMALL_CURSES
extern bool can_change_color _P_((void));
extern bool has_colors _P_((void));
extern int color_content _P_((int color,short * red,short * greeen,short * blue));
extern int init_color _P_((int color,short red,short green,short blue));
extern int init_pair _P_((int pair,short foreground,short background));
extern int pair_content _P_((int pair,short *fore,short *back));
extern int start_color _P_((void));
#endif
extern int delch _P_((void));
extern int wdelch _P_((WINDOW *win));
extern int mvdelch _P_((int y,int x));
extern int mvwdelch _P_((WINDOW *win,int y,int x));
extern int deleteln _P_((void));
extern int wdeleteln _P_((WINDOW *win));
extern int insdelln _P_((int n));
extern int winsdelln _P_((WINDOW *win,int n));
extern int insertln _P_((void));
extern int winsertln _P_((WINDOW *win));
extern int getch _P_((void));
extern int wgetch _P_((WINDOW *win));
extern int mvgetch _P_((int y,int x));
extern int mvwgetch _P_((WINDOW *win,int y,int x));
extern int ungetch _P_((int ch));
extern int getstr _P_((char *str));
extern int wgetstr _P_((WINDOW *win,char *str));
extern int mvgetstr _P_((int y,int x,char *str));
extern int mvwgetstr _P_((WINDOW *win,int y,int x,char *str));
extern int wgetnstr _P_((WINDOW *win,char *str,int n));
#define getyx(w,y,x) ((y)=_gety(w),(x)=_getx(w))
extern int _gety _P_((WINDOW*)),_getx _P_((WINDOW*));
#define getparyx(w,y,x) ((y)=_getpy(w),(x)=_getpx(w))
extern int _getpy _P_((WINDOW*)),_getpx _P_((WINDOW*));
#define getbegyx(w,y,x) ((y)=_getby(w),(x)=_getbx(w))
extern int _getby _P_((WINDOW*)),_getbx _P_((WINDOW*));
#define getmaxyx(w,y,x) ((y)=_getmy(w),(x)=_getmx(w))
extern int _getmy _P_((WINDOW*)),_getmx _P_((WINDOW*));
extern chtype inch _P_((void));
extern chtype winch _P_((WINDOW *win));
extern chtype mvinch _P_((int y,int x));
extern chtype mvwinch _P_((WINDOW *win,int y,int x));
extern int inchstr _P_((chtype *str));
extern int inchnstr _P_((chtype *str,int n));
extern int winchstr _P_((WINDOW *win,chtype *str));
extern int winchnstr _P_((WINDOW *win,chtype *str,int n));
extern int mvinchstr _P_((int y,int x,chtype *str));
extern int mvinchnstr _P_((int y,int x,chtype *str,int n));
extern int mvwinchstr _P_((WINDOW *win,int y,int x,chtype *str));
extern int mvwinchnstr _P_((WINDOW *win,int y,int x,chtype *str,int n));
extern void delscreen _P_((SCREEN *sp));
extern int endwin _P_((void));
extern WINDOW *initscr _P_((void));
extern int isendwin _P_((void));
extern SCREEN *newterm _P_((char *type,FILE * outfd,FILE *infd));
extern SCREEN *set_term _P_((SCREEN *new));
extern int cbreak _P_((void));
extern int nocbreak _P_((void));
extern int echo _P_((void));
extern int noecho _P_((void));
extern int halfdelay _P_((int tenths));
extern int intrflush _P_((WINDOW *win,bool bf));
extern int keypad _P_((WINDOW *win,bool bf));
extern int meta _P_((WINDOW *win,bool bf));
extern int nodelay _P_((WINDOW *win,bool bf));
extern int notimeout _P_((WINDOW *win,bool bf));
extern int raw _P_((void));
extern int noraw _P_((void));
extern void noqiflush _P_((void));
extern void qiflush _P_((void));
extern void timeout _P_((int delay));
extern void wtimeout _P_((WINDOW *win,int delay));
extern int typeahead _P_((int fd));
extern int insch _P_((chtype ch));
extern int winsch _P_((WINDOW *win,chtype ch));
extern int mvinsch _P_((int y,int x,chtype ch));
extern int mvwinsch _P_((WINDOW *win,int y,int x,chtype ch));
extern int instr _P_((char *str));
extern int winstr _P_((WINDOW *win,char *str));
extern int mvinstr _P_((int y,int x,char *str));
extern int mvwinstr _P_((WINDOW *win,int y,int x,char *str));
extern int innstr _P_((char *str,int n));
extern int winnstr _P_((WINDOW *win,char *str,int n));
extern int mvinnstr _P_((int y,int x,char *str,int n));
extern int mvwinnstr _P_((WINDOW *win,int y,int x,char *str,int n));
extern int insstr _P_((char *str));
extern int winsstr _P_((WINDOW *win,char *str));
extern int mvinsstr _P_((int y,int x,char *str));
extern int mvwinsstr _P_((WINDOW *win,int y,int x,char *str));
extern int insnstr _P_((char *str,int n));
extern int winsnstr _P_((WINDOW *win,char *str,int n));
extern int mvinsnstr _P_((int y,int x,char *str,int n));
extern int mvwinsnstr _P_((WINDOW *win,int y,int x,char *str,int n));
extern int def_prog_mode _P_((void));
extern int def_shell_mode _P_((void));
extern int reset_prog_mode _P_((void));
extern int reset_shell_mode _P_((void));
extern int resetty _P_((void));
extern int savetty _P_((void));
#define getsyx(y,x) ((y) = _getsy(),(x) = _getsx())
extern int _getsy _P_((void)),_getsx _P_((void));
extern int setsyx _P_((int y,int x));
extern int ripoffline _P_((int n,int (*init)(WINDOW*,int)));
extern int curs_set _P_((int visibility));
extern int napms _P_((int ms));
extern int move _P_((int y,int x));
extern int wmove _P_((WINDOW *win,int y,int x));
extern int clearok _P_((WINDOW *win,bool bf));
extern int idlok _P_((WINDOW *win,bool bf));
extern void idcok _P_((WINDOW *win,bool bf));
extern void immedok _P_((WINDOW *win,bool bf));
extern int leaveok _P_((WINDOW *win,bool bf));
extern int setscrreg _P_((int top,int bot));
extern int wsetscrreg _P_((WINDOW *win,int top,int bot));
extern int scrollok _P_((WINDOW *win,bool bf));
extern int nl _P_((void));
extern int nonl _P_((void));
extern int overlay _P_((WINDOW *srcwin,WINDOW *dstwin));
extern int overwrite _P_((WINDOW *srcwin,WINDOW *dstwin));
extern int copywin _P_((WINDOW * srcwin,WINDOW * dstwin,int sminrow,int smincol,int dminrow,int dmincol,int dmaxrow,int dmaxcol,int ovrlay));
extern WINDOW *newpad _P_((int nlines,int ncols));
extern WINDOW *subpad _P_((WINDOW *orig,int nlines,int ncols,int begy,int begx));
extern int prefresh _P_((WINDOW *pad,int pminrow,int pmincol,int sminrow,int smincol,int smaxrow,int smaxcol));
extern int pnoutrefresh _P_((WINDOW *pad,int pminrow,int pmincol,int sminrow,int smincol,int smaxrow,int smaxcol));
extern int pechochar _P_((WINDOW *pad,chtype ch));
extern int printw _P_((char *fmt,...));
extern int wprintw _P_((WINDOW *win,char *fmt,...));
extern int mvprintw _P_((int y,int x,char *fmt,...));
extern int mvwprintw _P_((WINDOW *win,int y,int x,char *fmt,...));
extern int vwprintw _P_((WINDOW *win,char *fmt,va_list));
extern int touchwin _P_((WINDOW *win));
extern int touchline _P_((WINDOW *win,int y,int n));
extern int untouchwin _P_((WINDOW *win));
extern int wtouchln _P_((WINDOW *win,int y,int n,int changed));
extern int is_linetouched _P_((WINDOW *win,int line));
extern int is_wintouched _P_((WINDOW *win));
extern int refresh _P_((void));
extern int wrefresh _P_((WINDOW *win));
extern int wnoutrefresh _P_((WINDOW *win));
extern int doupdate _P_((void));
extern int redrawwin _P_((WINDOW *win));
extern int wredrawln _P_((WINDOW *win,int beg_line,int num_lines));
extern int scanw _P_((char *fmt,...));
extern int wscanw _P_((WINDOW *win,char *fmt,...));
extern int mvscanw _P_((int y,int x,char *fmt,...));
extern int mvwscanw _P_((WINDOW *win,int y,int x,char *fmt,...));
extern int vwscanw _P_((WINDOW *win,char *fmt,va_list));
extern int scroll _P_((WINDOW *win));
extern int scrl _P_((int n));
extern int wscrl _P_((WINDOW *win,int n));
extern int scr_dump _P_((char *filename));
extern int scr_restore _P_((char *filename));
extern int scr_init _P_((char *filename));
extern int scr_set _P_((char *filename));
extern int slk_init _P_((int fmt));
extern int slk_set _P_((int labnum,char *label,int labfmt));
extern int slk_refresh _P_((void));
extern int slk_noutrefresh _P_((void));
extern char *slk_label _P_((int labnum));
extern int slk_clear _P_((void));
extern int slk_restore _P_((void));
extern int slk_touch _P_((void));
extern int slk_attron _P_((chtype attrs));
extern int slk_attroff _P_((chtype attrs));
extern int slk_attrset _P_((chtype attrs));
extern int baudrate _P_((void));
extern char erasechar _P_((void));
extern int has_ic _P_((void));
extern int has_il _P_((void));
extern char killchar _P_((void));
extern char *longname _P_((void));
extern chtype termattrs _P_((void));
extern char *termname _P_((void));
extern char *keyname _P_((int c));
extern int filter _P_((void));
extern void use_env _P_((bool bf));
extern int putwin _P_((WINDOW *win,FILE *filep));
extern WINDOW * getwin _P_((FILE * filep));
extern delay_output _P_((int ms));
extern int flushinp _P_((void));
extern WINDOW *newwin _P_((int nlines,int ncols,int begy,int begx));
extern int delwin _P_((WINDOW *win));
extern int mvwin _P_((WINDOW *win,int y,int x));
extern WINDOW *subwin _P_((WINDOW *orig,int nlines,int ncols,int begy,int begx));
extern WINDOW *derwin _P_((WINDOW *win,int rows,int cols,int ypos,int xpos));
extern int mvderwin _P_((WINDOW *win,int par_y,int par_x));
extern WINDOW *dupwin _P_((WINDOW *win));
extern void wsyncup _P_((WINDOW *));
extern int syncok _P_((WINDOW *win,bool bf));
extern void wcursyncup _P_((WINDOW *win));
extern void wsyncdown _P_((WINDOW *));
extern int mouse_on _P_((long status));
extern int mouse_set _P_((long status));
extern int mouse_off _P_((long status));
extern int map_button _P_((unsigned long button));
extern unsigned long getbmap _P_((void));
extern unsigned long getmouse _P_((void));
extern int request_mouse_pos _P_((void));
extern void wmouse_position _P_((WINDOW *win,int *y,int *x));
#undef _P_
#endif
