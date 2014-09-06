
#ifndef _TERMIOS_H
#define _TERMIOS_H
typedef unsigned short tcflag_t;
typedef unsigned char cc_t;
typedef unsigned int speed_t;
#define NCCS 11
struct termios {
tcflag_t c_iflag;
tcflag_t c_oflag;
tcflag_t c_cflag;
tcflag_t c_lflag;
speed_t c_ispeed;
speed_t c_ospeed;
cc_t c_cc[NCCS];
};
#define BRKINT 000001
#define ICRNL 000002
#define IGNBRK 000004
#define IGNCR 000010
#define IGNPAR 000020
#define INLCR 000100
#define INPCK 000200
#define ISTRIP 000400
#define IXOFF 001000
#define IXON 002000
#define PARMRK 004000
#define OPOST 000001
#define CLOCAL 000001
#define CREAD 000002
#define CSIZE 000014
#define CSTOPB 000020
#define HUPCL 000040
#define PARENB 000100
#define PARODD 000200
#define CS5 000000
#define CS6 000004
#define CS7 000010
#define CS8 000014
#define ECHO 000001
#define ECHOE 000002
#define ECHOK 000004
#define ECHONL 000010
#define ICANON 000020
#define IEXTEN 000040
#define ISIG 000100
#define NOFLSH 000200
#define TOSTOP 000400
#define VEOF 0
#define VEOL 1
#define VERASE 2
#define VINTR 3
#define VKILL 4
#define VMIN 5
#define VQUIT 6
#define VTIME 7
#define VSUSP 8
#define VSTART 9
#define VSTOP 10
#define B0 0000000
#define B50 0010000
#define B75 0020000
#define B110 0030000
#define B134 0040000
#define B150 0050000
#define B200 0060000
#define B300 0070000
#define B600 0100000
#define B1200 0110000
#define B1800 0120000
#define B2400 0130000
#define B4800 0140000
#define B9600 0150000
#define B19200 0160000
#define B38400 0170000
#define B57600 0200000
#define B115200 0210000
#define TCSANOW 1
#define TCSADRAIN 2
#define TCSAFLUSH 3
#define TCIFLUSH 1
#define TCOFLUSH 2
#define TCIOFLUSH 3
#define TCOOFF 1
#define TCOON 2
#define TCIOFF 3
#define TCION 4
#ifdef __STDC__
#define _PROTOTYPE_(func,params) func params
#else
#define _PROTOTYPE_(func,params) func()
#endif
_PROTOTYPE_( int tcsendbreak,(int _fildes,int _duration) );
_PROTOTYPE_( int tcdrain,(int _filedes) );
_PROTOTYPE_( int tcflush,(int _filedes,int _queue_selector) );
_PROTOTYPE_( int tcflow,(int _filedes,int _action) );
_PROTOTYPE_( speed_t cfgetospeed,(struct termios *_termios_p) );
_PROTOTYPE_( int cfsetospeed,(struct termios *_termios_p,speed_t _speed));
_PROTOTYPE_( speed_t cfgetispeed,(struct termios *_termios_p) );
_PROTOTYPE_( int cfsetispeed,(struct termios *_termios_p,speed_t _speed));
_PROTOTYPE_( int tcgetattr,(int _filedes,struct termios *_termios_p) );
_PROTOTYPE_( int tcsetattr,(int _filedes,int _opt_actions,struct termios *_termios_p) );
#undef _PROTOTYPE_
#endif
