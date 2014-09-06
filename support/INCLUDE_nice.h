#ifndef _NICE_H
#define _NICE_H
#define AND &&
#define OR ||
#define NOT !
#define EQ ==
#define NE !=
#define LT <
#define LE <=
#define GT >
#define GE >=
#define BAND &
#define BOR |
#define BXOR ^
#define BNOT ~
#define LSHF <<
#define RSHF >>
#define INC ++
#define DEC --
#define MOD %
#define PNTR *
#define CONT *
#define ADDR(c) &c
#define BEGIN {
#define END }
#define ENDDEF }
#define IF(c) { if(c)
#define THEN {
#define ELSE ;} else {
#define ENDIF ;}}
#define ELSEIF(c) ;} else if(c) {
#define CASE(c) { switch(c) {
#define CASEOF(c) case c : {
#define DEFAULT default : {
#define ENDCOF } break
#define ENDCASE }}
#define SELECT(c) { switch(c) {
#define REMAINDER default : {
#define ENDSELECT }}
#define FOR(c) { for(c) {
#define ENDFOR ;}}
#define WHILE(c) { while(c) {
#define ENDWHILE ;}}
#define REPEAT { do {
#define UNTIL(c) } while(!(c)); }
#define REPEAT_WHILE(c) { while(c) {
#define REPEAT_FOREVER { while (1) {
#define ENDREPEAT ;}}
#endif
