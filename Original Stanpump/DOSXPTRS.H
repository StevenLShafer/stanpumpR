#ifndef _DOSXPTRS_DOT_H
#define _DOSXPTRS_DOT_H

/* The following section helps us with far pointers for the 32-bit
 * DOS extenders.  Some of the compilers do not support far pointers,
 * so we set up a generic far pointer that all compilers can
 * use.
 */

#if defined( DOSX386 )

#if defined( GF_WATCOM_C_386 )
/*
 * Pharlap and Watcom are both trying to define FP_OFF.  I get a redef
 * error when I include DOS.H *after* pharlap.h.  So I do a preemptive
 * include here to avoid that problem.
 */
#include <dos.h>
#if !defined( _M_IX86 )
#define _M_IX86 0
#endif
#endif

#include <pharlap.h>

#define GF_FARPTR       FARPTR
#define GF_FP_OFF       FP_OFF
#define GF_FP_SEL       FP_SEL

#define GF_REALPTR      REALPTR

extern unsigned short _flat_code_sel;
extern unsigned short _flat_data_sel;
extern unsigned short _flat_dos_sel;
extern unsigned short _flat_vid_sel;
void GF_CONV _SetTNTConfigInfo( void );

#else

#define GF_FARPTR       void GF_FAR *
#define GF_REALPTR      unsigned long

#define GF_MK_FP        MK_FP

#endif

#endif
