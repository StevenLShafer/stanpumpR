#include <stdio.h>
#include <dos.h>
#include <conio.h>
#include "stanpump.h"

#define BITSET(x, n) ( (((unsigned) x >> n) & 0x0001) == 1 ? 1 : 0 )

#define AND(a, b)    ( a &= b )  /* a = a AND b */

/*===========================================================================*/

#define INT09	0x0009	    /* Keyboard interrupt number	       */
#define INT1B	0x001B	    /* Ctrl-C interrupt number		       */
#define INT23	0x0023	    /* Ctrl-Break interrupt number	       */

#define CtrlOff 0xFB	    /* Ctrl-C bit mask			       */

#define KBDFLAG 0x00000417  /* Keyboard flag byte address	       */

#define KB_DATA 0x0060	    /* Kbd port address 		       */
#define ADDRESS unsigned far *

#pragma check_stack(off)

int keyboard_reset = 0;
extern int keyin();

void (interrupt far *KbdPtr)(void);	/* points to keyboard routine. */
void (interrupt far *BrkPtr)(void);	/* points to break routine.    */

void (interrupt far *OldInt09)();	/* save old kbd handler */
void (interrupt far *OldInt1B)();	/* save old ^C	handler */
void (interrupt far *OldInt23)();	/* save old brk handler */

ADDRESS KbdCtrl;

void interrupt far Int09(void);
void interrupt far Int1B(void);
void interrupt far Int23(void);

void set_keyboard()
	{
       OldInt09 = _dos_getvect( INT09 );
       OldInt1B = _dos_getvect( INT1B );
       OldInt23 = _dos_getvect( INT23 );

       KbdPtr = Int09;
       _dos_setvect( INT09, KbdPtr );

       BrkPtr = Int1B;
       _dos_setvect( INT1B, BrkPtr);

       BrkPtr = Int23;
       _dos_setvect( INT23, BrkPtr );

       KbdCtrl	= (ADDRESS) KBDFLAG;
       keyboard_reset = 1;
	}

void restore_keyboard()
	{
	if (keyboard_reset == 1)
		{
        _dos_setvect( INT09, OldInt09 );
   	    _dos_setvect( INT1B, OldInt1B );
   	    _dos_setvect( INT23, OldInt23 );
   	   }
	}

/*============================================================================*/
/* Interrupt Service Routines. */

void interrupt far
Int09( void )
{
     _disable();
     if ( BITSET(*KbdCtrl, 2))
     {
	  AND( *KbdCtrl, CtrlOff );
     }
     OldInt09();
}

void interrupt far
Int1B(void)
{
     /* New home for Ctrl-C.	 */
}

void interrupt far
Int23(void)
{
// home of control break?
}
