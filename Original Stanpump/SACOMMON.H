/******************************************************************************\
*                                                                              *
*      SACOMMON.H     -     Common include file for SA changes in Greenleaf.   *
*                                                                              *
*      Copyright (c) Ring Zero Systems 1992-94                                  *
*                                                                              *
\******************************************************************************/

#ifndef _SACOMMON_H
#define _SACOMMON_H


extern int ( far *vgfd_entry_point )();

#define VGFD_Device_ID                  0x35

#define UNVIRT_IRQ  0
#define VIRT_IRQ    1
#define ENABLE_IRQ  2

/* defines for valloc() codes */
#define ALLOC_AS    1
#define ALLOC_PT    2
#define ALLOC_TX    3
#define ALLOC_RX    4

/* Defines for Isr codes */
#define GsciIsaIsr  1
#define GsciMcIsr   2
#define DigiIsaIsr  3
#define DigiMcaIsr  4
#define StargateIsr 5
#define AstIsr      6
#define ArnetIsr    7
#define ContecIsr   8

typedef void ( GF_CDECL GF_FAR *handler_function )( void GF_FAR *data );

extern int GF_CONV InitGreenleaf();
extern int Virtualize_IRQ( int offset, int irq, int mode );
extern int ResolveIrq( int irq );
extern int IrqInUseByOthers( int index, int mode_close );
extern int port_belongs_vm( int port );
extern int port_info_belongs_vm( struct PORTINFO GF_FAR *port_info );
extern int irq_belongs_vm( int index );
extern void far *valloc( int size, int number, int code, int port );
extern int vfree( void far * );
extern int Get_VM_ID(void);
extern void far *GetIrqTable( void );
extern void far *GetIsrData( int size, int irq, int first_port_number );
extern void far *GetIsrDataPointer( int size, int index, int first_port_number );
extern int SetIsrData( void far *p, int offset, int intrpt_num );
extern int SetIsrDataPointer( void far *p, int offset, int index );
extern int SetRealHandler( int handler, int index );
extern void EnableIrq( int port );
#if defined( VGFD_DLL )
struct PORTINFO GF_FAR * GF_CONV Get_as_chnl( void );
#endif


#ifdef GF_WINDOWS
/* extern void GF_CONV TWYield( TW_HEADER *tw_hdr );
*/
#endif


#endif
