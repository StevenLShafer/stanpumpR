#ifndef COMMLIB_DOT_H
#define COMMLIB_DOT_H

/*
 * COMMLIB.H       5.00A August 8, 1994
 *
 * The Greenleaf Comm Library
 *
 * Copyright (C) 1985-1994 Greenleaf Software Inc.  All Rights Reserved.
 *
 * NOTES
 *
 *  This is the master include file for CommLib Level 2 functions.  You
 *  pretty much have to include this file.  It has all the prototypes,
 *  macros, and structures needed to use CommLib Level 2.
 *
 * MODIFICATIONS
 *
 * December 12, 1992  4.00A : Initial release
 *
 */

/*
 * If you have a custom header file that you want to include in every
 * source module in CommLib, you can automatically add it to all of the
 * library modules in a two step process that doesn't require you to
 * modify any of our source.  First, define GF_CUSTOM in either your
 * project file or in BUILD.INI.  Second, insert the appropriate directives
 * in a file called GFCUSTOM.H.  This header file is *not* supplied with
 * Commlib, since you are the one who is going to be providing the
 * customization.  Note that this is a good way to include a product
 * like SmartHeap or MemCheck in Commlib.  (For either of these products
 * you will also want to define GF_DISABLE_HEAP_DEBUGGER as well.
 */
#if defined( GF_CUSTOM )
#include "gfcustom.h"
#endif

#include "compiler.h"
#include "dosxptrs.h"
#if defined( GF_WINDOWS ) || defined( GF_WIN32 )
#include <windows.h>
#if defined( GF_WIN32 )
#define HTASK   HANDLE
#endif
#endif

#define COMLIBVERSION   0x400           /* Comm library Version  */

/*
 * 16550 trigger definitions are used across more than one driver.
 */
typedef enum trigger_level{
    TRIGGER_DISABLE    = 0x00,
    TRIGGER_01         = 0x01,
    TRIGGER_04         = 0x41,
    TRIGGER_08         = 0x81,
    TRIGGER_14         = 0xc1
} TRIGGER_LEVEL;

/*
 * People wonder why COM1 is an int instead of a macro.  The reason goes back
 * to the cutover to Commlib 3.2.  Lots of functions that used to take an
 * int port number in 3.1 changed to taking a PORT structure pointer in
 * 3.2.  Normally, if you try to pass an int as a pointer you will get an
 * error, so users that didn't update their code properly could count on
 * getting an error from the compiler.  However, COM1 is defined as 0, and
 * 0 is a special value that can be passed as a pointer.  So defining it
 * as an extern int avoids that particular problem, and will cause an
 * error if somebody tries to pass COM1 as an argument to ZmodemSend().
 */

#define COM1       (int)0
#define COM2            1
#define COM3            2
#define COM4            3
#define COM5            4
#define COM6            5
#define COM7            6
#define COM8            7
#define COM9            8
#define COM10           9
#define COM11           10
#define COM12           11
#define COM13           12
#define COM14           13
#define COM15           14
#define COM16           15
#define COM17           16
#define COM18           17
#define COM19           18
#define COM20           19
#define COM21           20
#define COM22           21
#define COM23           22
#define COM24           23
#define COM25           24
#define COM26           25
#define COM27           26
#define COM28           27
#define COM29           28
#define COM30           29
#define COM31           30
#define COM32           31
#define COM33           32
#define COM34           33
#define COM35           34

/*
 * Macro's and constants to ease the reading of Micro Channel POS
 * registers.
 */
#define POSCHNLSELECT            0x96         /* POS Channel Select */
#define POSLOWIDPORT             0x100        /* POS I.D. Low byte */
#define POSHIGHIDPORT            0x101        /* POS I.D. High byte */
#define POS2PORT                 0x102        /* POS Adapter info ports 2-5*/
#define POS3PORT                 0x103
#define POS4PORT                 0x104
#define POS5PORT                 0x105
#define POSCHMIN                 0x08         /* Minimum channel */
#define POSCHMAX                 0x0f         /* Maximum channel */
#define POSCHNLDISABLE           0x00         /* Disable channel access */

#define POSSELECTCHANNEL( p )    _asoutb( POSCHNLSELECT, p )
#define POSDESELECTCHANNEL( p )  _asoutb( POSCHNLSELECT, POSCHNLDISABLE )
#define POSID()                  ( ( _asinb( POSHIGHIDPORT ) << 8 ) + \
                                 _asinb( POSLOWIDPORT ) )
#define POS2INFO()               _asinb(POS2PORT)
#define POS3INFO()               _asinb(POS3PORT)
#define POS4INFO()               _asinb(POS4PORT)
#define POS5INFO()               _asinb(POS5PORT)

#define TICKS_PER_SECOND              18
#define MILLISECONDS_PER_TICK         55

#ifndef TRUE
#define TRUE            1
#endif

#ifndef FALSE
#define FALSE           0
#endif

#define GF_MKFP( seg, offset ) ( void GF_FAR *) ( ( (unsigned long) \
                                   ( seg ) << 16 ) + ( offset ) )
#define GF_MKFFP( seg, offset ) (void (GF_FAR * )()) ( ( (unsigned long) \
                                    ( seg ) << 16 ) + ( offset ) )

#define IRQ0   0
#define IRQ1   1
#define IRQ2   2
#define IRQ3   3
#define IRQ4   4
#define IRQ5   5
#define IRQ6   6
#define IRQ7   7
#define IRQ8   8
#define IRQ9   9
#define IRQ10 10
#define IRQ11 11
#define IRQ12 12
#define IRQ13 13
#define IRQ14 14
#define IRQ15 15

/*
 * Line Status
 */
#define OVERRUN 2
#define PARERR  4
#define FRAMERR 8
#define BREAKDET 16
#define THRE 32
#define TEMT 64

/*
 * Modem Status
 */
#define CTSCHG  1
#define DSRCHG  2
#define RITRAIL 4
#define CDCHG   8
#define CTS     16
#define DSR     32
#define RI      64
#define CD      128

/*
 * Error codes returned by all functions OR in _aserror.
 */
#define ASSUCCESS                0
#define ASGENERALERROR          -1
#define ASINVPORT               -2
#define ASINUSE                 -3
#define ASINVBUFSIZE            -4
#define ASNOMEMORY              -5
#define ASNOTSETUP              -6
#define ASINVPAR                -7
#define ASBUFREMPTY             -8
#define ASBUFRFULL              -9
#define ASTIMEOUT               -10
#define ASNOCTS                 -11
#define ASNOCD                  -12
#define ASNODSR                 -13
#define ASNO8250                -14
#define ASXMSTATUS              -15
#define ASUSERABORT             -16
#define ASFILERR                -17
#define ASXMERROR               -18
#define ASNOWIDERX              -19
#define ASCONFLICT              -20
#define ASCRCMODE               -21
#define ASNOHAYESOK             -22
#define ASNOHAYESRESPONSE       -23
#define ASNOTSUPPORTED          -24
#define ASILLEGALBAUDRATE       -25
#define ASILLEGALPARITY         -26
#define ASILLEGALWORDLENGTH     -27
#define ASILLEGALSTOPBITS       -28
#define ASNOCOPYRIGHTNOTICE     -29
#define ASDRIVERNOTINSTALLED    -30
#define ASOVERFLOW              -31
#define ASCONNECTFAILURE        -32
#define ASDOSEXTENDERERROR      -33
#define ASILLEGALBOARDNUMBER    -34
#define ASBOARDINUSE            -35
#define ASHANDSHAKEBLOCK        -36
#define ASMAXPORTSEXCEEDED      -37
#define ASILLEGALIRQ            -38
#define ASIRQINUSE              -39
#define ASUSERDEFINEDERROR      -75
/*
 * Number of ports supported by Windows
 */
#define MAXWINPORTS     9

#ifdef __cplusplus
extern "C" {
#endif
extern char ParityLetter[ 5 ];
#ifdef __cplusplus
}
#endif

typedef enum { OUT_OF_MEMORY = -1,
               GREENLEAF,
               BIOS,
               EXTENDED_BIOS,
               FOSSIL,
               DIGIBOARD_COMXI,
               DIGIBOARD_PCXE,
               DIGIBOARD_UNIVERSAL,
               GREENLEAF_FAST,
               MODEM_ASSIST,
               PHAR_LAP_286,
               RATIONAL_SYSTEMS_DOS_16M,
               SPARKLE,
               ARNET,
               STARGATE,
               MICROSOFT_WINDOWS,
               MICROSOFT_WIN32,
               OTHER
             } DRIVER_TYPE;

typedef enum { OVERRUN_ERROR  = 2,
               PARITY_ERROR   = 4,
               FRAMING_ERROR  = 8,
               BREAK_DETECTED = 16
             } LINE_STATUS_CODES;

typedef enum { CTS_SET  = 0x10,
               DSR_SET  = 0x20,
               RI_SET   = 0x40,
               CD_SET   = 0x80
             } MODEM_STATUS_CODES;

typedef enum { RX_CHAR           = 0x0001,
               RX_EVENT_FLAG     = 0x0002,
               TX_EMPTY          = 0x0004,
               CTS_STATE_CHANGE  = 0x0008,
               DSR_STATE_CHANGE  = 0x0010,
               RLSD_STATE_CHANGE = 0x0020,
               BREAK_STATE       = 0x0040,
               LINE_STATUS_ERR   = 0x0080,
               RI_STATE          = 0x0100,
               CTS_STATE         = 0x0400,
               DSR_STATE         = 0x0800,
               RLSD_STATE        = 0x1000,
               TERI_STATE        = 0x2000
             } EVENT_CODES;


typedef  void ( GF_DLL_FAR GF_CONV *PORT_DUMPER )( char GF_DLL_FAR *data );



/*
 * This is the PORT structure that everything else in Level 2 revolves
 * around.  All driver routines operate on PORT structures.
 */

struct GFINSTANCEDATAtag;

#define PORT struct _tag_port

struct _tag_port {
#if defined( DOSX386 ) || defined( DOS4G )
    GF_FARPTR driver;
#else
    void *driver;
#endif
    PORT GF_DLL_FAR *next_port;
    int handle;
    int status;
    DRIVER_TYPE driver_type;
    int dialing_method;
    unsigned int count;
    int ( GF_CONV * read_char )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * peek_char )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * write_char )( PORT GF_DLL_FAR *port, int c );
    int ( GF_CONV * port_close )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * port_set )( PORT GF_DLL_FAR *port,
                                long baud_rate,
                                char parity,
                                int word_length,
                                int stop_bits );
    int ( GF_CONV * use_xon_xoff)( PORT GF_DLL_FAR *port, int control );
    int ( GF_CONV * use_rts_cts )( PORT GF_DLL_FAR *port, int control );
    int ( GF_CONV * use_dtr_dsr )( PORT GF_DLL_FAR *port, int control );
    int ( GF_CONV * set_dtr )( PORT GF_DLL_FAR *port, int control );
    int ( GF_CONV * set_rts )( PORT GF_DLL_FAR *port, int control );
    long ( GF_CONV * space_free_in_TX_buffer )( PORT GF_DLL_FAR *port );
    long ( GF_CONV * space_used_in_TX_buffer )( PORT GF_DLL_FAR *port );
    long ( GF_CONV * space_free_in_RX_buffer )( PORT GF_DLL_FAR *port );
    long ( GF_CONV * space_used_in_RX_buffer )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * clear_TX_buffer )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * write_buffer )( PORT GF_DLL_FAR *port,
                                    char GF_DLL_FAR *buffer,
                                    unsigned int count );
    int ( GF_CONV * read_buffer )( PORT GF_DLL_FAR *port,
                                   char GF_DLL_FAR *buffer,
                                   unsigned int count );
    int ( GF_CONV * dump_port_status )( PORT GF_DLL_FAR *port,
                                        PORT_DUMPER printer );
    int ( GF_CONV * send_break )( PORT GF_DLL_FAR *port,
                                  int milliseconds );
    int ( GF_CONV * get_modem_status )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * get_line_status )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * clear_line_status)( PORT GF_DLL_FAR *port );
    int ( GF_CONV * block )( PORT GF_DLL_FAR *port, int control );
    void ( GF_CONV * clear_error )( PORT GF_DLL_FAR *port );
    void GF_FAR *user_data;
    struct GFINSTANCEDATAtag GF_FAR *lpThis;
};

#undef PORT

typedef struct _tag_port PORT;

/* The GFINSTANCEDATAtag structure below is used for PowerComm only.
 * A pointer to this structure has been added to the PORT structure
 * for internal use.
 */

struct GFINSTANCEDATAtag {
    int ( GF_CONV * _PortIdleFunctionPtr )( PORT GF_DLL_FAR *port );
    int ( GF_CONV * _AbortModemFunctionPtr )( PORT GF_DLL_FAR *port );
    void * ( GF_CONV GF_DLL_FAR * _XferFileOpenFunctionPtr )(
                          void GF_DLL_FAR *status,
                          char GF_DLL_FAR *name,
                          char GF_DLL_FAR *mode );
    int ( GF_CONV * _AbortXferFunctionPtr )( void GF_DLL_FAR *status );
    int _DefaultAbortKey;
    char GF_DLL_FAR * ( GF_CONV *_UserErrorNameFunctionPtr )(
                          int error_code );
    TRIGGER_LEVEL Default16550TriggerLevel;
    TRIGGER_LEVEL DefaultFast16550TriggerLevel;
    long hm_delay_value;
    char GF_FAR *hm_match_string;
    void ( GF_CONV *hm_character_printer )( char c );
    int _hm_abort_key;
    int _aserror;
    unsigned int _DefaultRXBufferSize;
    unsigned int _DefaultTXBufferSize;
#if defined( GF_WINDOWS ) || defined( GF_WIN32 )
    HTASK hTask;
#else
    int hTask;
#endif
    int nRefCount;
};

typedef struct GFINSTANCEDATAtag GFINSTANCEDATA;
typedef GFINSTANCEDATA GF_FAR * LPGFINSTANCEDATA;
LPGFINSTANCEDATA GF_CONV GetGFInstanceDataPtr( void );
LPGFINSTANCEDATA GF_CONV AddOrGetGFInstanceData( void );

/*
 * This function pointer is set up as a typedef so we can set up the
 * prototypes for idle and abort functions with a type that won't cause
 * problems with certain C++ compilers.
 */
typedef  int ( GF_DLL_FAR GF_CONV *PORT_HOOK )( PORT GF_DLL_FAR *port );

/*
 * The following macros define pseudo-functions.  These are all the
 * virtual functions defined in the PORT structure.  The macros just
 * make it easier to access the functions without worrying about the
 * port structure.
 */
#define ReadChar( p )               p->read_char( p )
#define PeekChar( p )               p->peek_char( p )
#define WriteChar( p, c )           p->write_char( p, c )
#define PortClose( p )              p->port_close( p )
#define PortSet( p, b, py, wl, sb ) p->port_set( p, b, py, wl, sb )
#define UseXonXoff( p, c )          p->use_xon_xoff( p, c )
#define UseRtsCts( p, c )           p->use_rts_cts( p, c )
#define UseDtrDsr( p, c )           p->use_dtr_dsr( p, c )
#define DumpPortStatus( p, f )      p->dump_port_status( p, f )
#define SetDtr( p, c )              p->set_dtr( p, c )
#define SetRts( p, c )              p->set_rts( p, c )
#define SpaceFreeInTXBuffer( p )    p->space_free_in_TX_buffer( p )
#define SpaceFreeInRXBuffer( p )    p->space_free_in_RX_buffer( p )
#define SpaceUsedInTXBuffer( p )    p->space_used_in_TX_buffer( p )
#define SpaceUsedInRXBuffer( p )    p->space_used_in_RX_buffer( p )
#define ClearTXBuffer( p )          p->clear_TX_buffer( p )
#define WriteBuffer( p, b, i )      p->write_buffer( p, b, i )
#define ReadBuffer( p, b, i )       p->read_buffer( p, b, i )
#define SendBreak( p, t )           p->send_break( p, t )
#define GetModemStatus( p )         p->get_modem_status( p )
#define GetLineStatus( p )          p->get_line_status( p )
#define ClearLineStatus( p )        p->clear_line_status( p )
#define Block( p, c )               p->block( p, c )
#define ClearError( p )             p->clear_error( p )

#ifdef __cplusplus
extern "C" {
#endif

unsigned short int GF_CONV CalculateBlockCRC16( unsigned int count,
                                                unsigned short int startvalue,
                                                void GF_DLL_FAR *buffer );
unsigned short int GF_CONV CalculateCharacterCRC16( unsigned short int crc,
                                                    unsigned char c );
unsigned short int GF_CONV CalculateCharacterCRC16m( unsigned short int crc,
                                                     unsigned char c );
unsigned long     GF_CONV  CalculateBlockCRC32( unsigned int count,
                                                unsigned long startvalue,
                                                void GF_DLL_FAR *buffer );
unsigned long     GF_CONV CalculateCharacterCRC32( unsigned long crc,
                                                   unsigned char c );
void              GF_CDECL _assti( void );
void              GF_CDECL _ascli( void );
int               GF_CDECL _asinb( unsigned io_address );
int               GF_CDECL _asoutb( unsigned io_address, int value );
int               GF_CONV  asitime( void );
long              GF_CONV _asgetdivisor( unsigned io_address, int ier_mask );
#if defined( GF_HIGH_C )
#pragma Alias( _assti, __assti )
#pragma Alias( _ascli, __ascli )
#pragma Alias( _asinb, __asinb )
#pragma Alias( _asoutb, __asoutb )
#endif

/*
 * This function is also used in Data Windows.  We don't define it
 * twice so as to not get a compiler error.
 */
#if !defined( DW_DOT_H )
unsigned          GF_CDECL machine( void );
#endif

void              GF_CONV  timer( unsigned ticks );
int               GF_CONV  submodel( void );
char GF_DLL_FAR * GF_CONV CommErrorName( int error_code );
char GF_DLL_FAR * GF_CONV AsciiControlCharacterName( int c );
int               GF_CONV Change8259Priority( int irq );
int               GF_CONV IsMicroChannel( void );
unsigned int      GF_CONV get_bios_segment( void );
long              GF_CONV ElapsedTime( void );
int               GF_CONV PortKillTime( PORT GF_DLL_FAR *port,
                                        long milliseconds );
int               GF_CONV DESQViewRunning( void );
int               GF_CONV WindowsEnhancedMode( void );
void              GF_CONV YieldWindowsTimeSlice( void );
void              GF_CONV YieldDESQViewTimeSlice( void );
void GF_CONV SetDefaultBufferSize( unsigned int rx_buffer_size,
                                   unsigned int tx_buffer_size );



int GF_CONV GetDsr( PORT GF_DLL_FAR *p );
int GF_CONV GetCd( PORT GF_DLL_FAR *p );
int GF_CONV GetRi( PORT GF_DLL_FAR *p );
int GF_CONV GetCts( PORT GF_DLL_FAR *p );
int GF_CONV GetParityError( PORT GF_DLL_FAR *p );
int GF_CONV GetOverrunError( PORT GF_DLL_FAR *p );
int GF_CONV GetFramingError( PORT GF_DLL_FAR *p );
int GF_CONV GetBreakDetect( PORT GF_DLL_FAR *p );
int GF_CONV WriteString( PORT GF_DLL_FAR *p, char GF_DLL_FAR *string,
                         int termination_sequence );
int GF_CONV WriteStringTimed( PORT GF_DLL_FAR *p, char GF_DLL_FAR *string,
                              int termination_sequence,
                              long milliseconds );
int GF_CONV WriteBufferTimed( PORT GF_DLL_FAR *p, char GF_DLL_FAR *buffer,
                              unsigned int count, long milliseconds );
int GF_CONV ReadCharTimed( PORT GF_DLL_FAR *p, long milliseconds );
int GF_CONV WriteCharTimed( PORT GF_DLL_FAR *port, int c, long milliseconds );
int GF_CONV ReadBufferTimed( PORT GF_DLL_FAR *port, char GF_DLL_FAR *buffer,
                             unsigned int count, long milliseconds );
int GF_CONV ReadString( PORT GF_DLL_FAR *port, char GF_DLL_FAR *buffer,
                        unsigned int size, int termination_sequence );
int GF_CONV ReadStringTimed( PORT GF_DLL_FAR *port, char GF_DLL_FAR *buffer,
                             unsigned int size,
                             int termination_sequence,
                             long milliseconds );
int GF_CONV ClearRXBuffer( PORT GF_DLL_FAR *port );
int GF_CONV IsTXEmpty( PORT GF_DLL_FAR *port );
int GF_CONV IsRXEmpty( PORT GF_DLL_FAR *port );
int GF_CONV IsTXFull( PORT GF_DLL_FAR *port );
int GF_CONV IsRXFull( PORT GF_DLL_FAR *port );

PORT * GF_CONV PortOpenFossil( int port_number, long baud_rate, char parity,
                               int word_length, int stop_bits );

PORT * GF_CONV PortOpenSmartDigiboard( int port_number, long baud_rate,
                                       char parity, int word_length,
                                       int stop_bits  );

PORT GF_DLL_FAR * GF_CONV GF_DLL_FAR PortOpenGreenleaf( int port_number, long baud_rate,
                                             char parity, int word_length,
                                             int stop_bits );

PORT * GF_CONV PortOpenGreenleafPolled( int port_number, long baud_rate,
                                        char parity, int word_length,
                                        int stop_bits  );

PORT * GF_CONV PortOpenModemAssist( int port_number, long baud_rate,
                                    char parity, int word_length,
                                    int stop_bits );

#if defined(DOSX286)
#define PortOpenGreenleafFast PortOpenPharLap286
#elif defined(DOS16M)
#define PortOpenGreenleafFast PortOpenDos16M
#endif

PORT * GF_CONV PortOpenGreenleafFast( int port_number, long baud_rate,
                                      char parity, int word_length,
                                      int stop_bits  );
PORT * GF_CONV PortOpenBIOS( int port_number, long baud_rate,
                             char parity, int word_length,
                             int stop_bits  );

PORT * GF_CONV PortOpenExtendedBIOS( int port_number, long baud_rate,
                                     char parity, int word_length,
                                     int stop_bits  );

PORT * GF_CONV PortOpenSparkle( int port_number, long baud_rate,
                                char parity, int word_length,
                                int stop_bits  );

PORT * GF_CONV PortOpenSmartArnet( int port_number, long baud_rate,
                                   char parity, int word_length,
                                   int stop_bits  );

PORT * GF_CONV PortOpenSmartStarGate( int port_number, long baud_rate,
                                      char parity, int word_length,
                                      int stop_bits  );

#if defined( GF_WINDOWS ) && !defined( GF_WIN32 )
PORT * GF_CONV PortOpenMSWindows( int port_number, long baud_rate,
                                  char parity, int word_length,
                                  int stop_bits
                                  );
unsigned int GF_CONV PortMSWindowsGetEventMask( PORT *port );
unsigned int GF_CONV PortMSWindowsGetMaskedEvents( PORT *port );
int GF_CONV PortEnableCommNotification( PORT *port,
                                        HWND hwnd,
                                        int in_notify,
                                        int out_notify,
                                        unsigned int event_mask );
#elif defined( GF_WIN32 )
PORT * GF_CONV PortOpenMSWin32( int port_number, long baud_rate,
                                char parity, int word_length,
                                int stop_bits );
#endif

void GF_CONV SetPortIdleFunctionPtr( PORT_HOOK idle_fn );
/* void GF_CONV SetPortIdleFunctionPtr( int (GF_CONV GF_DLL_FAR *f)( PORT GF_DLL_FAR *port) );
 */
void GF_CONV SetUserErrorNameFunctionPtr( char *(GF_CONV GF_DLL_FAR *f)( int error ) );
void GF_CONV SetAbortModemFunctionPtr( PORT_HOOK abort_fn );
#ifdef VGFD
int GF_CONV InitGreenleaf( void );
int GF_CONV Get_COM1( void );
#endif

#ifdef __cplusplus
}
#endif

/*
 * Things after this point are all in place in order to have compatibility
 * with earlier versions of the CommLib.  Feel free to delete everything
 * from here down if you are not using any of the old function names.
 */

/*
 *
 */
#define OFF             0
#define ON              1

#define glcrc( l, c, b )        CalculateBlockCRC16( l, c, b )
#ifdef COMPAT32
#if COMPAT32 > 0
#error Compatibility with the 3.x versions of CommLib are no longer supported!
#endif
#endif

/*
 * _debug.h has macros and prototypes for the heap debugger.  This will only
 * be invoked for certain configurations when the _DEBUG macro is
 * turned on.  If memory debugging gets turned on, we set the macro
 * GF_HEAP_DEBUGGER.  If you want to build a debug library, but don't want
 * to use our debugger, define GF_DISABLE_HEAP_DEBUGGER, and none of the
 * stuff in _DEBUG.H will get turned on.  You can define that constant
 * in either your project, or the appropriate place in BUILD.INI.
 */

//#include "_debug.h"

#endif /* #ifndef COMMLIB_DOT_H */
