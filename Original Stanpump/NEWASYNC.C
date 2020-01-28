// newasync.c is a set of wrapper functions for the Greenleaf Comm library

#include 	<stdio.h>
#include	<stdlib.h>
#include    <ctype.h>
#include 	"stanpump.h"
#include 	<commlib.h> 
#include	<sacommon.h>
             
PORT *Greenleaf_port;
char *Rx_ptr;
int Rx_ready_flag;          
int imed_counter;
int overrun_error;
int framing_error;
int parity_error;
int port_opened_flag = 0;

char *pump_communication ( char *message_p );
void read_pump_char();
int enter_key();
void restore_keyboard();

void setup_port(baud, parity, bits, stop_bits)
long baud;
char parity;
int bits;
int stop_bits;
	{						/* setup_port() */
	int vxd_flag;
	vxd_flag = InitGreenleaf();
	if (WindowsEnhancedMode() && vxd_flag == 0)
		{
		cls();
		gotoxy(0, 5);
		printf("To run STANPUMP in Windows using Enhanced Mode, you must\n");
		printf("modify your system.ini file as follows:\n\n");
		printf("1. Find the section marked [386Enh].\n");
		printf("2. Delete the line that says:\n");
		printf("   device=VCD.386\n");
		printf("3. Add the following lines (to the [386Enh] section:)\n");
	        printf("   device=c:\\stanpump\\vgfd.386\n");
		printf("   device=c:\\stanpump\\vgfcd.386\n");
		printf("   GfMaxDosComPorts=4\n");
		printf("   GfMaxDosBuffPages=1\n");
		printf("4. Restart Windows\n\n");
		printf("Of course, if STANPUMP is not in the directory \"c:\\stanpump\"\n");
		printf("then you would want to refer the \"device=\" commands to\n");
		printf("the correct directory where STANPUMP is installed.\n");
		restore_keyboard();
		exit(0);
		}

	Greenleaf_port = 
		PortOpenGreenleaf(com_port, baud, parity, bits, stop_bits);
	port_opened_flag = 1;
//	if (com_port == 0 || com_port == 2)
//		Change8259Priority(IRQ4);
//	else
//		Change8259Priority(IRQ3);
	UseRtsCts(Greenleaf_port, 1);
	}					/* setup_port */


////////////////////////////////////////////////////////////////////////////////
// Function:	pump_communication 
//
// Usage:		Send the specified message to pump and wait for pump response.
//
// Description: 
//				
// Return:		A pointer to pump response string.
//				0 returned if pump response not received in 3 seconds.
//				
//				
////////////////////////////////////////////////////////////////////////////////

char *pump_communication ( char *message)
{
	long prior_time, counter;
	static char response[200];

	fprintf(com_fd, "Out: %s\n", message);
	if (showio_flag == 1)
		{
		gotoxy(0,9);
		printf("                                            ");
		gotoxy(0,10);
		printf("                                            ");
		gotoxy(0,9);
		printf("Out: %s", message);
		}
	
	ClearRXBuffer(Greenleaf_port);
	ClearTXBuffer(Greenleaf_port);
	ClearLineStatus(Greenleaf_port);
	overrun_error = 0;
	framing_error = 0;
	parity_error = 0;
	imed_counter = -1;
	
	WriteString(Greenleaf_port, message, -1);
	
	prior_time = absolute_clock();
	counter = prior_time + 2L;

	Rx_ready_flag = FALSE;
	Rx_ptr = response;
	do
		{
		read_pump_char(); 	/* This is polled, but it is read 	*/
							/* into the buffer by the fast read */
							/* interrupt command 				*/
		if (absolute_clock() != prior_time)
			{
			enter_key();	/* opportunity to escape */
			if (started_flag == 1)
				display_time(relative_clock());
			if (absolute_clock() < prior_time || absolute_clock() > counter)
				{
				*Rx_ptr++ = 0;			// termiate received string
			    fprintf(com_fd, "In: %s (timed out)\n", response);
				return response;
				}
			prior_time = absolute_clock();
			}
		} while ( !Rx_ready_flag );

    fprintf(com_fd, "In: %s\n", response);
	if (showio_flag == 1)
		{
		gotoxy(0,10);
		printf("In:  %s", response);
		}
    return response;			/* normal return */
}

void read_pump_char()
{
    int data;
    
	data = ReadChar(Greenleaf_port);
	if (overrun_error == 0 && GetOverrunError(Greenleaf_port))
		{
		fprintf(com_fd, "Overrun Error\n");
		ClearLineStatus(Greenleaf_port);
		overrun_error = 1;
		}
	if (framing_error == 0 && GetFramingError(Greenleaf_port))
		{
		fprintf(com_fd, "Framing Error\n");
		ClearLineStatus(Greenleaf_port);
		framing_error =1;
    	}
	if (parity_error == 0 && GetParityError(Greenleaf_port))
		{
		fprintf(com_fd, "Parity Error\n");
		ClearLineStatus(Greenleaf_port);
		parity_error = 1;
		}

	if (data <= 0)
    	return;
    
	switch (pump_type)
		{
	case (2):
		{
	    switch ( data )
			{
		case '<':
			if (imed_counter == -1)
				{
				imed_counter = 0;
				*Rx_ptr++ = data;
				}	/* else ignore angle bracket */
			break;
		case '>':
			imed_counter = 1;
			*Rx_ptr++ = data;
			break;
		default:		// store all other input in buffer
			if (isascii(data))
				{
				*Rx_ptr++ = data;
				if (imed_counter > 0)
					imed_counter++;
				if (imed_counter > 4)
					{
					*Rx_ptr = 0;
					Rx_ready_flag = TRUE;
					}
				}
			break;
			}
		break;
		}
	case (3):
		{
        switch (data)
            {
        case (':'): /* pump idle */
        case ('>'): /* pump running forward */
        case ('<'): /* pump running in reverse */
        case ('*'): /* pump stalled */
			*Rx_ptr++ = data;
			*Rx_ptr++ = 0;			// termiate received string
			Rx_ready_flag = TRUE;	// signal end of receiving
			break;

        case('0'):
        case('1'):
        case('2'):
        case('3'):
        case('4'):
        case('5'):
        case('6'):
        case('7'):
        case('8'):
        case('9'):
        case('.'):
			*Rx_ptr++ = data;
			break;

        default:
            break;
            }
		break;
		}
	case (4):
		{
	    switch ( data )
			{
		case 0x0d:					// carriage return signals end of packet
			*Rx_ptr++ = data;
			*Rx_ptr++ = 0;			// termiate received string
			Rx_ready_flag = TRUE;	// signal end of receiving
			break;

		case 0x0a:		// ignore line feed character
			break;

		default:		// store all other input in buffer
			*Rx_ptr++ = data;
			break;
			}
		break;
		}
	}
}

void close_port()
	{
	PortClose(Greenleaf_port);
	Greenleaf_port = NULL;
	}
