/************************************************************/
/*   GR_3400.c					                           	*/
/*   Driver for Graseby 3400 Syringe Pump                  	*/
/************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <conio.h>
#include "stanpump.h"
#include "gr_3400.h"
#include "newasync.h"

/*********************************************************/
/* global variables 									 */
/*********************************************************/

char	*pump_command_string [ NUMBER_GRASEBY_PUMP_COMMANDS ] =
			{
				"STRT",				/* START_INFUSION */
				"STOP",				/* STOP_INFUSION */
				"CHGR",				/* CHANGE_RATE */
				"GETR",				/* GET_RATE */
				"TOTL",				/* GET_TOTAL_DELIVERED */
				"ZTOT",				/* ZERO_TOTAL_DELIVERED */
				"CHCK",				/* CHECK_MESSAGE */
				"CHKR",				/* CHECK_RATE */
			};

int		pump_command_parameter_count [ NUMBER_GRASEBY_PUMP_COMMANDS ] =
			{
				0,					/* START_INFUSION */
				0,					/* STOP_INFUSION */
				1,					/* CHANGE_RATE */
				0,					/* GET_RATE */
				0,					/* GET_TOTAL_DELIVERED */
				0,					/* ZERO_TOTAL_DELIVERED */
				0,					/* CHECK_MESSAGE */
				1 					/* CHECK_RATE */
			};

char	*pump_status_string [ NUMBER_GRASEBY_PUMP_STATUSES ] =
			{
				"STBY",
				"INFS",
				"ALRM",
			};


int				graseby_status;
unsigned int 	fromhex();
extern int 		enter_key();
extern int      graseby_max_rate_flag;
extern int 		port_opened_flag;
char 			*write_graseby_3400_sub();

/*********************************************************/
/* Specific Pump addressing routines                     */
/*********************************************************/

void initialize_graseby_3400_pump()
	{					/* initialize_pump */
	int i;
	if (port_opened_flag == 0)
		{
		graseby_status = PRESTATUS;
	
		setup_port(9600L, 'N', 8, 1);	/* 9600 baud, null parity, 8 bits, 1 stop bit */
    
		gotoxy(0, 6);
		printf("NEVER PRESS THE START BUTTON ON THE GRASEBY DURING COMPUTER OPERATION\n\n");
		printf("The reason is that the pressing the START button on the pump disables\n");
		printf("the computer interface.  Thus, STANPUMP loses the ability to control\n");
		printf("the pump rate, which may result in either underdosing or overdosing. \n");
		printf("For this reason, STANPUMP is very intolerant of pressing START button\n");
		printf("on the Graseby pump during computer operation.\n\n");
		printf("Press any key to acknowledge this warning  ");
		while (!kbhit());
		gotoxy(0, 6);
		for (i = 0; i < 13; i++)
			printf("                                                                        \n");

		}
	// loop until pump communication is setup
	gotoxy(13, 24);
	printf("Turn on Graseby 3400 Pump                  ");
	comm_status = 1;
	while (comm_status || pump_status) /* status */
		{
		write_graseby_3400 ( CHECK_MESSAGE, 0.0 );
		while (graseby_status == INFUSING)
			{
			display_pump_status();
			write_graseby_3400( STOP_INFUSION, 0.0 );
			}
		enter_key();	/* opportunity to escape to DOS */
		}

	gotoxy(11, 24);
	printf("                                                 ");

	/* clear total dose amount */
	write_graseby_3400 ( ZERO_TOTAL_DELIVERED, 0.0 );
	write_graseby_3400_pump_rate ( 0.0 );
	
	// get maximum rate
	clear_prompt_area();
	graseby_max_rate_flag = 1;
	write_graseby_3400_pump_rate(1300.0);
	maximum_rate = 0;
	gotoxy(0, 20);
	printf("Please look at the face plate on the Graseby 3400 pump\n");
	printf("and enter the max. rate allowed (50-1200 ml/hr): ");
	maximum_rate = 0;
	while (maximum_rate < 50 || maximum_rate > 1200)
		{
		write_graseby_3400( START_INFUSION, 0.0 );
		gotoxy(63, 21);
		printf("          ");
		maximum_rate = enter_real(0.0, 49, 21);
		}
	graseby_max_rate_flag = 0;
	write_graseby_3400_pump_rate(0.0);
	gotoxy(0,20);
	printf("                                                      \n");
	printf("                                                      \n");

	purge_graseby_3400();
    }   /* initialize_graseby_3400_pump */



/*********************************************************/
/*********************************************************/

void write_graseby_3400_pump_rate ( rate )
double rate;
{					/* write_rate */
	write_graseby_3400 ( CHANGE_RATE, rate );
	
	if ( rate == 0.0 )
	{
		while ( graseby_status == INFUSING )
			{
			display_pump_status();
			write_graseby_3400( STOP_INFUSION, 0.0 );
			}
	}
	else 
	{
	if ( graseby_status == STANDBY )
		write_graseby_3400( START_INFUSION, 0.0 );
	}
    /* Zero tolerance for pressing START on keyboard */
	if (pump_status == 17)
		{
		while ( graseby_status == INFUSING )
			{
			display_pump_status();
			write_graseby_3400( STOP_INFUSION, 0.0 );
			}
		}
}                   /* write_rate_graseby_3400 */



/*********************************************************/
/*********************************************************/

void keep_graseby_pumping (rate)
double rate;
	{					/* keep_graseby_pumping */

	if ( graseby_status == INFUSING )
			write_graseby_3400 ( CHECK_RATE, rate );
//			write_graseby_3400_pump_rate( rate );

	}                   /* keep_graseby_pumping */





/*********************************************************/
/*********************************************************/

char *read_graseby_3400_vol_infused ()
	{					/* read_volume_infused */
	return write_graseby_3400 ( GET_TOTAL_DELIVERED, 0.0 );

	}                   /* read_graseby_3400_volume_infused */

/*********************************************************
* Function:		build_command_string 
*
* Usage:		Given command code and parameter, this function
*				constructs a command string conforming to the
*				Graseby 3400 protocol.
*
* Description: 	The message format is
*				<command parameter>ss CR
*				
*				
* Return:		A pointer to the message string
*				
********************************************************/

char *build_command_string ( command_code, pumping_rate )
int		command_code;
double 	pumping_rate;
	{
	static char		command_buffer [ 80 ];
	unsigned int	index;
	unsigned char	checksum;
    unsigned char	working_buffer [ 80 ];

	// branch according to whether a rate parameter is needed
	if ( pump_command_parameter_count [ command_code ] > 0 )
		{
		// construct the message text with start and end characters
		sprintf ( working_buffer, "<%s%d>",
					pump_command_string [ command_code ],
					(int)(pumping_rate * 10.0 + 0.5 ));
		}
	else
		{
		// construct the message text with start and end characters
		sprintf ( working_buffer, "<%s>", pump_command_string [ command_code ] );
		}


	// calculate checksum
	for ( checksum = 0, index = 0; index < strlen(working_buffer); index++ )
		{
		checksum += working_buffer [ index ];
		}

	// add checksum and CR to command string
	sprintf ( command_buffer, "%s%02X%c", working_buffer, checksum, CR );

	// return the pointer to command string
	return command_buffer;
	}


/*********************************************************
* Function:		write_graseby_3400 
*
* Usage:		To perform a command-response sequence
*				for Graseby 3400 pump communication.
*
* Description: 	Given command code and data, this function
*				constructs a command string and send it to 
*				Graseby 3400 pump and wait until a response
*				packet is received.
*
* Return:		A pointer to the parameter part of response
*					message string. Also variable
*					graseby_status set to pump status code.
*				NULL pointer returned and "status" variable
*					set to non-zero value if communication
*					error detected.
*				
********************************************************/

char *write_graseby_3400 ( command_code, command_data )
int command_code;
double command_data;
	{
	static char result[200];

	// first pass
	strcpy(result, write_graseby_3400_sub (command_code, command_data));
	if (graseby_max_rate_flag == 0)
	 	display_pump_status();
	if (comm_status == 0)
		return result; 

	// second pass
	strcpy(result, write_graseby_3400_sub (command_code, command_data));
	if (graseby_max_rate_flag == 0)
		display_pump_status();
	if (comm_status == 0)
		return result;
		
	// third pass
	strcpy(result, write_graseby_3400_sub (command_code, command_data));
	if (graseby_max_rate_flag == 0)
		display_pump_status();
	return result; 
	}

char *write_graseby_3400_sub ( command_code, command_data )
int		command_code;
double 	command_data;
	{					/* write_graseby_3400_sub */
	static char 	return_string[200];	/* response from pump */
	char 			*out_string;		/* string to be sent */
	char 			*i;					/* input, output text */
	int 			j;					/* loop variable */
	unsigned char 	checksum;
	unsigned int	index;
	int				ce_flag;			/* ce flag set on return */

	comm_status = 0;
	if (command_code != CHECK_RATE)
		pump_status = 0;
	ce_flag = 0;

	gotoxy(0,10);
	/* construct the command string */
	out_string = build_command_string ( command_code, command_data );

	// now exchange strings with pump
	strcpy(return_string, pump_communication(out_string));
    
    /* check for no-response error */
	if ( return_string[0] == (char) 0 )
		{
		comm_status = 1;
		fprintf(errorfile, "No characters received before timeout\n");
		return return_string;
		}
    
    /* error if packet not completed (timeout) */
	i = return_string;
	while (*i != CR && *i != 0)
		i++;
			
	if ( *i != CR )
		{
		comm_status = 1;
		fprintf(errorfile, "CR missing from transmission: %s\n", return_string);
		return return_string;
		}
			
    /* search for begining of packet */
	i = return_string;
	while ( *i != '<' )
		{
		if ( *i == 0 )
			{
			comm_status = 3;
			fprintf(errorfile, "No '<' at beginning of packet: %s\n", return_string);
			return return_string;
			}
		else
			i--;
		}

	/* verify checksum */
	for ( checksum = 0, index = 0; i[index] != '>' && i[index] != 0; index++ )
		checksum += i [ index ];

	/* error if end of string encountered */
	if ( i [ index ] == 0 )
		{
		comm_status = 3;
		fprintf(errorfile, "No "">"" at end of packet: %s\n", return_string);
		return return_string;
		}

	/* verify checksum */
	checksum += i [index];	/* need to add last character ('>') */
	if ((fromhex(i[index+1]) * 16 + fromhex(i[index+2])) != checksum)
		{
		comm_status = 2;
		return return_string;
		}
	/* checksum verified, now decode the response string */
	i++;	/* point to ACKNOWLEDGEMENT CODE */

	if ( strncmp ( i, "OK", 2 ) != 0 )
		{               
		if (strncmp ( i, "CE", 2) == 0)
			{
			ce_flag = 1;
			}           
		else
			{
		comm_status = 3;
		fprintf(errorfile, "No ""OK"" found: %s\n", return_string);
		return return_string;
			}
		}

	/* acknowlegement code verified, now check command code */
	i += 2;								/* point to COMMAND CODE */
	if ( strncmp ( i, pump_command_string [ command_code ], 4 ) != 0 )
		{
		comm_status = 3;
		fprintf(errorfile, "Missing command string in response: %s\n",
				return_string);
		return return_string;
		}

	/* command code verified, now check status code */
	i += 4;								/* point to STATUS CODE */
	for ( j = 0; j < NUMBER_GRASEBY_PUMP_STATUSES; j++ )
		{
		if ( strncmp ( i, pump_status_string [ j ], 4 ) == 0 )
			break;
		}

	if ( j >= NUMBER_GRASEBY_PUMP_STATUSES )
		{
		pump_status = 18;
		fprintf(errorfile,"String returned: %s\n", return_string);
		return return_string;
		}

	graseby_status = j;
	if ( graseby_status == ALARM )
		{
		pump_status = 14;			/* activate the screen error message */
		run_flag = 0;
		}
	if (graseby_status == INFUSING)
		{
		run_flag = 1;
		if (ce_flag == 1)
			pump_status = 17;		// Press STOP button
		}
	if (graseby_status == STANDBY)
		{
		run_flag = 0;
        if (ce_flag == 1)
        	{
        	if (strncmp(return_string+3,"STRT",4) == 0)
				pump_status = 16;	/* Check syringe */
			else
				pump_status = 17;	/* Was error 17, stop pressed, awaiting start */
			}                                   
		}
	if ( pump_status > 0 )
		{
		pump_rate_in_mls = 0;
		if (purge_flag == 0 && started_flag == 1)
			display_rate();
		}
	return i+4;						/* return a pointer to data*/
	}                   /*write_graseby_3400_sub*/

unsigned int fromhex(a)
char a;
	{				/* fromhex */
	if (a < 'A')
		return a - '0';
	else
		return toupper(a) - 'A' + 10;
	}

void purge_graseby_3400()
    {                   /* purge_graseby_3400 */
    int response;       /* 1 = purge, 2 = skip */
    double temp;
	long now;
	double purge_rate;
	
	if (maximum_rate > 200)
		purge_rate = 200;
	else
		purge_rate = maximum_rate;
	
    /* purge flag prevents reading of volume infused */
    /* safety redundancy */
    purge_flag = 1;
    while (run_flag == 1)
    	{
        write_graseby_3400( STOP_INFUSION, 0.0);
        display_pump_status();
        }
    pump_rate_in_mls = 0;
    if (started_flag == 1 && transport_flag == 0)
        prior_syringes = read_volume_infused(0.0);

    write_graseby_3400( ZERO_TOTAL_DELIVERED, 0.0 );
    prior_pump_volume = 0;

    minimum_syringe_volume = 1;
    syringe_warning_volume = 3;
    minimum_rate = 0.1;
    clear_prompt_area();
	gotoxy(0, 17);
	printf("Place syringe in pump and carefully seat syringe clamp.");
    gotoxy(0, 18);
    printf("When done, press Enter on computer keyboard.  ");
    enter_real(0.0, 45, 18);

    response = 0;
    while (response != 2)
        {
        gotoxy(0, 19);
        printf("Do you want to purge the line (1 = y, 2 = n)?                ");
        response = (int) enter_real(0.0, 47, 19);

        if (response == 1)
            {
            gotoxy(0, 19);
            printf("Purging... press enter to stop.                 ");
            gotoxy(31, 19);
            write_rate(purge_rate);
            now = absolute_clock();
            while (response == 1)
            	{
            	if (now != absolute_clock())
            		{
            		keep_graseby_pumping(purge_rate);
            		now = absolute_clock();
            		}
            	if (kbhit())
            		response = 0;
            	}
            /* now stop pump */
            while (run_flag == 1)
                write_rate (0.0);
            response = 0;
            }
        }
    gotoxy(0, 20);
    printf("Current volume in syringe?                        ");
    temp = 0;
    while (temp < minimum_syringe_volume)
        {
        gotoxy(28, 20);
        printf("               ");
        temp = enter_real(0.0, 28, 20);
        }
    syringe_volume = temp;
    write_graseby_3400( ZERO_TOTAL_DELIVERED, 0.0 );
    prior_pump_volume = 0;

    purge_flag = 0;
    if (started_flag == 1)
        {
        init_totals_display();
        display_rate();
        display_awake();
        }
    if (warning == 1 && syringe_volume >= syringe_warning_volume)
        warning = 0;
    if (sys_status == 1 && syringe_volume >= minimum_syringe_volume)
        sys_status = 0;
    stalled_flag = 0;
    }                   /* purge_graseby_3400 */

