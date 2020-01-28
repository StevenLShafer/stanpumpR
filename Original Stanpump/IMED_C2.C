/**************************************************/
/* Pump formatting commands for IMED C2 protocol  */
/**************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <conio.h>
#include "stanpump.h"
#include "gr_3400.h"
#include "newasync.h"

void restore_keyboard();
void stop_pump();

int check_C2()
    {                   /*  check_C2  */

    pump_status = read_C2_status();
    warning = 0;
    display_pump_status();
    /* if error, correct */
    if (pump_status > 0 || comm_status > 0)
        {
        printf("\a");
        pump_rate_in_mls = 0.0;
        display_rate();
        }
    return pump_status;
    }                   /*  check_C2  */

int read_C2_status()
    {                   /*read_C2_status*/
    char *status_string;
    char *level[5];
    int i;              /* index for status_string */

    pump_status = 0;
    warning = 0;
    status_string = write_C2("MODE?") + 6;
    if (comm_status)
        return 0;
    /* break apart response into usable pieces */
    for (i = 0;  i < 5;  i++)
        {
        level[i] = status_string;
        while (*status_string != '\0' && *status_string != ',')
            status_string++;
        status_string++;
        *(status_string - 1) = 0;
        }
    *(status_string - 2) = 0;
    /* evaluate response */
    if (strcmp(level[1], "LOCAL") == 0)
        return 1;       /* Computer Enable */
    if (strcmp(level[4], "ALARM") == 0)
        {
        status_string = write_C2("ALARM?") + 6;
        if (comm_status)
            return 0;
        if (strcmp(status_string, "LOBAT") == 0)
            return 2;
        if (strcmp(status_string, "OCCL") == 0)
            return 3;
        if (strcmp(status_string, "AIL") == 0)
            return 4;
        if (strcmp(status_string, "DOOR") == 0)
            return 5;
        if (strcmp(status_string, "USRTMOUT") == 0)
            return 6;
        if (strcmp(status_string, "COMTMOUT") == 0)
            return 7;
        if (strcmp(status_string, "ECDRMOVD") == 0)
            return 8;
        return 9;      /* check infusion set */
        }
    return 0;
    }                   /*read_C2_status*/

char *write_C2(a)
char *a;
	{					/* write_c2 */
    char out_string[200];   /* string to be sent */
	static char result[200];

	/* construct the command string */
    strcpy(out_string, frame(a));
	
	/* first pass */
	strcpy(result, write_C2_sub (out_string));
	if (comm_status == 0)
		return result; 

	/* second pass */
	strcpy(result, write_C2_sub (out_string));
	if (comm_status == 0)
		return result;
		
	/* third pass */
	strcpy(result, write_C2_sub (out_string));
	return result; 
	}					// write_c2

char *write_C2_sub(out_string)
char *out_string;
    {                   /* write_C2_sub */
    static char return_string[200]; /* response from pump */
    char *i; 			/* input, output text */
    int j;              /* loop variable */

    comm_status = 0;

	// now exchange strings with pump
	strcpy(return_string, pump_communication(out_string));
    
    /* check for no-response error */
	if ( return_string[0] == (char) 0 )
		{
		comm_status = 1;
		display_pump_status();
		fprintf(errorfile, "No characters received before timeout\n");
		return return_string;
		}
    
    /* error if no start of frame */
	i = return_string;
    if (*i != '<')
        {
        comm_status = 1;
        display_pump_status();
        fprintf(errorfile, "No '<' received to: %s\n", out_string);
        return (char *) 0;
        }

    if (strncmp(return_string, out_string, 8) != 0)
    	{
        comm_status = 3;
        display_pump_status();
        fprintf(errorfile, "%s returned address error: %s\n",
            out_string, return_string);
        if (strncmp(return_string +8,"ERROR",5) == 0)
	       	{
	    	cls();
	    	gotoxy(0, 3);
	    	printf("IMED Pump Malfunction\n");
	    	printf("Error Code: %d\n\n", atoi(return_string + 14));
	    	printf("Please call IMED Field Service to correct problem\n");
	    	printf("before using STANPUMP\n");
	    	restore_keyboard();
	    	stop_pump();
	    	exit(0);
	    	}
		else
        	return (char *) 0;
        }
    
    /* find end of frame */
    i = return_string + 8;
    while (*i != '>')
        {
        if (*i == 0)
            {
            comm_status = 3;
            display_pump_status();
            fprintf(errorfile, "No closing brace: %s\n", return_string);
            return (char *) 0;
            }
        i++;
        }

    /* split aside CRC bytes */
    for (j = 5;  j > 0;  j--)
        *(i + j + 1) = *(i + j);
    i++;
    *i++ = 0;
    if (strcmp(calc_crc(return_string), i) == 0)
        {
        *(i - 3) = 0;
        return &return_string[8];
        }

    else 
        {
        comm_status = 2;
        display_pump_status();
        fprintf(errorfile, "%s mismatched CRC values: %s %s %s\n",
            out_string, return_string, i, calc_crc(return_string));
        return (char *) 0;
        }
    }                   /*write_C2*/

char *frame(input_text)
char *input_text;
    {                   /* frame */
    static char output_text[50] = "<A"; /* framed request/command */
    if ((strlen(output_text) == 2) || output_text[6] != *(C2_address + 4))
        {
        strcpy(&output_text[2], C2_address);
        output_text[7] = ':';
        }
    strcpy(&output_text[8], input_text);
    strncat(output_text, ">", 1);
    strncat(output_text, calc_crc(output_text), 4);
    return output_text;
    }                   /* frame */

char *calc_crc(string)
char *string;
    {                   /* calc_crc */
    int i, j;
    unsigned crc, temp;
    static char crcbuf[20];

    crc = 0;
    for (;*string != '\0';  string++)
        {
        temp = *string ^ ((crc & 0xFF00) >> 8);
        temp = (temp >> 4) ^ temp;
        temp = ((temp << 5) ^ temp) ^ (crc << 8);
        crc = (temp << 12) ^ temp;
        }

    itoa(crc, crcbuf, 16);  /* convert integer to ascii hex */
    i = strlen(crcbuf) - 1;
    for (j = 3;  j >= 0;  j--)
        {
        if (i >= 0)
            if (crcbuf[i] > 'Z')
                crcbuf[j] = crcbuf[i--] & 0xDF;
            else 
                crcbuf[j] = crcbuf[i--];
        else 
            crcbuf[j] = '0';
        }
    return crcbuf;
    }                   /* calc_crc */

