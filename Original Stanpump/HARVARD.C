#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <conio.h>
#include "stanpump.h"
#include "gr_3400.h"
#include "newasync.h"

/************************************************************/
/* Harvard Pump 22: check_harvard_22 and write_harvard_22   */
/************************************************************/

int check_harvard_22()
    {                   /* check_harvard_22 */
    if (stalled_flag == 0)
        write_harvard_22("");

    return pump_status;
    }                   /*  check_harvard_22  */

char *write_harvard_22(pumpstring)
char *pumpstring;
	{
	static char result[200];

	/* first pass */
	strcpy(result, write_harvard_22_sub (pumpstring));
	if (comm_status == 0)
		return result; 

	/* second pass */
	strcpy(result, write_harvard_22_sub (pumpstring));
	if (comm_status == 0)
		return result;
		
	/* third pass */
	strcpy(result, write_harvard_22_sub (pumpstring));
	return result; 
	}

char *write_harvard_22_sub(pumpstring)
char *pumpstring;
    {                   /* write_harvard_22 */
    char *i, *o;        /* input/output text */
    char out_string[20];    /* output string */
    static char return_string[80];  /* input string */
    long counter1;      /* used to time backing up of pump */
    double back_rate;   /* rate to run backwards after syringe stall */
    int seconds;		/* used to time backwards run */

	comm_status = 0;
	pump_status = 0;
	
    /* copy to outstring, and add carriage return */
    o = &out_string[0];
    while (*o++ = *pumpstring++)
        ;
    *(o - 1) = '\r'; /* 0D */
    *o = 0;

	strcpy(return_string, pump_communication(out_string));
	
	// index to end
	i = return_string;
	while (*i)
		i++;
	i--;	// step back 1
	// determine pump status
    switch (*i)
        {
    case (':'): /* pump idle */
        run_flag = 0;
        break;

    case ('>'): /* pump running forward */
        run_flag = 1;
        break;

    case ('<'): /* pump running in reverse */
        run_flag = 0;
        break;

    case ('*'): /* pump stalled */
        /* first, stop pump */
        /* again, double transmit for safety */
        run_flag = 0;
        pump_communication ("stp\r");

        /* Now, run the pump backwards .1 cm */
        /* to loosen the damn connector! */
        back_rate = maximum_rate / 4;

        /* back off for 2 seconds */
        sprintf(out_string, "mlh %8.3lf\r", back_rate);

        /* Write out reverse rate */
        pump_communication (out_string);

        /* now reverse pump */
        pump_communication ("rev\r");

        /* wait 2 seconds */
        counter1 = absolute_clock();
        seconds = 0;
        while (seconds < 3)
        	{
			if (absolute_clock() != counter1)
				{
				seconds++;
				enter_key();	/* opportunity to escape */
				if (started_flag == 1)
					display_time(relative_clock());
				counter1 = absolute_clock();
				}
			}
        /* now stop pump */
        pump_communication("stp\r");

        /* repeat stop, for security */
        pump_communication("stp\r");

        /* for the unlikely case that the */
        /* pump came up in stalled condition */
        if (started_flag == 0)
            {
            return &return_string[0];
            }

        stalled_flag = 1;
        break;

	default:
		comm_status = 4;
		break;
		}

    if (stalled_flag == 1)
        {                     
        pump_rate_in_mls = 0.0;
        display_rate();

        /* determine cause of stall */
        if (syringe_volume <= minimum_syringe_volume)
            sys_status = 1;
        else
            pump_status = 13;

        if (purge_flag == 0)
            display_prompts();
        }

    display_pump_status();
    return &return_string[0];
    }                   /* write_harvard_22 */


void purge_harvard_22()
    {                   /* purge_harvard_22 */
    int response;       /* 1 = purge, 2 = skip */
    char harvard_string[20];    /* string sent to pump */
    double temp;

    /* purge flag prevents reading of volume infused */
    /* safety redundancy */
    while (run_flag == 1)
        write_harvard_22("stp");
    pump_rate_in_mls = 0;
    if (started_flag == 1 && transport_flag == 0)
        prior_syringes = read_volume_infused(0.0);
    purge_flag = 1;

    /* safety redundancy added */
    write_harvard_22("clv");
    while(atof(write_harvard_22("vol")) > 0.0)
        write_harvard_22("clv");
    write_harvard_22("mlt 100");
    prior_pump_volume = 0;

    get_syringe_size();
    clear_prompt_area();
    sprintf(harvard_string, "mmd %5.2lf", syringe_diameter[syringe_type]);

    /* redundancy added for safety */
    write_harvard_22(harvard_string);
    /* confirmation of syringe diameter */
    while (fabs(atof(write_harvard_22("dia")) - syringe_diameter[syringe_type]) > .001)
        write_harvard_22(harvard_string);

    gotoxy(0, 17);
    printf("Pump now programmed for %s syringe.                    ",
        syringe_name[syringe_type]);
    gotoxy(0, 18);
    printf("Place %s syringe in pump, then press Enter.", syringe_name[syringe_type]);
    enter_real(0.0, 43 + strlen(syringe_name[syringe_type]), 18);

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
            write_rate((double) max_syringe_volume[syringe_type] * 4.0);
            response = (int) enter_real(0.0, 33, 19);
            /* back off pump if error */
            write_harvard_22("");
            /* now stop pump */
            while (run_flag == 1)
                write_harvard_22("stp");
            /* additional redundancy */
            write_rate(0.0);
            response = 0;
            }
        }
    gotoxy(0, 20);
    printf("Current volume in syringe?                        ");
    temp = 0;
    while (temp < minimum_syringe_volume
         || temp > (double) max_syringe_volume[syringe_type])
        {
        gotoxy(28, 20);
        printf("               ");
        temp = enter_real(0.0, 28, 20);
        }
    syringe_volume = temp;
    /* An M1116 error occurs at this spot if a Cyrix */
    /* coprocessor is used that is not compatable with */
    /* the 80386 CPU in the computer */
    write_harvard_22("clv");
    while (atof(write_harvard_22("vol")) > 0.0)
        write_harvard_22("clv");
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
    }                   /* purge_harvard_22 */

void get_syringe_size()
    {                   /* get_syringe_size */
    int max_syringe, i;
    double radius;      /* used to calculate maximum harvard pump rate */
    double temp;
    int confirm;        /* 1 = confirmed */

    for (max_syringe = 1;  
        syringe_diameter[max_syringe];  
        max_syringe++)
        ;
    max_syringe--;

    if (one_syringe_type == 0 || syringe_type == 0)
        confirm = 0;
    else
        confirm = 1;
    while (confirm != 1)
        {
        while (syringe_type < 1 || syringe_type > max_syringe)
            {
            gotoxy(0, 21 - max_syringe);
            printf("                                                              ");
            gotoxy(0, 22 - max_syringe);
            printf("Please select syringe type:                                   ");
            for (i = 1;  i <= max_syringe;  i++)
                {
                gotoxy(0, 22 - max_syringe + i);
                printf("     %d = %s                                                   ",
                    i, syringe_name[i]);
                }
            gotoxy(0, 23);
            printf("     Your selection?                                               ");
            syringe_type = (int) enter_real(0.0, 22, 23);
            gotoxy(0, 22 - max_syringe);
            for (i = 0;  i < max_syringe + 2;  i++)
                printf("                                                              \n");
            }
        clear_prompt_area();
        gotoxy(0, 17);
        printf("Please confirm selection of %s.\n", syringe_name[syringe_type]);    
        printf("    1 = confirmed\n");
        printf("    2 = NOT confirmed\n");
        printf("    Your entry: ");
        confirm = (int) enter_real(0.0, 16, 20);
        if (confirm == 2)
            {
            syringe_type = 0;
            clear_prompt_area();
            }
        }

    minimum_syringe_volume = max_syringe_volume[syringe_type] * .02;
    syringe_warning_volume = minimum_syringe_volume * 2;

    /* convert diameter (in mm) to radius, (in cm) */
    radius = syringe_diameter[syringe_type] / 10.0 / 2.0;
    /* set max and min rate of Harvard Pump 22 */
    temp = 4.7 * 60 * radius * radius * 3.1415;
    if (temp < maximum_rate)
        maximum_rate = temp;
    minimum_rate = 0.00013 * 60 * radius * radius * 3.1415 * 4;     /* * 4 empirically added */
    /* old version  return 23 - max_syringe; */
    }       /* get_syringe_size */

