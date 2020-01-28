/************************************************************/
/* Drivers.c                                                */
/************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <conio.h>
#include "stanpump.h"
#include "gr_3400.h"
#include "newasync.h"

int enter_key();
extern int port_opened_flag;

/*********************************************************/
/* Specific Pump addressing routines                     */
/*********************************************************/

void initialize_pump()
    {                   /* initialize_pump */
    pump_status = 0;
    comm_status = 0; 
    switch (pump_type)
        {
    case (1):
        {
        warning = 0;
        break;
        }
    case (2):
        {
        /* set for maximum rate of C2 pump */
        if (maximum_rate > 999)
            maximum_rate = 999;
        if (C2_sim_flag)
            {
			if (port_opened_flag == 0)
				{
				setup_port(1200L, 'N', 8, 1);    /* 1200 baud, null parity, 8 bits, 1 stop bit */
				port_opened_flag = 1;
            	strcpy(C2_address, "1234A");
            	}
            }
        else 
            {
            if (port_opened_flag == 0)
            	{
           		setup_port(9600L, 'N', 8, 1);    /* 9600 baud, null parity, 8 bits, 1 stop bit */
            	strcpy(C2_address, "0000A");
            	/* Need to make a command line parameter or keyboard input */
            	}
            }
        pump_status = 1;
        while (pump_status + comm_status > 0)
            {
            pump_status = 0;
            write_C2("");
     	   	while (read_C2_status() == 1)
        	    {
            	gotoxy(11, 24);
	            printf("Press Computer Control / Monitor button        ");
    	        while (read_C2_status() == 1)
        	        ;
            	}
            enter_key();    /* opportunity to escape to DOS */
            }
        gotoxy(11, 24);
        printf("                                                 ");
        write_C2("INFUSTYP=PROGRAM");
        write_C2("INFUSE=OFF");
        write_C2("VTBI=500");
        write_C2("VINF=0");
        write_C2("TIMER=60");
        break;
        }
    case (3):
        {
        run_flag = 0;
        stalled_flag = 0;
        if (port_opened_flag == 0)
	        setup_port(1200L, 'N', 8, 2);    /* 1200 baud, null parity, 8 bits, 2 stop bits */
        pump_status = 1;
        while (pump_status + comm_status > 0)
            {
            gotoxy(13, 24);
            printf("\aTurn on Harvard Pump                       ");
            /* safety redundancy */
            write_harvard_22("stp");
            while (run_flag == 1)
                write_harvard_22("stp");
            enter_key();    /* opportunity to escape to DOS */
            }
        purge_harvard_22(); /* purge routine */
        break;
        }   
    case (4):
        {
        initialize_graseby_3400_pump ();
        break;
        }
    default:
        {
        }
        }
    }   /* initialize_pump */

void check_pump_status()
    {                   /* check_pump_status */
    switch (pump_type)
        {
    case (1):
        {
        break;
        }
    case (2):
        {
        pump_status = check_C2();
        warning = 0;
        break;
        }
    case (3):
        {
        pump_status = check_harvard_22();
        break;
        }
    default:
        {
        }
        }
    }                   /* check_pump_status */

void write_rate(rate)
double rate;
    {                   /* write_rate */
    char rate_string[20];
    char harvard_string[25];
    double target_volume;
    if (purge_flag == 0)    /* don't record rates if purging */
        {
        fprintf(pumpfile, "2, %ld, %9.4lf\n", relative_clock(), rate);
        fflush(pumpfile);
        }
    switch (pump_type)
        {
    case (1):
        {
        break;
        }
    case (2):
        {
        if (rate == 0)
            write_C2("INFUSE=OFF");
        else 
            {
            sprintf(rate_string, "RATE=%d", (int) rate);
            write_C2(rate_string);
            write_C2("INFUSE=ON");
            }
        break;
        }
    case (3):
        {
        if (rate == 0)
            {
            while (run_flag == 1)
                write_harvard_22("stp");
            }
        else 
            {
            /* Double check syringe size */
            while (fabs(atof(write_harvard_22("dia")) - syringe_diameter[syringe_type]) > .001)
                {
                sprintf(harvard_string, "mmd %5.2lf", syringe_diameter[syringe_type]);
                write_harvard_22(harvard_string);
                }

            sprintf(rate_string, "mlh %8.3lf", rate);
            write_harvard_22(rate_string);
            /* double check */
            while (fabs(1.0 - atof(write_harvard_22("rat"))/rate) > .01)
                write_harvard_22(rate_string);

            /* now, turn on pump */
            if ((pump_status == 0 && comm_status == 0 && sys_status == 0
                && run_flag == 0) || purge_flag == 1)
                write_harvard_22("run");
            /* now, transmit volume to stop pump if computer */
            /* glitches out */
            if (purge_flag == 0)    /* don't limit if purging */
                {
                target_volume = prior_pump_volume + rate / 3600.0 * delta_seconds * 1.5 + .5;
                sprintf(rate_string, "mlt %8.3lf", target_volume);
                write_harvard_22(rate_string);
                }
            }
        break;
        }
    case (4):
        {
        write_graseby_3400_pump_rate ( rate );
        break;
        }
    default:
        {
        }
        }
    }                   /* write_rate */

double read_volume_infused(delta)
double delta;
    {                   /* read_volume_infused */
    double volume;
    int retries;
    double temp_volume;
    double upper_limit;
    double lower_limit;
    double target_volume;
    char *volume_string;

    /* if pump is purging, then no volume has been infused */
    if (purge_flag == 1 || transport_flag == 1)
        return prior_syringes;

    /* if tracking an old run, just plug in and return */
    if (reproduce_flag)
        {
        volume = reproduce_volume - prior_syringes;
        }
    else 
        {
        switch (pump_type)
            {
        case (1):
            {
            volume = prior_reported_volume_infused + delta - prior_syringes;
            break;
            }
        case (2):
            {
            if (comm_status > 0 || pump_status > 0 || sys_status > 0)
                volume = prior_reported_volume_infused + delta - prior_syringes;
            else 
                {
                volume = atof(write_C2("VINF?") + 5);
                }
            break;
            }
        case (3):
            {
            volume = atof(write_harvard_22("vol"));

            /* validation of volume read */
            retries = 0;
            temp_volume = volume;
            target_volume = prior_pump_volume + delta;
            upper_limit = prior_pump_volume + delta * 1.1;
            lower_limit = prior_pump_volume + delta * 0.9;
            if (upper_limit == lower_limit)
                upper_limit = upper_limit * 1.1;

            while ((retries < 2) && (temp_volume < lower_limit || temp_volume > upper_limit))
                {
                temp_volume = atof(write_harvard_22("vol"));
                if (fabs(temp_volume - target_volume) < 
                    fabs(volume - target_volume))
                    volume = temp_volume;
                    retries++;
                }

            prior_pump_volume = volume;
            break;
            }
        case (4):
            {
            volume_string = read_graseby_3400_vol_infused ();
            /* need to check above: dangerous coding practice */

            if (comm_status > 0 || pump_status > 0 || sys_status > 0)
                volume = prior_reported_volume_infused + delta - prior_syringes;
            else 
                volume = atof( volume_string ) / 10.0;
            break;
            }
        default:
            {
            volume = 0;
            }
            }
        }
    volume += prior_syringes;
    fprintf(pumpfile, "1, %ld, %10.6lf\n", relative_clock(), volume);
    fflush(pumpfile);
    return volume;
    }                   /* read_volume_infused */

void stop_pump()
    {                   /* stop_pump */
    pump_status = 0;
    comm_status = 0; 
	if (port_opened_flag)
		{
		if (started_flag)
			{
			switch (pump_type)
				{
			case (2):
				write_C2("INFUSE=OFF");
				break;
			case (3):
			case (4):
				write_rate(0.0);
				break;
				}
			}
		close_port();
		}

    }   // stop_pump
