/* commands.c */
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "stanpump.h"

void command_line_a(argc, argv)
int argc;
char *argv[];
	{					/* command_line_a */
	int i;				/* index for commands on command line */
	char *c;			/* for converting args to lower case */

    /* initialize pointers in case there are no arguments */
    /* Many should be handled by statics within individual routines */
    /* but a bug in Instant-C 3.0 prevents proper initialization of */
    /* static variables....grrrr */
    /* loop through arguments */
	for (i = 1;  i < argc;  i++)
		{
        /* convert to lower case */
		c = argv[i];
		while (*c = tolower(*c))
			c++;
		if (strncmp(argv[i], "sub", 3) == 0)
			{
			if (argc > i + 1)
				{
				strcpy(subject, argv[i + 1]);
				i++;
				}
			continue;
			}
		if (strncmp(argv[i], "user", 4) == 0)
			{
			if (argc > i + 1)
				{
				strcpy(user, argv[i + 1]);
				i++;
				}
			continue;
			}
		if (strncmp(argv[i], "sim", 3) == 0)
			{
			pump_type = 1;
			continue;
			}
		if (strncmp(argv[i], "slow", 4) == 0)
			{
			if (argc > i + 1)
				{
				slow_sim = atol(argv[i + 1]);
				if (slow_sim <= 0)
					slow_sim = 0;
				else
					i++;
				}
			continue;
			}
		if (strcmp(argv[i], "gibbs") == 0)
			{
			gibbs_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "wei", 3) == 0)
			{
			if (argc > i + 1)
				{
				mass = atof(argv[i + 1]);
				if (mass < .01 || mass > 300)
					mass = 0;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "hei", 3) == 0)
			{
			if (argc > i + 1)
				{
				height = atof(argv[i + 1]);
				if (height < .01 || height > 300)
					height = 0;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "age", 3) == 0)
			{
			if (argc > i + 1)
				{
				age = atof(argv[i + 1]);
				if (age < 1 || age > 130)
					age = 0;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "gen", 3) == 0 ||
		    strncmp(argv[i], "sex", 3) == 0)
			{
			if (argc > i + 1)
				{
				if (*argv[i + 1] == 'm')
					{
					gender = 1;
					i++;
					}
				else
					{
					if (*argv[i + 1] == 'f')
						{
						gender = 2;
						i++;
						}
					}
				}
			continue;
			}
		if (strncmp(argv[i], "mal", 3) == 0)
			{
			gender = 1;
			continue;
			}
		if (strncmp(argv[i], "fem", 3) == 0)
			{
			gender = 2;
			continue;
			}
		if (strncmp(argv[i], "end", 3) == 0)
			{
			if (argc > i + 1)
				{
				force_end = atoi(argv[i + 1]) * 60;
				i++;
				}
			continue;
			}
		if (strncmp(argv[i], "del", 3) == 0)
			{
			if (argc > i + 1)
				{
				delta_seconds = atoi(argv[i + 1]);
				if (delta_seconds < 10 || delta_seconds > 60)
					delta_seconds = 10;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "des", 3) == 0)
			{
			if (argc > i + 1)
				{
				desired = atof(argv[i + 1]);
				i++;
				}
			continue;
			}
		if (strncmp(argv[i], "file", 4) == 0)
			{
			if (argc > i + 1)
				{
				strcpy(output_file, argv[i + 1]);
				i++;
				}
			continue;
			}
		}
	}					/* command_line_a */

void command_line_b(argc, argv)
int argc;
char *argv[];
	{					/* command_line_b */
	int i;				/* index for commands on command line */
	seed = 300;
	for (i = 1;  i < argc;  i++)
		{
		if (strncmp(argv[i], "kin", 3) == 0)
			{
			if (argc > i + 1)
				{
				strcpy(kinetic_file, argv[i + 1]);
				drug_id = max_drug_id;
				kinetic_set = 1;
				strcpy(argv[i + 1], "x");
				i++;
				}
			continue;
			}
		if (strncmp(argv[i], "fen", 3) == 0)
			{
			drug_id = 1;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "hug", 3) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "sco", 3) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "sha", 3) == 0)
					{
					kinetic_set = 3;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "alf", 3) == 0)
			{
			drug_id = 2;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "sco1", 4) == 0 ||
					strncmp(argv[i + 1], "scot", 4) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "sco2", 4) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "mai", 3) == 0)
					{
					kinetic_set = 3;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "hud", 3) == 0)
					{
					kinetic_set = 4;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "gor1", 5) == 0)
					{
					kinetic_set = 5;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "gor2", 5) == 0)
					{
					kinetic_set = 6;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "sha", 3) == 0)
					{
					kinetic_set = 7;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "suf", 3) == 0)
			{
			drug_id = 3;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "hud", 3) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "bov", 3) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "gep", 3) == 0)
					{
					kinetic_set = 3;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "dex", 3) == 0)
			{
			drug_id = 4;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "mar", 3) == 0)
					{
					kinetic_set = 7;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "thi", 3) == 0)
			{
			drug_id = 5;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "hum1", 4) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "hum2", 4) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "rats", 4) == 0)
					{
					kinetic_set = 4;
					i++;
					continue;
					}
				}
			continue;
			}
		}
	}					/* command_line_b */

void command_line_c(argc, argv)
int argc;
char *argv[];
	{					/* command_line_c */
	int i;				/* index for commands on command line */
	for (i = 1;  i < argc;  i++)
		{
		if (strncmp(argv[i], "mid", 3) == 0)
			{
			drug_id = 6;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "vir", 3) == 0)
					{
					kinetic_set = 5;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "dia", 3) == 0)
			{
			drug_id = 7;
			kinetic_set = 1;
			continue;
			}
		if (strncmp(argv[i], "pro", 3) == 0)
			{
			drug_id = 8;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "mar", 3) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "ped", 3) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "icu", 3) == 0)
					{
					kinetic_set = 3;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "mora", 4) == 0)
					{
					kinetic_set = 4;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "sch", 3) == 0)
					{
					kinetic_set = 5;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "lido", 4) == 0)
			{
			drug_id = 9;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "row", 3) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "voz", 3) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "roc", 3) == 0)
			{
			drug_id = 17;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "sz", 2) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "pl", 2) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "ket", 3) == 0)
			{
			drug_id = 10;
			kinetic_set = 1;
			continue;
			}
		if (strncmp(argv[i], "methadone", 9) == 0)
			{
			drug_id = 13;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "in", 2) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "sc", 2) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "pan", 3) == 0)
			{
			drug_id = 14;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "yo", 2) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "el", 2) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "vec", 3) == 0)
			{
			drug_id = 15;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "yo", 2) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "el", 2) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "atr", 3) == 0)
			{
			drug_id = 16;
			kinetic_set = 1;
			}
		if (strncmp(argv[i], "meth", 4) == 0)
			{
			drug_id = 11;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "hud", 3) == 0)
					{
					kinetic_set = 1;
					i++;
					continue;
					}
				if (strncmp(argv[i + 1], "blo", 3) == 0)
					{
					kinetic_set = 2;
					i++;
					continue;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "etom", 4) == 0)
			{
			drug_id = 12;
			kinetic_set = 1;
			continue;
			}
		
		if (strncmp(argv[i], "eff", 3) == 0)
			{
			effect_flag = 2;
			continue;
			}
		if (strncmp(argv[i], "pla", 3) == 0)
			{
			effect_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "zer", 3) == 0)
			{
			zero_order_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "tar", 3) == 0)
			{
			zero_order_flag = 0;
			continue;
			}
		}
	}					/* command_line_c */

void command_line_d(argc, argv)
int argc;
char *argv[];
	{					/* command_line_d */
	int i;				/* index for commands on command line */
	for (i = 1;  i < argc;  i++)
		{
		if (strcmp(argv[i], "c2") == 0)
			{
			pump_type = 2;
			continue;
			}
		if (strncmp(argv[i], "bard",4) == 0)
			{
			pump_type = 1;
			bard_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "har", 3) == 0)
			{
			pump_type = 3;
			continue;
			}
		if (strncmp(argv[i], "gra", 3) == 0)
			{
			pump_type = 4;
			continue;
			}
		if (strncmp(argv[i], "bd5", 3) == 0)
			{
			syringe_type = 1;
			continue;
			}
		if (strncmp(argv[i], "bd10", 4) == 0)
			{
			syringe_type = 2;
			continue;
			}
		if (strncmp(argv[i], "bd20", 4) == 0)
			{
			syringe_type = 3;
			continue;
			}
		if (strncmp(argv[i], "bd30", 4) == 0)
			{
			syringe_type = 4;
			continue;
			}
		if (strncmp(argv[i], "bd60", 4) == 0)
			{
			syringe_type = 5;
			continue;
			}
		if (strncmp(argv[i], "mj6", 4) == 0)
			{
			syringe_type = 6;
			continue;
			}
		if (strncmp(argv[i], "mj12", 4) == 0)
			{
			syringe_type = 7;
			continue;
			}
		if (strncmp(argv[i], "mj20", 3) == 0)
			{
			syringe_type = 8;
			continue;
			}
		if (strncmp(argv[i], "mj30", 4) == 0)
			{
			syringe_type = 9;
			continue;
			}
		if (strncmp(argv[i], "mj60", 4) == 0)
			{
			syringe_type = 10;
			continue;
			}
		if (strncmp(argv[i], "h5", 2) == 0)
			{
			syringe_type = 11;
			continue;
			}
		if (strcmp(argv[i], "onetype") == 0)
			{
			one_syringe_type = 1;
			continue;
			}
		if (strncmp(argv[i], "vol", 3) == 0)
			{
			if (argc > i + 1)
				{
				syringe_volume = atof(argv[i + 1]);
				if (syringe_volume < 1 || syringe_volume > 60)
					syringe_volume = 0;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "syr", 3) == 0)
			{
			if (argc > i + 3)
				{
				max_syringe_volume[11] = atoi(argv[i + 3]);
				if (max_syringe_volume[11] < 1 || max_syringe_volume[11] > 2400)
					{
					syringe_diameter[11] = 0;
					continue;
					}
				syringe_diameter[11] = atof(argv[i + 2]);
				strcpy(syringe_name[11], argv[i + 1]);
				i += 3;
				syringe_type = 11;
				}
			continue;
			}
		if (strcmp(argv[i], "study") == 0)
			{
			hudson_study(argv[i + 1]);
			continue;
			}
		if (strcmp(argv[i], "roche") == 0)
			{
			lorazepam_study_flag = 1;
			delta_seconds = 60;
			seed += 7;
			continue;
			}
		if (strcmp(argv[i], "c2sim") == 0)
			{
			pump_type = 2;
			C2_sim_flag = 1;
			continue;
			}
		if (strcmp(argv[i], "com2") == 0)
			{
			com_port = 1;
			continue;
			}
		if (strcmp(argv[i], "com3") == 0)
			{
			com_port = 2;
			continue;
			}
		if (strcmp(argv[i], "com4") == 0)
			{
			com_port = 3;
			continue;
			}
		if (strncmp(argv[i], "comp", 4) == 0)
			{
			if (argc > i + 1)
				{
				compression_factor = (float) atof(argv[i + 1]);
				if (compression_factor > 1.0)
					compression_factor = 0.05;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "con", 3) == 0)
			{
			if (argc > i + 1)
				{
				infusate_concentration = atof(argv[i + 1]);
				if (infusate_concentration < 1)
					infusate_concentration = 0;
				else
					i++;
				}
			continue;
			}
		if (strncmp(argv[i], "prior", 5) == 0)
			{
			prior_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "rep", 3) == 0)
			{
			if (argc > i + 1)
				{
				reproduce_file = fopen(argv[i + 1], "r");
				if (reproduce_file != NULL)
					{
					strcpy(reproduce_filename, argv[i + 1]);
					reproduce_flag = 1;
					track_flag = 0;
					drg_flag = 0;
					i++;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "track", 5) == 0)
			{
			if (argc > i + 1)
				{
				track_file = fopen(argv[i + 1], "r");
				if (track_file != NULL)
					{
					strcpy(track_filename, argv[i + 1]);
					track_flag = 1;
					reproduce_flag = 0;
					drg_flag = 0;
					track_time = 0;
					i++;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "drg", 3) == 0)
			{
			if (argc > i + 1)
				{
				drg_input_file = fopen(argv[i + 1], "r");
				if (drg_input_file != NULL)
					{
					strcpy(drg_input_filename, argv[i + 1]);
					drg_flag = 1;
					reproduce_flag = 0;
					track_flag = 0;
					drg_time = 0;
					i++;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "ici", 3) == 0)
			{
			propofol_study_flag = 1;
			pump_type = 3;
			one_syringe_type = 1;
			if (argc > i + 1)
				{
				if (strncmp(argv[i + 1], "demo", 4) == 0)
					{
					pump_type = 1;
					i++;
					}
				}
			continue;
			}
		if (strncmp(argv[i], "nausea", 6) == 0)
			{
			nausea_study_flag = 1;
			pump_type = 3;
			one_syringe_type = 1;
			continue;
			}
		if (strncmp(argv[i], "debug", 5) == 0)
			{
			debug_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "io", 2) == 0)
			{
			showio_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "delete", 6) == 0)
			{
			delete_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "quick", 5) == 0)
			{
			quick_flag = 1;
			continue;
			}
		if (strncmp(argv[i], "fast", 4) == 0)
			{
			pump_type = 1;
			sim_speed = 1;
			continue;
			}
		if (strncmp(argv[i], "heart", 5) == 0)
			{
			if (argc > i + 1)
				{
				heart_failure = atoi(argv[i + 1]);
				if (heart_failure < 0 || heart_failure > 1)
					heart_failure = 2; /* default */
				else
					i++;
				}
			continue;
			}
		}
	}					/* command_line_d */

