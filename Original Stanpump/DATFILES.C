/********************************************************************/
/* This program takes the *.dat file from stanpump and creates     	*/
/* a file, a *.con file showing concentration vs time in the plasma */
/* (column 3) and effect site (column 4).  Time is noted in 		*/
/* minutes since the beginning of the study (column 1) and clock 	*/
/* clock time (column 2).  Infusion rate is noted in column 5, and  */
/* cumulative mls input is noted in column 6						*/
/********************************************************************/

#include <stdio.h>
#include <math.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <sys\timeb.h>
#include <string.h>
#include <errno.h>
#include <time.h>
char *clock_time();

char inputname[64];
char outputname[64];
double compression_factor;
double concentration;
long offset;
long start_time;
double remainder;
int datafd;
FILE *output;
void save_infusion();
double total_drug;
int total_lines;
double readnumber();
struct stat filestat;

main(argc, argv)
int argc;
char *argv[];
	{
	int flag, max_flag;
	double time, value, max_time, temp;
	double last_value;
	double last_time;
	int error;
	double cp_new_time, cp_prior_time, cp_new_value, cp_prior_value,
			cp_new_slope, cp_old_slope, cp_old_value;
	double ce_new_time, ce_prior_time, ce_new_value, ce_prior_value,
			ce_new_slope, ce_old_slope;
	double rate, volume;

	total_drug = 0;
	total_lines = 0;

    /* Get initial data */
	printf("\n\n\n\n\n\nDAT File Processing program\n");
	printf("Format: datfiles inputfile outputfile compression_factor (in degrees)\n");
	if (argc > 1)
		strcpy(inputname, argv[1]);
	else
		inputname[0] = 0;

	if (argc > 2)
		strcpy(outputname, argv[2]);
	else
		outputname[0] = 0;

	if (argc > 3)
		compression_factor = atof(argv[3]);
	else
		compression_factor = 5;

	datafd = -1;
	while (datafd ==  -1)
		{
		printf("Name of input file: ");
		if (inputname[0] == 0)
			scanf("%s", inputname);
		else
			printf("%s\n", inputname);

		if (!stat(inputname, &filestat) )
			{
			printf("File: %s\n", inputname);
			printf("Size: %ld\n", filestat.st_size);
			start_time = filestat.st_atime;
			printf("Created: %s\n", ctime(&start_time));
			printf("time field: %s\n", clock_time(start_time));
			}
		datafd = open(inputname, 0);
		if (datafd == -1)
			{
			printf("     Unable to open input file %s\n", inputname);
			inputname[0] = 0;
			}
		else
			{
			/* find end of file */
			error = 0;
			max_time = 0;
			max_flag = 0;
			while (error != -1)
				{
				flag = readnumber(datafd, &error);
				time = readnumber(datafd, &error);
				temp = readnumber(datafd, &error);
				if (time > max_time)
					max_time = time;
				if (flag > max_flag)
					max_flag = flag;
				}
			printf("max time = %f\n", max_time);
			start_time -= max_time;
			printf("start time = %s\n", clock_time(start_time));
			close(datafd);
			datafd = open(inputname, 0);
			}		
		}

	output = (FILE *) 0;
	while (output == (FILE *) 0)
		{
		printf("Name of output file: ");
		if (outputname[0] == 0)
			scanf("%s", outputname);
		else
			printf("%s\n", outputname);
		output = fopen(outputname, "w");
		if (output == (FILE *) 0)
			{
			printf("Cannot open file: %s\n", outputname);
			outputname[0] = 0;
			}
		}

	fprintf(output, "minutes, true time, plasma, effect, rate (ml/hr), mls infused\n");
	printf("Compression factor: ");
	if (compression_factor == 1000)
		scanf("%lf", &compression_factor);
	else
		printf("%lf\n", compression_factor);
	compression_factor *= .017453; /* convert to radians */

	error = 0;
	cp_old_value = 0;
	cp_prior_time = 0;
	cp_prior_value = 0;
	cp_new_time = 0;
	cp_new_value = 0;
	ce_prior_time = -1;
	ce_prior_value = 0;
	ce_new_value = 0;
	ce_new_time = 0;
	cp_old_slope = -1;
	ce_old_slope = -1;
	rate = 0;
	volume = 0;
	while (error != -1)
		{
		flag = readnumber(datafd, &error);
		time = readnumber(datafd, &error);
		value = readnumber(datafd, &error);
		switch(flag)
			{
			case (1):
				{
				volume = value;
				break;
				}
			case (2):
				{
				rate = value;
				break;
				}
			case (3):
				{
				cp_prior_time = cp_new_time;
				cp_prior_value = cp_new_value;
				cp_new_time = time;
				cp_new_value = value;
				break;
				}
			case (4):
				{
				ce_prior_time = ce_new_time;
				ce_prior_value = ce_new_value;
				ce_new_time = time;
				ce_new_value = value;
				break;
				}
			}
		if (flag == max_flag)
			{
			/* break if no change in time */
			if (cp_new_time > cp_prior_time)
				{
				/* Calculate new slope */
				cp_new_slope = (cp_new_value - cp_prior_value) /
								(cp_new_time - cp_prior_time);
				ce_new_slope = (ce_new_value - ce_prior_value) /
								(ce_new_time - ce_prior_time);
				/* atan slopes */
				cp_new_slope = atan(cp_new_slope);
				ce_new_slope = atan(ce_new_slope);
				
				if (fabs (cp_new_slope - cp_old_slope) > compression_factor ||
					fabs (ce_new_slope - ce_old_slope) > compression_factor ||
					fabs (cp_old_value - cp_new_value) > cp_old_value * .1)
					{
					cp_old_slope = cp_new_slope;
					ce_old_slope = ce_new_slope;
					cp_old_value = cp_prior_value;
					fprintf(output, "%9.4lf,%s, %9.4lf, %9.5lf, %9.5lf, %9.5lf\n", 
						cp_prior_time/60, 
						clock_time(start_time + (long) cp_prior_time),
						cp_prior_value, ce_prior_value, rate, volume);
					total_lines++;
					}
				}
			}
		}
	fprintf(output, "%9.4lf,%s, %9.4lf, %9.5lf, %9.5lf, %9.5lf\n", 
				cp_new_time/60, clock_time(start_time + (long) cp_new_time),
				cp_new_value, ce_new_value, rate, volume);
	printf("Total lines output: %d\n", total_lines + 1);
	}

double readnumber(datafp, error)
int datafp;
int *error;
	{					/* readnumber */
	static char buffer[256];
	static int buffsize = 0;
	static char *okdigits = "0123456789.+-";
	static char *expdigits = "edED";
	static int n = 0;	/* where we are in buffer */
	int n1;				/* used to refill buffer */
	double i;
	double factor;
	int exponent;
	int sign;

    /* initialize if error = 1 */
	if (*error == 1)
		buffsize = 0;

	*error = 0;
    /* fill buffer if empty */
	if (n >= buffsize)
		{
		buffsize = read(datafp, &buffer[0], 256);
		if (buffsize == 0)
			{
			*error = -1;
			return (0);
			}
		n = 0;
		}

    /* index to start of number */
	while (strchr(okdigits, buffer[n]) == 0 || !buffer[n])
		{
		if (buffer[n++] == '!')
			{
			*error = -1;
			return (0);
			}
		else if (n == buffsize)
			{
			if ((buffsize = read(datafp, &buffer[0], 256)) == 0)
				{
				*error = -1;
				return (0);
				}
			n = 0;
			}
		}

    /* frame shift and fill buffer to limit if necessary*/
	if (n > buffsize - 20)
		{
		buffsize -= n;
		for (n1 = 0;  n1 < buffsize;  buffer[n1++] = buffer[n++])
			;
		buffsize += read(datafp, &buffer[buffsize], 256 - buffsize);
		n = 0;
		}

    /* adjust sign if necessary */
	i = 0;
	sign = 1;
	if (buffer[n] == '+' || buffer[n] == '-')
		{
		if (buffer[n++] == '-')
			sign = -1;
		if (n == buffsize)
			return (0);
		}

    /* read in integer portion */
	while (isdigit(buffer[n]))
		{
		i = i * 10 + buffer[n++] - '0';
		if (n == buffsize)
			return (i * sign);
		}

    /* read in decimal portion */
	if (buffer[n] == '.')
		{
		n++;
		factor = .1;
		if (n == buffsize)
			return (i * sign);


		while (isdigit(buffer[n]))
			{
			i += (buffer[n++] - '0') * factor;
			factor /= 10;
			if (n == buffsize)
				return (i * sign);
			}
		}

	i = i * sign;
    /* read in exponent */
	if (strchr(expdigits, buffer[n]) > 0)
		{
		n++;
		exponent = 0;
		sign = 1;
		if (n == buffsize)
			return (i);

        /* adjust sign if necessary */
		if (buffer[n] == '+' || buffer[n] == '-')
			{
			if (buffer[n++] == '-')
				sign = -1;
			if (n == buffsize)
				return (i);
			}

        /* read in exponents digits */
		while (isdigit(buffer[n]))
			{
			exponent = exponent * 10 + buffer[n++] - '0';
			if (n == buffsize)
				break;
			}
		return (i * pow(10.0, (double) exponent * sign));
		}
	else return i;
	}					/* readnumber */

char *clock_time(clock)
long clock;
	{					/* time of day */
	int i;
	static char timestring[50];
	static char outputstring[50] = "01/01/01 00:00:00";
	static char *months = "JanFebMarAprMayJunJulAugSepOctNovDec";
	strcpy(timestring, ctime(&clock));
	for (i = 9; i < 17; i++)
		outputstring[i]=timestring[i+2];
	timestring[7] = 0;
	i = (int) ((strstr(months,&timestring[4])-months)/3+1);
	sprintf(outputstring,"%02d",i);
	outputstring[2]='/';
	outputstring[3]=timestring[8];
	outputstring[4]=timestring[9];
	outputstring[6]=timestring[22];
	outputstring[7]=timestring[23];
	return outputstring;
	}
