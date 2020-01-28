/************************************************************/
/*                  stanpump.c                           	*/
/* Computer Controlled Infusion Pump driver program,     	*/
/* Developed by Steven L. Shafer, Stanford University   	*/
/* in Instant-C, compiled with Microsoft Visual C++ 1.5		*/
/* Although compiled with Visual C++, the code is strictly 	*/
/* K&R C.										           	*/
/* Bayesian analysis written by Pierre Maitre and Steven 	*/
/* Shafer.													*/
/* STANPUMP is copyrighted 1986-1994, by Steven L. Shafer	*/
/* The STANPUMP source and executable code may be freely	*/
/* distributed.  STANPUMP is not a commercial product, and	*/
/* must not be sold or distributed for profit.				*/
/************************************************************/
/* When editing this file, set to 4 spaces / tab stop 		*/
/************************************************************/
/* Incomplete Revision History								*/
/* 1986-1988: Initial coding								*/
/* 11/89: Effect Site added									*/
/* 4/90: Bayesian framework									*/
/* 5/90: Variable precision (so effect site can run on PC)	*/
/* 5/90: Multicompartment UDF's added						*/
/* 5/24/90:  Speed now an external parameter				*/
/* 5/29/90:  Can now track an old run with the reproduce	*/
/*           [file] command line argument.  This is for 	*/
/*           debugging, and so will not be further			*/
/*			 documented										*/
/* 5/31/90:  Obscure bug in Findpeak fixed, causing the		*/
/*           program to occasionally hang under very 		*/
/*           narrowly confined conditions.					*/
/* 6/25/90:  Fixed bug causing loss of the drug name when	*/
/*			 external files were used.  Also added Shafer	*/
/*           fentanyl and alfentanil kinetics and 			*/
/*			 reorganized define_kinetics					*/
/* 7/17/90:  Harvard-22 driver updated to not abort when    */
/*           volume infused drops to 0, but rather to       */
/*           restore pump function and continue study.      */
/*           Revision date added to first screen.           */
/*           Precalculation of next infusion rate added     */
/*           to infuse() to avoid Raemer effect.            */
/* 12/10/90: Added rat dexmedetomidine and preliminary      */
/*           shafer propofol kinetics to STANPUMP           */
/* 05/10/91: Added etomidate and methohexital kinetics      */
/*           Removed chronofusor and IMED 929 drivers       */
/* 11/21/91: Corrected unlocated variable v in read_volume 	*/
/*           infused that may have contributed to rare b&h	*/
/*           bug.  Also corrected look_ahead by eliminating */
/*           time dependent adjustment.                     */
/* 11/25/91: Bayesian analysis added.  						*/
/*           Initial version of Bayesian routines written   */
/*           by Pierre Maitre, M.D.  Subsequent versions    */
/*           jointly authored by Pierre Maitre and Steve    */
/*           Shafer.                                        */
/*           State variables changed from compartment amts  */
/*			 to virtual compartments for each exponential   */
/* 02/10/92: Time bug fixed for long (> 48 h) infusions		*/
/*           New simulation mode added                      */
/* 02/24/92: Bayesian front end improved                    */
/* 07/15/92: MLT precision bug corrected					*/
/* 2/2/93:	 Vecuronium/pancuronium added, as well as full	*/
/*           pharmacodynamic effect model					*/
/*           Generic Bayesian model added as well			*/
/* 7/21/93:  Changed zero-order mode so that infusion does  */
/*           not stop prior infusion while the rate is      */
/*           entered.                                       */
/* 9/8/93	 Changes made to ICI ECHO and Chemotherapy      */
/*           emesis program to ensure lockout of high       */
/*           targets.  Bug preventing F9 pause of zero-     */
/*           order mode fixed.								*/
/* 9/15/94	 Serial routines changed from polling serial 	*/
/*			 port to interrupt driven routines. This 		*/
/*			 permits more reliable serial operation in  	*/
/*			 multitasking environments.						*/
/* 9/17/94	 Graseby driver added, based on development 	*/
/*           work by Jason Cheng.							*/
/* 3/18/95	 Ke0 now modified to support passing of time 	*/
/*			 of peak effect rather than ke0 as the main		*/
/*			 plasma-effect compartment equilibration 		*/
/* 			 descriptor.									*/
/* 6/28/95   Fixed the Track and Reproduce File bugs caused */
/*           by fscanf limitations.  Added the ability to   */
/* 			 track .drg style files                         */  
/* 12/5/95   Fixed the PK for dexmedetomidine (V2) problem	*/
/* 5/11/96   Added lorazepam multicenter trial randomization*/
/*           fixed bug in look ahead   						*/
/* 1/5/98	 Added ability to predefine when run would end  */
/************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <bios.h>
#include <math.h>
#include <dos.h>
#include <ctype.h>
#include <conio.h>
#include <string.h>
#include <fcntl.h>
#include <io.h>
#include <sys\types.h>
#include <sys\stat.h>
#include "bayes.h"
#include "gr_3400.h"
#include "newasync.h"
#include <commlib.h>
//Note that commlib.h is part of the Greenleaf communications library. 
//People interested in recompiling STANPUMP will need the Greenleaf 
//Communications library, and the Greenleaf PowerComm library. The Powercomm
//library is necessary for safe interrupt-driven communications
//under Windows (enhanced mode only).  Greenleaf can be reached at
//214 248-2561


/* enum 	boolean_value
			{	FALSE,
				TRUE };  */

char *revision_date = "12/18/99";

unsigned int roche_codes[] = {1382,14742,9535,28864,15368,12593,29785,3395,
1553,10137,23350,8602,6465,11672,15675,30718,13514,11070,22122,14128,27022,
8921,15970,20575,28262,3395,15663,19961,19359,12912,30718,20575,26715,20587,
16891,15663,2474,25806,19654,14447,28557,20268,9842,13833,13207,9228,7079,
29478,15356,29183,19961,14128,14447,3383,16596,30092,7386,29183,12900,19040,
12298,3076,12912,23350,23338,7079,5532,21189,16289,18745,25487,28864,
7386,2781,29183,27329,15356,29478,23964,26715,632,2155,26727,27943,9842,14128,
20268,16289,6465,1541,14128,13833,25806,15970,13833,12593,21815};
int lorazepam_study_flag;
int seed=521;

typedef union REGS REGS;

char subject[80];
char user[80];
int debug_flag = 0;		/* 1 = debug messages, 0 = silent				*/
int showio_flag = 0;	/* 1 = display I/O, 0 = silent 					*/
int pump_type;			/* type of infusion pump */
char *pump_name[] = {" ",
                     "SIMULATION",
                     "C2 Protocol",
                     "Harvard Pump 22",
					 "Graseby 3400 Pump",
					 "Ohmeda 9000",
					 "Medfusion 2010"};
double pump_precision[] = {0, .0000001, 1, .001, .1, .1, .01};
double pump_rate_in_mls;/* infusion rate (cc/hr)   				        */
int started_flag;		/* 0 if not pumping, else 1                     */
int atx;				/* x location on screen                         */
int aty;				/* y location on screen                         */
int delta_seconds;		/* seconds between adjustments                  */
long seconds;			/* most recent second (used by keyin())         */
long reference_time;	/* clock count reference point                  */
long prior_infuse_time;	/* time when last in the infuse routine         */
long prior_absolute_time;	/* used in absolute clock for > 24h patch   */
long time_ahead;		/* time from now to emergence                   */
long infuse_time;		/* time of last infuse() update					*/
long force_end;			/* time program will be forced to end			*/
int peak_time;			/* time when effect site udf peaks              */
int prior_peak_time;	/* prior peak time (speeds up effect mode calc)	*/
double t_peak;			/* time effect site concentration should peak	*/
						/* used to force recalculation of ke0 when 		*/
						/* mixing ke0 and PK from different studies		*/
int reproduce_flag;		/* 1 = tracking old pump file, 0 otherwise		*/
FILE *reproduce_file;	/* old pump file 								*/
double reproduce_rate;	/* rate to be reproduced						*/
double reproduce_volume;	/* volume to be reproduced 					*/
void reproduce_old_run();	/* actually reads the file					*/
char reproduce_filename[80];// Name of file being reproduced
int track_flag;			/* 1 = tracking concentrations, 0 otherwise		*/
FILE *track_file;		/* tracking concentration file					*/
long track_time;		/* time for change 								*/
double track_concentration;	/* concentration at time of change			*/
void track();			/* routine which follows file					*/
char track_filename[80];// Name of file being tracked
int drg_flag;			/* 1 = follow a drg file						*/
FILE *drg_input_file;	/* drg file pointer								*/
long drg_time;			/* time for change 								*/
double drg_rate;		/* new infusion rate at time of change			*/
void track_drg();		/* routine which follows file					*/
char drg_input_filename[80];// Name of drg file
double compression_factor;/* Factor for infusion array compression 		*/

/* factor to adjust over time for continuous pump rate errors */
double infusion_error_adjust = 1.0;

/* keyboard interrupt routines */
void set_keyboard();
void restore_keyboard();

/* Keith's serial I/O routines */
int com_port;
unsigned int com_address[] = {0x3F8,0x2F8,0x3E8,0x2E8};

/* Keep Lint Happy */
void purge_harvard_22();
void stop_pump();
void write_rate();
void check_pump_status();
void save_infusion();
void bayes();
void cube();
void strip_white();
void hudson_study();
void initialize_pump();
double pd_model();
double inverse_pd_model();
int keyin();
int enter_key();
int find_peak();
double recalculate_ke0();
int before_or_after();

int drug_id;
int kinetic_set;
int max_drug_id = 20;
static char *site[] = {"Plasma", "Effect site", "Effect site"};
char *drug_name[] = {" ", "fentanyl",
                    "alfentanil",
                    "sufentanil",
                    "dexmedetomidine",
                    "thiopental",
                    "midazolam",
                    "diazepam",
                    "propofol",
					"lidocaine",
					"ketamine",
					"methohexital",
					"etomidate",
					"methadone",
					"pancuronium",
					"vecuronium",
					"atracurium",
					"rocuronium",
					"lorazepam",
					"remifentanil",
                    "external                                        "};
void fentanyl();
void alfentanil();
void sufentanil();
void dexmedetomidine();
void thiopental();
void midazolam();
void diazepam();
void propofol();
void lidocaine();
void ketamine();
void methohexital();
void etomidate();
void methadone();
void pancuronium();
void vecuronium();
void atracurium();
void rocuronium();
void lorazepam();
void remifentanil();
void external();

double virtual_model();
void get_prior_infusions();
char external_file_name[64];

char parameter_name[80];	/* identity of parameters used              */
int syringe_type;
char *syringe_name[] = 
                    {" ",
                    "B-D 5 cc",
                    "B-D 10 cc",
					"B-D 20 cc",
					"B-D 30 cc",
                    "B-D 60 cc",
                    "Monoject 6 cc",
                    "Monoject 12 cc",
					"Monoject 20 cc",
					"Monoject 35 cc",
                    "Monoject 60 cc",
                    "Hamilton 5 cc                             "};
double syringe_diameter[] = 
                    {0, 12.06, 14.50, 19.13, 21.70, 26.7,
					12.7, 15.9, 20.40, 23.80, 26.60, 10.30, 0};
int max_syringe_volume[] = 
                    {0, 5, 10, 20, 30, 60, 6, 12, 20, 35, 60, 5};
char *medfusion_syringe_maker[] = 
					{"B-D",
					"Monoject",
					"Terumo",
					"B-D Glass",
					"50 ml Abboject"};
int medfusion_syringe = -1;	/* syringe type selected for medfusion			*/
static double max_medfusion_rate[][5] =
			/*BD    Mono   Teru   BD-G   Abbot */
			{
			{11.0,  11.0,  11.0,  10.0,  0}, 	/* size 0 */
			{36.0,  39.6,  39.6,  39.6,  0},	/* size 1 */
			{70.0,  80.8,  80.8,  70.0,  0},	/* size 2 */
			{104.0, 126.0, 126.0, 104.0, 0},	/* size 3 */
			{182.0, 207.0, 207.0, 0,     0},	/* size 4 */
			{234.0, 285.0, 270.0, 0,     0},	/* size 5 */
			{355.0, 356.0, 378.0, 0, 360.0}		/* size 6 */
			};
static int medfusion_syringe_size[][5] =
			{
			{1,  1,  1,  1,  0},				/* size 0 */
			{3,  3,  3,  3,  0},				/* size 1 */
			{5,  6,  5,  5,  0},				/* size 2 */
			{10, 12, 10, 10, 0},				/* size 3 */
			{20, 20, 20,  0, 0},				/* size 4 */
			{30, 35, 30,  0, 0},				/* size 5 */
			{60, 60, 60, 0, 50}					/* size 6 */
			};
char medfusion_id[5] = "0123";		/* medfusion pump id */
int maxfusion_syringe;	/* syringe type selected for medfusion			*/
double syringe_volume;	/* volume remaining in syringe (harvard 22)     */
double prior_syringes;	/* volume accumulated from prior syringes       */
double minimum_syringe_volume;	/* volume triggering an error           */
double syringe_warning_volume;	/* volume triggering a warning          */
int one_syringe_type;	/* flag = 1 if only one syringe type permitted	*/

int graseby_max_rate_flag = 0;	/* = 1 if getting max graseby rate 		*/
int yesno_flag = 0;		/* = 1 for tell enterkey that y/n ok input		*/
int pump_status;		/* pump status, 0 = OK                          */
int comm_status;		/* communication link status, 0 = OK			*/
int sys_status;			/* system status, 0 = OK						*/
int warning;			/* pump warning, 0 = no warning                 */
int stalled_flag;		/* set to 1 if pump is stalled, reset to 0      */
                        /* by operator intervention (FN 3)              */
int run_flag;			/* set to 1 if running, 0 if stopped			*/
int purge_flag;			/* set to 1 if purging, 0 at all other times    */
int transport_flag;		/* disable all serial communications during     */
                        /* pump transport (mainly for pump 22)          */
int effect_flag;		/* 1 = drive effect site, 0 = drive plasma      */
						/* 2 = drive effect itself						*/
int effect_data;		/* 1 = display effect, 0 = no data              */
int gibbs_flag;			/* 1 = gibbs pre_processing (for Dan Raemer)	*/
int prior_flag;			/* 1 = drug given prior to running stanpump		*/
int propofol_study_flag;/* 1 = propofol multicenter trial				*/
int propofol_study_phase;/* Phase of the propofol study					*/
int propofol_prompts_flag;/* Chatty prompts for propofol studies		*/
int nausea_study_flag;	/* 1 = nausea study								*/
int delete_flag;		/* 1 = ok to delete prior file of same name		*/
int quick_flag;			/* 1 = omit comfirmations prior to run			*/
int zero_order_flag;	/* 1 = just run boluses or infusions			*/
double pending_bolus;	/* bolus to be given in zero order mode			*/
double pending_infusion;/* infusion rate to be given in zero order mode */
double bard_flag;		/* 1 = simulate bard InfusO.R. rates			*/
int bolus_units = 0;	/* preferred units for bolus input				*/
int infusion_units = 0;	/* preferred units for infusion input			*/
int display_infusion_units = 4;	/* units for rate display (often above)		*/
int no_zero_order_flag;	/* 1 = zero_order_mode disallowed				*/
int plasma_only_flag;	/* 1 = only target plasma concentration			*/

double p_state[5];		/* Virtual states of plasma						*/
double e_state[5];		/* Virtual states of effect site 				*/
double p_coef[5];		/* Virtual coefficients in plasma				*/
double e_coef[5];		/* Virtual coefficients in effect site			*/
double lambda[5];		/* Lambda values (hybrid rate constants			*/
double p_udf[200];		/* Udf for plasma, arbitrary infusion length	*/
double e_udf[2701];		/* Udf for effect site, T = of delta_seconds	*/
double eff_df[2701];	/* df for effect site, rate = 0					*/

double mass;			/* patient mass in kilograms                    */
double age;				/* age in years									*/
double height;			/* height (cm)									*/
int gender;				/* patients gender, 1=male,2=female				*/
double bsa;				/* body surface area							*/
double lbm;				/* lean body mass								*/
int heart_failure;		/* 1 = failure, 0 = no failure					*/
double virtual_icu_drug;/* 1 = virtual icu flag, per Zomorodi/Somma		*/
double virtual_real_sum;/* sum of virtual plus real drugs				*/	
int scaled_mass_flag;	/* for external file, = 1 to scale vc to mass   */
double desired;			/* desired drug concentration (ng/ml)           */
double desired_effect;	/* desired effect if effect is targetted		*/
double infusate_concentration;	/* infusate concentration (ug/ml)       */
double amount_infused;	/* total amount infused                         */
double prior_reported_volume_infused;	/* prior volume amount reported	*/
										/* by infusion pump				*/
double prior_working_volume_infused;	/* prior calculation of volume	*/
										/* used in pump calculations	*/
double prior_pump_volume;				/* used to validate pump volume	*/
double maximum_rate;	/* maximum infusion rate allowed                */
double minimum_rate;	/* minimum infusion rate allowed                */
char species[15];		/* species being studied                        */
char conc_units[10];	/* concentration (ng or ug) per ml              */
char infused_units[10];	/* units for the infusate                       */

/* Rate Constants */
double vc;				/* patient vc (l)                               */
double ve;				/* effect site volume (l)                       */
double k10;				/* Rate constants, note that they get           */
double k12;				/* converted from /min to /second units         */
double k13;
double k21;
double k31;
double k41;
double cl1;				/* systemic clearance							*/

/* pharmacodynamic model */
double e0;				/* baseline effect 								*/
double emax;			/* maximum effect 								*/
double ic50;			/* concentration at 50% of maximum effect		*/
double ic50_gamma;		/* ic50 ^ gamma									*/
double gamma;			/* steepness of ce-effect curve					*/
int pd_data;			/* 1 = full pharmacodynamic model				*/

char effect_text[80];	/* text describing effect (e.g. % NMB)			*/
char effect_units[20];	/* units of drug effect							*/

double max_concentration;	/* maximum permissible concentration        */
double awake_concentration;	/* concentration desired for awakening      */
int sim_speed;			/* 1 = real time,  1+ = simulate x fold         */
long slow_sim;
int hudson_flag;		/* flag for Bob Hudson's study                  */
double ref_con;			/* reference concentration for Hudson's study	*/

FILE *pumpfile;			/* pump output file                             */
FILE *drgfile;			/* drg style output file						*/
FILE *errorfile;		/* output for communication error               */
FILE *bayes_file;		/* bayes file for pierre maitre					*/
FILE *graphics_file;	/* File in minutes, plasma, effect site for     */
						/* graphing the STANPUMP output                 */
FILE *pkfile;			/* file showing PK values used by STANPUMP		*/
FILE *com_fd;			/* unbuffered output file for communications	*/
char output_file[30];	/* file name for output                         */
char kinetic_file[64];	/* pharmacokinetic data file option             */

void main_loop();		/* Main processing loop 						*/
double model();			/* three compartment pk model, and rate calc    */
double enter_real();
char *enter_string();
double read_volume_infused();
char *write_C2();
char *write_harvard_22();
long relative_clock();
long absolute_clock();
void simulate_clock();
void toggle_sim_speed();
long next_time;			/* annoying bug with statics in Instant-C 		*/
char *clock_time();
void look_ahead();
void process_rate();
void command_line_a();
void command_line_b();
void command_line_c();
void command_line_d();
void define_kinetics();
void calculate_udfs();	/* calculate unit disposition functions 		*/
char C2_address[10];
int C2_sim_flag;

/* Make PC-Lint Happy */
void main();
void start();
void set_flags();
void terminate();
void infuse();
void display_pump_status();
void display_init();
void init_totals_display();
void display_awake();
void display_prompts();
void clear_prompt_area();
void display_update();
void display_time();
void display_desired();
void display_rate();
void gotoxy();
void cls();
void setup_port();
void initialize_globals();
void opening_screen();

void main(argc, argv)
int argc;				/* Number of command line arguments */
char *argv[];			/* array of pointers to command line arguments */
	{					/*  main  */
	started_flag = 0;	/* used by enter_key */
	propofol_study_flag = 0;
	nausea_study_flag = 0;
	lorazepam_study_flag = 0;
	initialize_globals();

	if (debug_flag == 0)
	    set_keyboard();

	command_line_a(argc, argv);	/* Process command line arguments */
	command_line_b(argc, argv);
	command_line_c(argc, argv);
	command_line_d(argc, argv);
	define_kinetics();	/* define the drug and kinetic parameters */
	gotoxy(0, 21);
	printf("Please wait...");
	if (prior_flag)
		get_prior_infusions();
	start();			/* get additional data, initialize & start pump */
	started_flag = 1;	/* now we are off and running           */
	display_init();
	atx = 0;
	aty = 25;
	do
		{
		main_loop();
		if (bayes_processing_flag)
			bayes();
		}
	while (TRUE);
	}					/* main */

void main_loop()
	{
	int c;
	double temp, max_temp, min_temp;
	double temp_desired, temp_desired_effect;
	int tempint, i;
	char temp_effect_text[80];
	c = keyin();		/* get keyboard character               */
                       /* Note that in the process of checking */
                       /* the keyboard, keyin() will also      */
                       /* check to see if it is time to update */
                       /* the clock on the screen and will     */
                       /* branch to the pump control routine   */
                       /* [infuse()] at the appropriate        */
                       /* time.                                */
	switch (c)			/* switch to appropriate routine */
		{
	case 'n':			/* new wake-up concentration */
		{
		if (hudson_flag || lorazepam_study_flag)
			break;
		clear_prompt_area();
		gotoxy(0, 17);
		if (effect_flag < 2)
			{
			printf("New look-ahead concentration?");
			temp = enter_real(awake_concentration, 31, 17);
			if (temp >= .05)
				awake_concentration = temp;
			}
		else
			{
			max_temp = (e0 + emax) * .99999;
			printf("New look-ahead %s %s?", effect_text, effect_units);
			atx = 18 + strlen(effect_text) + strlen(effect_units);
			temp = enter_real(pd_model(awake_concentration), atx, 17);
			if (temp > e0 && temp <= (e0 + emax) * .99999)
				awake_concentration = inverse_pd_model(temp);
			}
		clear_prompt_area();
		display_prompts();
		look_ahead();
		atx = 0;
		aty = 25;
		gotoxy(atx, aty);
		break;
		}
	case 'r':			/* resume pump operation after error */
		{				/* and F3: change syringe */
		switch (pump_type)
			{
		case 3:
			while (run_flag == 1)
				write_harvard_22("stp");
			temp_desired = desired;
			temp_desired_effect = desired_effect;
			desired = 0;
			desired_effect = 0;
			prior_peak_time = peak_time;
			infuse();
			if (one_syringe_type == 0)
				syringe_type = 0;
			purge_harvard_22();	/* purge dialog */
			clear_prompt_area();
			display_prompts();
			desired = temp_desired;
			desired_effect = temp_desired_effect;
			prior_peak_time = peak_time;
			display_desired();
			infuse();
			atx = 0;
			aty = 25;
			gotoxy(atx, aty);
			break;
		case 4:
			while (run_flag == 1)
				write_graseby_3400( STOP_INFUSION, 0.0 );
			temp_desired = desired;
			temp_desired_effect = desired_effect;
			desired = 0;
			desired_effect = 0;
			prior_peak_time = peak_time;
			infuse();
			purge_graseby_3400();	/* purge dialog */
			clear_prompt_area();
			display_prompts();
			desired = temp_desired;
			desired_effect = temp_desired_effect;
			prior_peak_time = peak_time;
			display_desired();
			infuse();
			atx = 0;
			aty = 25;
			gotoxy(atx, aty);
			break;			
			}
		break;
		}
	case 's':			/* F5: toggle simulation speed */
		{
		if (pump_type == 1)
			{
			toggle_sim_speed();
			display_prompts();
			}
		break;
		}
	case 'p':			/* F8: toggle effect site mode */
		{
		if (zero_order_flag == 0 && plasma_only_flag == 0)
			{
			if (pd_data == 0)
				{
				effect_flag = 1 - effect_flag;
				}
			else
				{
				effect_flag++;
				if (effect_flag == 3)
					effect_flag = 0;
				}
			if (effect_flag == 2)
				desired_effect = pd_model(desired);
			display_prompts();
			display_desired();
			prior_peak_time = peak_time;
			infuse();
			}
		break;
		}

	case 't':			/* F9: toggle transport mode (pump pause) */
		{
		if (pump_type == 1)
			break;		/* No transport mode if no pump */
		if (transport_flag == 0)
			{
			switch (pump_type)
				{
			case 3:
				/* first, stop pump, with a confirmed transmission of stp */
				run_flag = 1;
				while (run_flag == 1)
					write_harvard_22("stp");
				desired = 0;
				pending_bolus = 0;
				pending_infusion = 0;
				display_desired();
				prior_peak_time = peak_time;
				infuse();
				prior_syringes = read_volume_infused((double) 0.0);
				transport_flag = 1;
				sys_status = 2;
				display_pump_status();
				clear_prompt_area();
				display_prompts();
				break;
			case 4:
				/* first, stop pump, with a confirmed transmission of stp */
				run_flag = 1;
				while (run_flag == 1)
					write_graseby_3400(STOP_INFUSION, 0.0);
				desired = 0;
				pending_bolus = 0;
				pending_infusion = 0;
				display_desired();
				prior_peak_time = peak_time;
				infuse();
				prior_syringes = read_volume_infused((double) 0.0);
				transport_flag = 1;
				sys_status = 2;
				display_pump_status();
				clear_prompt_area();
				display_prompts();
				break;
				}
			}
		else
			{
			initialize_pump();
			transport_flag = 0;
			sys_status = 0;
			}
		display_pump_status();
            /* no break, fall through to case 'c' to get new target */
		}
	case 'c':			/* new target concentration */
		{
		if (transport_flag == 0 && sys_status != 2)
			{
			infusion_error_adjust = 1;		// reset to 1 for new target
			clear_prompt_area();
			gotoxy(0, 17);
			if (zero_order_flag == 1)
				{
				printf("Please select:\n");
				printf("     1 = Give bolus dose\n");
				printf("     2 = Set infusion rate\n"); 
				printf("     Your selection: ");
				tempint = (int) enter_real(0.0, 21, 20);
				switch(tempint)
					{
				case (1):
					{
					temp = 0;
					if (bolus_units == 0)
						{
						gotoxy(0, 17);
						printf("Bolus dose units: \n");
						printf("     1 = %ss             \n", infused_units);
						printf("     2 = %ss/kg            \n", infused_units);
						printf("     3 = %ss/kg            \n", conc_units);
						printf("     Your selection:    ");
						bolus_units = (int) enter_real(0.0, 21, 21);
						}
					gotoxy(0, 22);
					switch(bolus_units)
						{
					case (1):
						{
						printf("Bolus dose (%ss): ", infused_units);
						temp = enter_real(-1.0, 18, 22);
						break;
						}
					case (2):
						{
						printf("Bolus dose (%ss/kg): ", infused_units);
						temp = enter_real(-1.0, 21, 22) * mass;
						break;
						}
					case (3):
						{
						printf("Bolus dose (%ss/kg): ", conc_units);
						temp = enter_real(-1.0, 21, 22) * mass / 1000;
						break;
						}
						}
					if (temp <= 0.0)
						bolus_units = 0;
					if (temp >= 0)
						pending_bolus = temp;
					break;
					}
				case (2):
					{
					temp = 0;
					if (infusion_units == 0)
						{
						gotoxy(0, 17);
						printf("Infusion rate units: \n");
						printf("     1 = %ss/min         \n", infused_units);
						printf("     2 = %ss/hour          \n", infused_units);
						printf("     3 = %ss/kg/min   \n", infused_units);
						printf("     4 = %ss/kg/min   \n", conc_units);
						printf("     5 = %ss/kg/hour  \n", infused_units);
						printf("     Your selection:    ");
						infusion_units = (int) enter_real(0.0, 21, 23);
						}
					if (infusion_units > 0 && infusion_units < 6)
						display_infusion_units = infusion_units;
					gotoxy(0, 23);
					switch(infusion_units)
						{
					case (1):
						{
						printf("Infusion rate (%ss/min):     ", infused_units);
						temp = enter_real(-1.0, 25, 23) / 60;
						break;
						}
					case (2):
						{
						printf("Infusion rate (%ss/hour):     ", infused_units);
						temp = enter_real(-1.0, 26, 23) / 3600;
						break;
						}
					case (3):
						{
						printf("Infusion rate (%ss/kg/min):    ", infused_units);
						temp = enter_real(-1.0, 28, 23) * mass / 60;
						break;
						}
					case (4):
						{
						printf("Infusion rate (%ss/kg/min):    ", conc_units);
						temp = enter_real(-1.0, 28, 23) * mass / 60 / 1000;
						break;
						}
					case (5):
						{
						printf("Infusion rate (%ss/kg/hour):    ", infused_units);
						temp = enter_real(-1.0, 29, 23) * mass / 3600;
						break;
						} /* close case */
						} /* close switch statement */
					if (temp <= 0)
						infusion_units = 0;
					if (temp >= 0)
						pending_infusion = temp;
					break;
					} /* close outside case */
					} /* close outside switch */
				} /* close if brace */
			else
				{
				if (hudson_flag || lorazepam_study_flag)
					{
					if (hudson_flag == 1)
						{
						printf("New percent of reference?    ");
						atx = 26;
						}
					if (lorazepam_study_flag == 1)
						{
						printf("New midazolam/lorazepam concentration? ");
						atx = 39;
						}
					}
				else
					{
					if (effect_flag < 2)
						{
						if (propofol_study_phase == 2 && drug_id == 8)
							{
							printf("New propofol level? ");
							atx = 21;
							}
						else
							{
							printf("New target concentration? ");
							atx = 27;
							}
						}
					else
						{
						printf("New desired %s? ", effect_text);
						atx = 14 + strlen(effect_text);
						}
					}
				if (virtual_icu_drug > 0)
					desired = virtual_real_sum;
				temp = enter_real(desired, atx, 17);
				if (virtual_icu_drug > 0)
					{
					virtual_real_sum = temp;
					desired = virtual_real_sum - virtual_icu_drug;
					}
				if (hudson_flag)
					temp = temp * ref_con / 100.0;
				if (lorazepam_study_flag == 1)
					{
					if (drug_id == 18)
						temp = temp * 1;
					}
				if (effect_flag < 2)
					max_temp = max_concentration;
				else
					max_temp = (e0 + emax) * .99999;
				if (propofol_study_phase == 2 && drug_id == 8)
					{
					max_temp = desired * 2;
					if (max_temp > 5)
						max_temp = 5;
					if (max_temp < 1.0)
						max_temp = 1;
					}
				if (nausea_study_flag == 1)
					{
					max_temp = desired * 2;
					if (max_temp > 1)
						max_temp = 1;
					if (max_temp < 0.25)
						max_temp = 0.25;
					}
				if (temp <= max_temp)
					{
					if (effect_flag < 2)
						{
						desired = temp;
						}
					else
						{
						if (temp >= e0)
							{
							desired_effect = temp;
							desired = inverse_pd_model(desired_effect);
							}
						}
					prior_peak_time = peak_time;
					display_desired();
					}
				}
			clear_prompt_area();
			display_prompts();
			infuse();
			atx = 0;
			aty = 25;
			gotoxy(atx, aty);
			}
		break;
		}
	case 'b':
		{
		if (bayes_data)
			{
			if (bayes_processing_flag == 0)
				{
				clear_prompt_area();
				gotoxy(0, 17);
				if (pd_data == 0)
					printf("Time of blood sample? ");
				else
					printf("Time of observation? ");
				atx = 22 - pd_data;
				bayes_time = enter_real(0.0, atx, 17);
				if (bayes_time > 0 && bayes_time <= (double) seconds / 60.0)
					{
					gotoxy(0, 17);
					if (pd_data == 0)
						{
						printf("Blood %s level?        ", drug_name[drug_id]);
						atx = 14 + strlen(drug_name[drug_id]);
						bayes_level = enter_real((double) 0, atx, 17);
						}
					else
						{
						strcpy(temp_effect_text, effect_text);
						temp_effect_text[0] = toupper(temp_effect_text[0]);
						printf("%s (%s)? ", temp_effect_text, effect_units);
						bayes_level = -1;
						max_temp = e0 + emax * 0.95;
						min_temp = e0 + emax * 0.05;
						while (bayes_level != 0 &&
							(bayes_level < min_temp || 
							bayes_level > max_temp))
							{
							atx = 6 + strlen(effect_text) + strlen(effect_units);
							gotoxy (atx, 17);
							printf("              ");
							bayes_level = enter_real((double) 0, atx, 17);
							if (bayes_level > max_temp)
								{
								gotoxy (0, 18);
								printf("\aCannot accept levels higher than %6.4f ", max_temp);
								}
							if (bayes_level != 0 && bayes_level < min_temp)
								{
								gotoxy(0, 18);
								printf("\aCannot accept levels less than %6.4f   ", min_temp);
								}
							}
						if (bayes_level != 0)
							bayes_level=inverse_pd_model(bayes_level);
						}
					for (tempint = 1;
							tempint <= n_measure && bayes_processing_flag == 0; 
								tempint++)
						{
						if ((int) (b_tm[tempint] * 100) == (int) (bayes_time * 100))
							{
							bayes_processing_flag = 1;
							if (bayes_level > 0)
								{
								/* change measurement */
								b_cpm[tempint] = bayes_level;
								}
							else
								{
								/* delete old entry */
								for (i = tempint; i < n_measure; i++)
									{
									b_cpm[i] = b_cpm[i + 1];
									b_cpp[i] = b_cpp[i + 1];
									b_tm[i] = b_tm[i + 1];
									}
								n_measure--;
								if (n_measure < 20)
									{
									gotoxy(51, 4 + n_measure);
									printf("                             ");
									}
								}
							}
						else
							{
							if (b_tm[tempint] > bayes_time)
								{
								bayes_processing_flag = 1;
								/* make room for new entry */
								for (i = n_measure; i >= tempint; i--)
									{
									b_cpm[i+1] = b_cpm[i];
									b_cpp[i+1] = b_cpp[i];
									b_tm[i+1] = b_tm[i];
									}
								n_measure++;
								b_tm[tempint] = bayes_time;
								b_cpm[tempint] = bayes_level;
								b_cpp[tempint] = 0;
								}
							}
						}
					if (bayes_processing_flag == 0)
						{
						if (bayes_level > 0)
							{
							bayes_processing_flag = 1;
							n_measure++;
							/* one more measured concentration */
							b_tm[n_measure] = bayes_time;
							b_cpm[n_measure] = bayes_level;
							b_cpp[n_measure] = 0;
							}
						}
					if (n_measure == 60)
						{
						/* toss out first value on stack */
						for (tempint = 1; tempint < n_measure; tempint++)
							{
							b_cpm[tempint] = b_cpm[tempint + 1];
							b_cpp[tempint] = b_cpp[tempint + 1];
							b_tm[tempint] = b_tm[tempint + 1];
							}
						}
					if (bayes_processing_flag)
						{
						gotoxy(55, 0);
						printf("        Bayesian Updating");
						gotoxy(55, 2);
						printf("Iteration:               ");
						}
					}
				}
			else
				{
				bayes_processing_flag = 0;
				gotoxy(55, 2);
				printf("Bayesian update aborted  ");
				gotoxy(80 - strlen(pump_name[pump_type]), 0);
				printf("%s", pump_name[pump_type]);
				n_measure--;
				if (n_measure < 20)
					first_displayed = 1;
				else
					first_displayed = n_measure - 19;
				for (tempint = first_displayed; tempint <= n_measure; tempint ++)
					{
					gotoxy(51, 4 + tempint - first_displayed);
					if (pd_data == 0)
						printf("%8.2lf %9.3lf  %9.3lf", b_tm[tempint], b_cpm[tempint], b_cpp[tempint]);
					else
						printf("%8.2lf %9.3lf  %9.3lf", b_tm[tempint], 
							pd_model(b_cpm[tempint]), pd_model(b_cpp[tempint]));
					}
				if (n_measure < 20)
					{
					gotoxy(51, 4 + n_measure);
					printf("                             ");
					}
				}
			clear_prompt_area();
			display_prompts();
			atx = 0;
			aty = 25;
			gotoxy(atx, aty);
			}
		break;
		}
	case 'z':		/* toggle zero_order_flag */
		{
		if (propofol_study_flag == 0 && lorazepam_study_flag == 0)
			{
			if (zero_order_flag == 0)
				{
				zero_order_flag = 1;
				desired = 0;
				desired_effect = e0;
				prior_peak_time = peak_time;
				display_desired();
				infuse();
				clear_prompt_area();
				display_prompts();
				}
			else
				{
				pending_infusion = 0;
				pending_bolus = 0;
				zero_order_flag = 0;
				display_desired();
				display_prompts();
				}
			}
		break;
		}
	case 'm':		/* Change Maximum Infusion Rate (Graseby) */
		{
		if (pump_type == 4)
			{
			clear_prompt_area();
			maximum_rate == 0;
			do 	{
				gotoxy(0, 17);
				printf("Maximum Graseby rate (1-600)?       ");
				maximum_rate = enter_real(maximum_rate, 30, 17);
				}
			while (maximum_rate < 1 || maximum_rate > 600);
			display_prompts();
			atx = 0;
			aty = 25;
			gotoxy(atx, aty);
			}
		break;
		}	
	case 'q':			/* quit */
		{
		clear_prompt_area();
		gotoxy(0, 17);
		printf("Verify: Press 'END' to terminate infusion.    \n");
		printf("        F1 to resume infusion.                \n");
		gotoxy(atx, aty);
		while ((c = keyin()) == 0)
			;
		if (c == 'e')
			{
			terminate();
			}
		else
			{
			clear_prompt_area();
			display_prompts();
			gotoxy(atx, aty);
			}
		break;
		}
	default:			/* any other key is ignored */
		{
		c = 0;
		}
		}
	}					/* main_loop */

void start()
	{					/* start */
	char c;				/* keyboard input character */
	char *a;			/* pointer for terminating name */
	char ccp_file_name[40];	/* ccp style output file */
	char drg_file_name[40];	/* .drg style output file */
	char pk_file_name[40];	/* pk values */
	char err_file_name[40];	/* file for error messages */
	char bayes_file_name[40];	/* file for bayesian info */
	char com_file_name[40];	/* communications file */
	char graphics_file_name[40];
	char yesno[10];			/* character input */
	int center;			/* study center (propofol study) */
	int patient_number;	/* patient number (propofol study) */
	char initials[20];	/* patient's initials */
	int phase_error;
	char phase_char;
	int temp_pump_type;	/* hold pump type while in loops */

	cls();
    /* open up file for study data */
	pumpfile = (FILE *) 0;

	propofol_study_phase = 0;
	while (pumpfile == (FILE *) 0)
		{
		if (propofol_study_flag > 0)
			{
			yesno[0] = 0;
			while (yesno[0] != 'y')
				{
				yesno[0] = 0;
				center = 0;
				propofol_study_phase = 0;
				patient_number = 0;
				initials[0] = 0;
				while (center < 1 || center > 50)
					{
					gotoxy(0, 4);
					printf("Study center?         ");
					center = (int) enter_real(0.0, 14, 4);
					}
				while (propofol_study_phase < 1 || propofol_study_phase > 3)
					{
					gotoxy(0, 5);
					printf("Study phase?         ");
					propofol_study_phase = (int) enter_real(0.0, 13, 5);
					phase_error = 0;
					switch (propofol_study_phase)
						{
					case (1):
						{
						if (drug_id == 8)
							phase_char = 'A';
						else
							{
							if (drug_id == 3)
								phase_char = 'B';
							else
								{
								phase_error = 1;
								}
							}
						break;
						}
					case (2):
						{
						if (drug_id == 8)
							phase_char = 'D';
						else
							{
							if (drug_id == 3)
								phase_char = 'S';
							else
								{
								phase_error = 1;
								}
							}
						break;
						}
					case (3):
						{
						if (drug_id == 8)
							phase_char = 'P';
						else
							{
							phase_error = 1;
							}
						break;
						}
						}
					if (phase_error == 1)
						{
						printf("%s not in phase %d", drug_name[drug_id], propofol_study_phase);
						propofol_study_phase = 0;
						}
					}
				while (patient_number < 1 || patient_number > 500)
					{
					gotoxy(0, 6);
					printf("Patient number?                                     ");
					patient_number = (int) enter_real(0.0, 16, 6);
					}
				while (initials[0] == 0)
					{
					gotoxy(0, 7);
					printf("Patient's 2 initials?         ");
					gotoxy(22, 7);
					scanf("%s", initials);
					}
				while (tolower(yesno[0]) != 'y' && tolower(yesno[0]) != 'n')
					{
					gotoxy(0, 8);
					printf("Accept above entries (y/n)?        ");
					gotoxy(28, 8);
					scanf("%s", yesno);
					}
				}
			sprintf(output_file, "%02d%c%03d%-2s", center, phase_char, patient_number, initials);
			a = &output_file[0];
			while (*a != ' ' && *a != '\0')
				a++;
			*a = '\0';
			}
		else
			{
			gotoxy(0, 4);
			printf("Name of study file?                 ");
			while (output_file[0] == 0)
				{
				gotoxy(20, 4);
				printf("                ");
				gotoxy(20, 4);
				strcpy(output_file, enter_string(20, 4));
				}

			a = &output_file[0];
			while (*a != '.' && *a != '\0' && *a != ' ')
				a++;
			*a = '\0';

			gotoxy(20, 4);
			printf("%s     ", output_file);
			}

		gotoxy(0, 7 + 3 * propofol_study_flag);
		printf("                                               ");
		gotoxy(0, 7 + 3 * propofol_study_flag);
		strcpy(ccp_file_name, output_file);
		strcat(ccp_file_name, ".dat");
		pumpfile = fopen(ccp_file_name, "r");
		if (pumpfile > (FILE *) 0)
			{
			printf("File already exists.  Delete old file (y/n) ? ");
			if (delete_flag == 0 && quick_flag == 0)
				{
				c = 0;
				while (c == 0)
					{
                    yesno_flag = 1;
					c = enter_key();
					yesno_flag = 0;
					switch (c)
						{
					case ('Y'):
					case ('y'):
						printf("y\n");
						break;
					case ('N'):
					case ('n'):
						printf("n\n");
						output_file[0] = 0;
						break;
					default:
						c = 0;
						}
					}
				}
			pumpfile = (FILE *) 0;
			}
		if (output_file[0] > 0)
			{
			printf("Opening unfiltered data file: %s\n", ccp_file_name);
			pumpfile = fopen(ccp_file_name, "w");
			if (pumpfile == (FILE *) 0)
				{
				printf("     Unable to open file %s\n", ccp_file_name);
				output_file[0] = 0;
				}
			}
		}

	/* output drg style file */
	strcpy(drg_file_name, output_file);
	strcat(drg_file_name, ".drg");
	printf("Opening filtered infusion file: %s\n", drg_file_name);
	drgfile = fopen(drg_file_name, "w");
	fprintf(drgfile, "9999 0\n");

	/* output pk file */
	strcpy(pk_file_name, output_file);
	strcat(pk_file_name, ".pk");
	printf("Opening pk values file: %s\n", pk_file_name);
	pkfile = fopen(pk_file_name, "w");

	/* output graphics file */

    /* open up error file */
	strcpy(err_file_name, output_file);
	strcat(err_file_name, ".err");
	printf("Opening error recording file: %s\n", err_file_name);
	errorfile = fopen(err_file_name, "w");

	if (bayes_data)
		{
		strcpy(bayes_file_name, output_file);
		strcat(bayes_file_name, ".bys");
		printf("Opening bayesian file: %s\n", bayes_file_name);
		bayes_file = fopen(bayes_file_name, "w");
		}

    /* open up communications file */
	strcpy(com_file_name, output_file);
	strcat(com_file_name, ".cmm");
	printf("Opening communications file: %s\n", com_file_name);
	com_fd = fopen(com_file_name, "w");

/* open up graphics file */
	strcpy(graphics_file_name, output_file);
	strcat(graphics_file_name, ".gra");
	printf("Opening ASCII graphics file: %s\n", graphics_file_name);
	graphics_file = fopen(graphics_file_name, "w");

	gotoxy(0, 15);
	printf("Please select infusion pump\n");
	printf("     1 = simulation (no pump)\n");
	printf("     2 = IMED C2 protocol\n");
	printf("     3 = Harvard Pump 22\n");
	printf("     4 = Grasby 3400\n");
	/*  printf("     5 = Ohmeda 9000\n");	*/
	/*	printf("     6 = Medfusion 2010A\n");	*/
	/*  printf("     7 = Welmed\n");	*/
	/*  printf("     8 = Baxter AS 50\n");	*/
	printf("     Your selection:");
	while (pump_type < 1 || pump_type > 4)
		{
		gotoxy(21, 20);
		printf("      ");
		gotoxy(21, 20);
		pump_type = (int) enter_real(0.0, 21, 20);
		}

	/* Experimental device notice displayed when connection is made over a */
	/* serial port.  This is to permit use of STANPUMP for research under  */
	/* IDE regulations, as I understand them. */
	if (pump_type > 1 && propofol_study_flag == 0 && 
		nausea_study_flag == 0 && lorazepam_study_flag == 0)
		{
		temp_pump_type = pump_type;		// needed to suppres closing of port
		pump_type = 0;                  // if escape here
		cls();
		gotoxy(0, 4);
		printf("When STANPUMP is used to directly control an infusion pump, it becomes\n");
		printf("a medical device.  As such, its use is experimental.  STANPUMP should be used\n");
		printf("to control an infusion pump only under the auspices of an appropriate\n");
		printf("institutional review board as part of an approved experimental protocol.\n");
		printf("\n\n\nPress ENTER to continue.\n");
		while (enter_key() == 0);
		pump_type = temp_pump_type;
		}

	cls();
	gotoxy(0, 4);
	printf("Pump type: %s\n", pump_name[pump_type]);

    /* open up pump port */
	initialize_pump();
	cls();

	gotoxy(0, 4);
	printf("Pump type: %s\n", pump_name[pump_type]);
	printf("Time increment: %d seconds\n", delta_seconds);
	printf("Study file: %s\n", output_file);

	printf("\nType '1' to start");
	if (pump_type == 1)
		printf("\n\nYou are in SIMULATION mode.");
	c = 0;
	seconds = -1;
	prior_absolute_time = 0;
	reference_time = absolute_clock();
	calculate_udfs();	/* calculate unit disposition functions */
	set_flags();

	if (drg_flag)
		track_drg();
	if (track_flag)
		track();
	if (reproduce_flag)
		reproduce_old_run();
	if (quick_flag == 0)
		{
		while (c != '1')
			{
			c = enter_key();
			if (seconds != absolute_clock())
				{
				gotoxy(0, 14);
				printf("%s", clock_time());
				seconds = absolute_clock();
				}
			}
		}
	prior_absolute_time = 0;
	reference_time = absolute_clock();
	fprintf(pkfile, "Starting time: %s", clock_time());
	}					/* start */

void terminate()               	/*  stops pump and gets summary for restart */
	{
	int n;
	desired = 0;
	prior_peak_time = peak_time;
	infuse();
	
	/* restore old interrupts */
	restore_keyboard();
	stop_pump();

	clear_prompt_area();
	gotoxy(0, 17);
	printf("Infusion terminated at user request.\n");
	if (hudson_flag == 0 && lorazepam_study_flag == 0)
		{
		printf("Total %s infused: %7.2lf  %s (%5.2lf mls)\n",
			drug_name[drug_id], amount_infused, infused_units,
			prior_working_volume_infused);
		}
	printf("Stanpump file saved in file %s.dat\n", output_file);
	for (n = 0;  n <= n_infs + 1;  n++)
		{
		if (bayes_data)
			fprintf(bayes_file, "%10.4lf %13.6lf\n", b_time[n], b_rate[n]);
		if (b_time[n + 1] < b_time[n])
			fprintf(drgfile, "%10.4lf %10.4lf %13.6lf\n", b_time[n], 9999.0, 0.0);
		else
			fprintf(drgfile, "%10.4lf %10.4lf %13.6lf\n", b_time[n], b_time[n + 1], b_rate[n]);
		}
	fprintf(drgfile, "9999 9999 0\n");
	fclose(pumpfile);
	fclose(drgfile);
	fclose(pkfile);
	fclose(errorfile);
	if (bayes_data)
		fclose(bayes_file);
	fclose (com_fd);
	fclose (graphics_file);
	gotoxy(0,19);
	exit(0);
	return;
	}					/*  terminate  */

void infuse()
	{					/* infuse */
	int interval;
	char harvard_string[25];
	double reported_volume_infused;
	double working_volume_infused;
	double delta_volume_infused;
	double delta_amount_infused;
	double pump_rate_in_amt;
	double l1, l2, l3, l4;
	double next_rate;

	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Entering infuse()\n");
		}
	/* Note: pump_rate_in_mls is in units of mls/hour          */
	/*       pump_rate_in_amt is in units of drug_units/second */

    /* check on pump status */
		infuse_time = relative_clock();
	next_time = infuse_time + delta_seconds;

    /* set interval since last visit to infuse() */
	if (prior_infuse_time == -1)
		interval = 0;	/* first visit only */
	else
		interval = (int) (infuse_time - prior_infuse_time);

	if (effect_flag > 0 && prior_peak_time > interval)
		prior_peak_time = prior_peak_time - interval;

	if (interval > 180)
		{
		fprintf(errorfile, "Interval = %ld at time %ld\n", interval, infuse_time);
		fflush(errorfile);
		interval = 180;
		}

	if (transport_flag == 0)
		check_pump_status();
		
	// Put in calculation for virtual drug 
	if (virtual_icu_drug > 0)
		{
		virtual_icu_drug = virtual_icu_drug * exp(-interval * 5.21667E-05);
		desired = virtual_real_sum - virtual_icu_drug * exp(-interval * 5.21667E-05);
		if (desired < 0)
			desired = 0;
		}

/* preliminary calculation of infusion rate */
	pump_rate_in_amt = pump_rate_in_mls / infusion_error_adjust * infusate_concentration / 3600.0;
	delta_volume_infused = (double) pump_rate_in_mls / infusion_error_adjust / 3600.0 * interval;

	if 	(
			(	effect_flag > 0  && 
				e_state[1] + e_state[2] + e_state[3] + e_state[4] < desired * .95 
			)
				||
			(	p_state[1] > 0 && 
				gibbs_flag == 1 && 
				zero_order_flag == 0 && 
				stalled_flag == 0 &&
				transport_flag == 0
			)
		)
		{
		if (debug_flag)
			{
			gotoxy(50, 4);
			printf("Calling process_rate()\n");
			}
		l1 = exp(-lambda[1] * interval);
		l2 = exp(-lambda[2] * interval);
		l3 = exp(-lambda[3] * interval);
		l4 = exp(-lambda[4] * interval);

		next_rate = model(
			p_state[1] * l1 + p_coef[1] * pump_rate_in_amt * (1 - l1),
			p_state[2] * l2 + p_coef[2] * pump_rate_in_amt * (1 - l2),
			p_state[3] * l3 + p_coef[3] * pump_rate_in_amt * (1 - l3),
			e_state[1] * l1 + e_coef[1] * pump_rate_in_amt * (1 - l1),
			e_state[2] * l2 + e_coef[2] * pump_rate_in_amt * (1 - l2),
			e_state[3] * l3 + e_coef[3] * pump_rate_in_amt * (1 - l3),
			e_state[4] * l4 + e_coef[4] * pump_rate_in_amt * (1 - l4));
		process_rate(next_rate);

		}

	/****************************************************************************/
	/* read and interpret volume infused 										*/
	/****************************************************************************/
	reported_volume_infused = read_volume_infused(delta_volume_infused);

	/* Check for loss of volume infused info by pump 				*/
	/* Specifically, to avoid runaway by Harvard Apparatus Pump 22 	*/
	/* when the volume infused gets reset to 0 (by Bovie??)			*/

	if (reported_volume_infused < prior_reported_volume_infused)
		{
		fprintf(errorfile, "Volume infused data lost at time %ld\n", infuse_time);
		fprintf(errorfile, "Reported volume infused: %16.8lf\n", reported_volume_infused);
		fprintf(errorfile, "Prior_reported_volume_infused: %16.8lf\n", 
				prior_reported_volume_infused);
		fflush(errorfile);
		write_rate(0.0);
		delta_volume_infused /= 2;
		reported_volume_infused = prior_reported_volume_infused + delta_volume_infused;
		prior_syringes = reported_volume_infused;
		switch(pump_type)
			{
		case 3: /* Harvard Pump 22 */
			/* recovery */
			warning = 2;
			display_pump_status();
			/* double entry for safety */
			while (run_flag == 1)
				write_harvard_22("stp");
			write_harvard_22("clv");
			while (atof(write_harvard_22("vol")) > 0.0)
				write_harvard_22("clv");
			prior_pump_volume = 0;
			/* retransmit syringe diameter */
			sprintf(harvard_string, "mmd %5.2lf", syringe_diameter[syringe_type]);
			write_harvard_22(harvard_string);

			/* double check on syringe size */
			while (fabs(atof(write_harvard_22("dia")) - syringe_diameter[syringe_type]) > .001)
				write_harvard_22(harvard_string);
			warning = 0;
			display_pump_status();
			break;
		case 4:
			warning = 2;
			display_pump_status();
			/* double entry for safety */
			while (run_flag == 1)
				write_graseby_3400( STOP_INFUSION, 0.0);
			write_graseby_3400(ZERO_TOTAL_DELIVERED);
			prior_pump_volume = 0;
			warning = 0;
			display_pump_status();
			break;
		default:
			desired = 0;
			prior_peak_time = peak_time;
			warning = 2;
			clear_prompt_area();
			display_pump_status();
			printf("\a");
			pump_type = 1;	/* force into simulation mode */
			display_prompts();	/* change to new prompts */
			}
		}
	working_volume_infused = prior_working_volume_infused + delta_volume_infused;

	/* is working_volume_infused too far ahead of pump? */
	if (working_volume_infused - reported_volume_infused > pump_precision[pump_type])
		{
		infusion_error_adjust *= 1.02; // gentle increase above 1
		working_volume_infused = reported_volume_infused + pump_precision[pump_type];
		}
	else
		/* or behind pump */
		{
		if (working_volume_infused < reported_volume_infused)
			{
			infusion_error_adjust /= 1.02; 
			working_volume_infused = reported_volume_infused;
			}
		}
	if (infusion_error_adjust > 2)
		infusion_error_adjust = 2;
	else
		{
		if (infusion_error_adjust < .5)
			infusion_error_adjust = .5;
		}

	// gotoxy(60, 10);
	// printf("%8.4f", infusion_error_adjust);
	
	amount_infused = working_volume_infused * infusate_concentration;
	delta_volume_infused = working_volume_infused - prior_working_volume_infused;
	delta_amount_infused = delta_volume_infused * infusate_concentration;

	syringe_volume -= delta_volume_infused;
	if (pending_bolus > 0)
		{
		pending_bolus -= delta_amount_infused;
		if (pending_bolus < 0)
			pending_bolus = 0;
		}
	/* write out to drg file an infusion for the interval */
	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Calling save_infusion()");
		}
	save_infusion(prior_infuse_time, infuse_time, delta_amount_infused);

	/********************************************************/
	/* bring model forward 									*/
	/********************************************************/
	if (interval == 0)
		{
		/* state_a1 += delta_amount_infused */
		}
	else
		{
		pump_rate_in_amt = delta_amount_infused / interval;
		l1 = exp(-lambda[1] * interval);
		l2 = exp(-lambda[2] * interval);
		l3 = exp(-lambda[3] * interval);
		l4 = exp(-lambda[4] * interval);
		p_state[1] = p_state[1] * l1 + p_coef[1] * pump_rate_in_amt * (1 - l1);
		p_state[2] = p_state[2] * l2 + p_coef[2] * pump_rate_in_amt * (1 - l2);
		p_state[3] = p_state[3] * l3 + p_coef[3] * pump_rate_in_amt * (1 - l3);
		if (effect_data)
			{
			e_state[1] = e_state[1] * l1 + e_coef[1] * pump_rate_in_amt * (1 - l1);
			e_state[2] = e_state[2] * l2 + e_coef[2] * pump_rate_in_amt * (1 - l2);
			e_state[3] = e_state[3] * l3 + e_coef[3] * pump_rate_in_amt * (1 - l3);
			e_state[4] = e_state[4] * l4 + e_coef[4] * pump_rate_in_amt * (1 - l4);
			}
		}

	if (transport_flag == 0)
		{
		fprintf(pumpfile, "3, %ld, %9.4lf\n", infuse_time, p_state[1] + p_state[2] + p_state[3]);
		if (effect_data)
			fprintf(pumpfile, "4, %ld, %9.4lf\n", infuse_time, e_state[1] + e_state[2] + e_state[3] + e_state[4]);
		if (effect_data == 0)
			fprintf(graphics_file, "%10.2f, %10s, %9.4lf\n", (double) infuse_time / 60.0, clock_time(), p_state[1] + p_state[2] + p_state[3]);
		else
			fprintf(graphics_file, "%10.2f, %10s, %9.4lf, %9.4lf\n", (double) infuse_time / 60.0, clock_time(), 
				p_state[1] + p_state[2] + p_state[3], e_state[1] + e_state[2] + e_state[3] + e_state[4]);
		fflush(pumpfile);
		}

    /* go to pk model, pump_rate is returned in units / second */
	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Calling process_rate()\n");
		}
	if (reproduce_flag)
		infusion_error_adjust = 1;

	if (zero_order_flag == 0)
		{
		next_rate = infusion_error_adjust * 
			model(p_state[1], p_state[2], p_state[3], 
					e_state[1], e_state[2], e_state[3], e_state[4]);
		}
	else
		{
		if (pending_bolus > 0)
			next_rate = pending_bolus / delta_seconds + pending_infusion;
		else
			next_rate = pending_infusion;
		}
	process_rate(next_rate);
	prior_working_volume_infused = working_volume_infused;
	prior_reported_volume_infused = reported_volume_infused;
	prior_infuse_time = infuse_time;
	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Calling display_update()\n");
		}
	display_update();
	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Calling display_rate()\n");
		}
	display_rate();
    /* tell look ahead routine to update state variables */
	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Calling look_ahead()\n");
		}
	look_ahead();
	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Exiting infuse()\n");
		}
	}					/* infuse */


void process_rate(pump_rate_in_amt)
double pump_rate_in_amt;	/* units/second */
	{					/* process_rate */
/* For medfusion
	int digits;
	double ln1, ln2;
*/
	if (debug_flag)
		{
		gotoxy(50, 12);
		printf("Entering process_rate()");
		gotoxy(50,13);
		printf("Pump rate: %f", pump_rate_in_amt * (double) 3600.0 / infusate_concentration);
		}
	if (pump_type == 3 || pump_type == 4 && transport_flag == 0)
		{
		if (syringe_volume < minimum_syringe_volume && purge_flag == 0)
			{
			/* safety redundancy */
			sys_status = 1;
			stalled_flag = 1;
			if (run_flag == 1)
			switch (pump_type)
				{
				case 3:
					write_harvard_22("stp");
					break;
				case 4:
					write_rate(0.0);
					break;
				}
			display_pump_status();
			display_prompts();
			}
		else
			if (syringe_volume < syringe_warning_volume)
				{
				warning = 1;
				}
		}

	pump_rate_in_mls = pump_rate_in_amt * 3600.0 / infusate_concentration;

	if (pump_rate_in_mls > maximum_rate)
		pump_rate_in_mls = maximum_rate;

    /* Adjust rate for pump limitations in range and precision */
    /* check on pump status */
	switch (pump_type)
		{
	case (1):
		{
		break;
		}
	case (2):	/* IMED C2 protocol */
		{
		if (pump_rate_in_mls > 999)
			pump_rate_in_mls = 999;
		pump_rate_in_mls = (double) ((int) (pump_rate_in_mls + 0.5));
		break;
		}
	case (3): /* Harvard Pump 22 */
		{
		if (pump_rate_in_mls > maximum_rate)
			pump_rate_in_mls = maximum_rate;
		else
			if (pump_rate_in_mls < minimum_rate)
				pump_rate_in_mls = 0;
		pump_rate_in_mls = (double) ((long) (pump_rate_in_mls * 1000 + 0.5)) / 1000;
		break;
		}
	case (4): /* Graseby 3400 */
		{
		if (pump_rate_in_mls > maximum_rate )
			pump_rate_in_mls = maximum_rate;
		pump_rate_in_mls = (double) ((int) (pump_rate_in_mls * 10 + 0.5)) / 10;
		break;
		}

/*
	case (?): ( Medfusion 2010 )
*/
/*
		{
		if (pump_rate_in_mls > maximum_rate)
			pump_rate_in_mls = maximum_rate;
		else
			if (pump_rate_in_mls < minimum_rate)
				pump_rate_in_mls = 0;
*/
		/* Round off to four significant digits */
/*
		ln1 = log(pump_rate_in_mls);
		ln2 = log(10.0);
*/		
		/* because of screwball (to me) implementation of (int) fn */
/*
		if (ln1 > 0)
			digits = (int) (ln1/ln2);
		else
			digits = (int) (ln1/ln2) - 1;
		pump_rate_in_mls = (double)((long)
			(pump_rate_in_mls * pow(10.0,3-digits)+.5)) / pow(10.0,3-digits);
		break;
		}
*/
	default:
		{
		}
		}

    /* instruct pump */
	if (comm_status || pump_status || sys_status || 
		transport_flag == 1 || purge_flag == 1 || stalled_flag == 1)
		{
		pump_rate_in_mls = 0.0;
		}
	else
		{
		write_rate(pump_rate_in_mls);
		}
	}					/* process_rate */

double model(temp1, temp2, temp3, temp1e, temp2e, temp3e, temp4e)
double temp1, temp2, temp3, temp1e, temp2e, temp3e, temp4e;
	{					/* model */
	double rate;		/* calculated pump rate           */
	int temp_peak;		/* temporary peak */
	double current;		/* used in effect mode calculation */
	double min_dif;		/* minimim acceptable difference */
	double result;
	int i;				/* used in loops */

	if (debug_flag)
		{
		gotoxy(50, 4);
		printf("Entering model()");
		}

	/* if tracking an old run, just return */
	if (reproduce_flag)
		{
		rate = reproduce_rate;
		if (rate > 0)
			return rate;
		else
			return 0;
		}

	if (effect_flag == 0 || fabs(desired - (temp1e + temp2e + temp3e + temp4e)) < desired * .05)
        /* even in effect mode, if we get very close, we switch and drive the plasma again */
        /* this is beneficial both for reasons of processing speed, and to reduce */
        /* the fluctuations seen in pure effect mode */
		{

		/* If first pass, it's easy */
		if (temp1 == 0)
			{
			if (debug_flag)
				{
				gotoxy(50, 5);
				printf("p_udf[%d] = %10.5lf", delta_seconds, p_udf[delta_seconds]);
				}
			return desired / p_udf[delta_seconds];
			}
		else
			{
	        /* Calculation as described in Bailey/Shafer IEEE */
			result = virtual_model(temp1, temp2, temp3, 0.0, (int) delta_seconds, 0);
			if (desired > result)
				return (desired - result) / p_udf[delta_seconds];
			else
				return 0.0;
			}
		}
	else
		{
        /* effect site rate calculation */
        /* first, calculate udf to peak_time */
		/* If first pass, it's easy */
		if (temp1 == 0)
			{
			prior_peak_time = peak_time;
			return desired / e_udf[peak_time];
			}

		/* zero out old effect site df */
		for (i = 0;  i <= peak_time + 1;  i++)
			eff_df[i] = 0;

		/* Should the pump be off? */
		if (virtual_model(temp1e, temp2e, temp3e, temp4e, delta_seconds, 1) > desired)
			return 0.0;
		/* minimim acceptable difference */
		if (pump_type == 1)
			min_dif = desired * .0000001;	/* simulation should be right on */
		else
			min_dif = desired * .001;	/* slightly looser for real hardware */

        /* Initial settings */
		temp_peak = prior_peak_time;
		if (temp_peak <= delta_seconds)
			temp_peak = delta_seconds + 1;
		rate = (desired - virtual_model(temp1e, temp2e, temp3e, temp4e, temp_peak, 1)) / e_udf[temp_peak];
		temp_peak = find_peak(temp_peak, rate, temp1e, temp2e, temp3e, temp4e);
		current = virtual_model(temp1e, temp2e, temp3e, temp4e, temp_peak, 1) + e_udf[temp_peak] * rate;

		/* Iterate until solution is found */
		while (fabs(current - desired) > min_dif)
			{
			rate = (desired - virtual_model(temp1e, temp2e, temp3e, temp4e, temp_peak, 1)) / e_udf[temp_peak];
			temp_peak = find_peak(temp_peak, rate, temp1e, temp2e, temp3e, temp4e);
			current = virtual_model(temp1e, temp2e, temp3e, temp4e, temp_peak, 1) + e_udf[temp_peak] * rate;
			}
		prior_peak_time = temp_peak;
		if (debug_flag)
			{
			gotoxy(50, 10);
			printf("Predicted peak_time: %2d", temp_peak);
			gotoxy(50, 11);
			printf("Predicted peak: %13.10lf", current);
			gotoxy(50, 12);
			printf("Predicted rate: %13.10lf", rate);
			}

		if (rate < .00001 && temp_peak < delta_seconds * 2)
			{
			next_time = next_time - delta_seconds + temp_peak;
			rate = 0;
			}
		}
	if (rate > 0)
		return rate;
	else
		return 0.0;
	}					/* model */

int find_peak(current_time, rate, temp1e, temp2e, temp3e, temp4e)
int current_time;
double rate;
double temp1e, temp2e, temp3e, temp4e;
	{					/* find_peak */
	double current;
	double earlier;
	double later;

	/* set up initial values */
	current = virtual_model(temp1e, temp2e, temp3e, temp4e, current_time, 1) + e_udf[current_time] * rate;
	earlier = virtual_model(temp1e, temp2e, temp3e, temp4e, current_time - 1, 1) + e_udf[current_time - 1] * rate;
	later = virtual_model(temp1e, temp2e, temp3e, temp4e, current_time + 1, 1) + e_udf[current_time + 1] * rate;
	while (current < earlier || current < later)
		{
		if (current < earlier)
			{
			if (current_time == delta_seconds)
					{
					return current_time;
					}
			current_time--;
			later = current;
			current = earlier;
			earlier = virtual_model(temp1e, temp2e, temp3e, temp4e, 
				current_time, 1) + e_udf[current_time] * rate;
			}
		else
			{
			current_time++;
			earlier = current;
			current = later;
			later = virtual_model(temp1e, temp2e, temp3e, temp4e, 
				current_time + 1, 1) + e_udf[current_time + 1] * rate;
			}
		}
	return current_time;
	}					/* find_peak */

double virtual_model(vm1, vm2, vm3, vm4, t, flag)
double vm1, vm2, vm3, vm4;
int t;
int flag;
	{					/* virtual_model */
	double temp;
	double vmf1, vmf2, vmf3, vmf4;
	if (flag && t > peak_time + 1)
		{
		fprintf(errorfile, "Time: %ld, t in virtual model = %d\n", relative_clock(), t);
		fprintf(errorfile, "peak time: %d\n", peak_time);
		return 0.0;
		}

	if (flag)
		{
		if (eff_df[t] > 0)
			{
			return eff_df[t];
			}
		}
	if ((lambda[1] * t) > 100.0)
		vmf1 = 0;
	else
		vmf1 = exp(-lambda[1] * t);

	if ((lambda[2] * t) > 100.0)
		vmf2 = 0;
	else
		vmf2 = exp(-lambda[2] * t);

	if ((lambda[3] * t) > 100.0)
		vmf3 = 0;
	else
		vmf3 = exp(-lambda[3] * t);

	if ((lambda[4] * t) > 100.0)
		vmf4 = 0;
	else
		vmf4 = exp(-lambda[4] * t);

	temp = vm1 * vmf1 + 
		vm2 * vmf2 + 
		vm3 * vmf3 +
		vm4 * vmf4;

	if (flag)
		eff_df[t] = temp;
	return temp;
	}					/* virtual_model */

void look_ahead()
	{					/* look_ahead */
	double a1, a2, a3, a4;	/* copies of state variables */
	long x0, x1;
	long x01;

	if (hudson_flag || lorazepam_study_flag)
		return;
	if (effect_flag > 0)
		{
		a1 = e_state[1];
		a2 = e_state[2];
		a3 = e_state[3];
		a4 = e_state[4];
		}
	else
		{
		a1 = p_state[1];
		a2 = p_state[2];
		a3 = p_state[3];
		a4 = 0.0;
		}

	if (a1 + a2 + a3 + a4 <= awake_concentration)
		{
		time_ahead = 0;
		display_awake();
		return;
		}
	if (virtual_model(a1, a2, a3, a4, (int) 30000, 0) > awake_concentration)
		{
		time_ahead = 30000;
		display_awake();
		return;
		}
	// binary search
	x0 = 0;
	x1 = 30000;
	while (labs(x0-x1) > 2)
		{
		x01 = (x0 + x1) / 2;
		if (virtual_model(a1, a2, a3, a4, (int) x01, 0) > awake_concentration)
			x0 = x01;
		else
			x1 = x01;
		}
	time_ahead = x0;
	display_awake();
	return;
	}					/* look_ahead */

/*********************************************************/
/* Handle Screen and Keyboard                            */
/*********************************************************/

int keyin()
	{					/*  keyin  */
	long time_now;
	int sim_delta;
	static long slow_sim_counter = 0;

	if (reproduce_flag == 1)
		reproduce_old_run();

	if (sim_speed == 1)
		{
		sim_delta = delta_seconds;
		time_now = relative_clock();
		if (slow_sim_counter <= 0)
			{
			if (track_flag > 0 && track_time > time_now && time_now + delta_seconds > track_time)
					sim_delta = (int) (track_time - time_now);
			if (next_time >= time_now && time_now + sim_delta >= next_time)
					sim_delta = (int) (next_time - time_now);
			simulate_clock(1);
			slow_sim_counter = slow_sim;
			}
		slow_sim_counter--;
		}

	time_now = relative_clock();	/* time since pump turned on */
	if (force_end > 0 && time_now > force_end)
		{
		terminate();
		}
	/* Are we following an old run? */
	if (seconds != time_now)		/* is it a new second? 		*/
		{
		seconds = time_now;			/* save the current seconds 	*/
		display_time(seconds);		/* tick clock on screen 	*/
		display_pump_status();		/* keep up with beeps */
		if ( pump_type == 4 )
			keep_graseby_pumping (pump_rate_in_mls); /* Periodic update needed */
		if (drg_flag == 2)
			{
			if (seconds >= drg_time)
				{
				pending_infusion = drg_rate;
				infusion_units = 1;
				display_infusion_units = 1;
				zero_order_flag = 1;
				infuse();
				drg_flag = 1;
				}
			}
		if (drg_flag == 1)
			track_drg();

		/* time for a rate change, if tracking a concentration plan? */
		if (track_flag == 2)
			{
			if (seconds >= track_time)
				{
				desired = track_concentration;
				prior_peak_time = peak_time;
				display_desired();
				infuse();
				track_flag = 1;
				}
			}
		else
			{
			if (track_flag == 1)
				track();
			}
        /* are we ready for an infusion update? */
		if (next_time == 0 || seconds >= next_time)
			{
			infuse();	/* update the pump and pk model */
			if (reproduce_flag == 2)
				reproduce_flag = 1;
			}
		}
    /*  now read in character (if any)  */
	return enter_key();	/* keyboard interrupt function & table translation */
	}					/*  keyin  */

void reproduce_old_run()
	{					/* reproduce_old_run */
	int flag;
	long time_point;
	double amount;
	char input_string[80];
	int fields;

	reproduce_flag = 2;
	flag = 0;

	gotoxy(68 - strlen(reproduce_filename),1);
	printf("Reproducing %s", reproduce_filename);
	fields = 0;
	while (fields < 3)
		{
		if (fscanf(reproduce_file, "%[^\n]\n", input_string) == EOF)
			{
			reproduce_flag = 0;
			gotoxy(68 - strlen(reproduce_filename),1);
			printf("Completed:  %s", reproduce_filename);
			return;
			}
		strip_white(input_string);
		fields = sscanf(input_string, "%d %ld %lf", &flag, &time_point, &amount);
		if (fields == 3)
			{
			gotoxy(62, 1+flag);
			printf("%d %ld %lf", flag, time_point, amount);
			switch (flag)
				{
				case (1):
					{
					reproduce_volume = amount;
					next_time = time_point;
					fields = 0; // Force another pass
					break;
					}
				case (2):
					{
					reproduce_rate = amount / (double) 3600.00 * infusate_concentration;
					break;
					}
				default:
					{
					fields = 0; //Force another pass
					}
				}
			}
		}
	}					/* reproduce_old_run */

void track()
	{					/* track */
	double temp;
	char input_string[80];
	int fields;

	gotoxy(71 - strlen(track_filename),1);
	printf("Tracking %s", track_filename);
	fields = 0;
	while (fields != 2)
		{
		if (fscanf(track_file, "%[^\n]\n", input_string) == EOF)
			{
			gotoxy(70 - strlen(track_filename),1);
			printf("Completed %s", track_filename);
			track_flag = 0;
			return;
			}
		strip_white(input_string);
		fields = sscanf(input_string, "%lf %lf", &temp, &track_concentration);
		}
	track_time = (long) (temp * 60.0+0.5);	/* convert to seconds */
	track_flag = 2;
	gotoxy(66,2);
	printf("%6.2f %6.2f", temp, track_concentration);
	}					/* track */
	
void track_drg()
	{					/* track_drg */
	double temp1, temp2, temp3;
	static double time_off = 0;
	char input_string[80];
	int fields;

	
	gotoxy(71 - strlen(drg_input_filename),1);
	printf("drg file %s", drg_input_filename);
	zero_order_flag = 1;
	infusion_units = 1;

	// Get next line
	fields = 0;
	while (fields <2)
		{
		if (fscanf(drg_input_file, "%[^\n]\n", input_string) == EOF)
			{
			gotoxy(70 - strlen(drg_input_filename),1);
			printf("Completed %s", drg_input_filename);
			drg_flag = 0;
			return;
			}
		strip_white(input_string);
		fields = sscanf(input_string, "%lf %lf %lf", &temp1, &temp2, &temp3);
		switch(fields)
			{
			case 2:
				if (temp1 == 9999) // Read past bolus line in DRG file
					fields = 0;
				drg_rate = temp2;
				break;       
			case 3:
				if (temp1 == 9999 || temp1 == 99999 ||
				temp2 == 9999 || temp2 == 99999)
					{
					temp1 = time_off;
					drg_rate = 0;
					}
				else
					{
					drg_rate = temp3;
					time_off = temp2;
				    }
				break;
			default:
				fields = 0;
			}
				
		}

	drg_time = (long) (temp1 * 60.0+0.5);	/* convert to seconds */
	drg_rate = drg_rate / 60.0;	/* convert to units/second */
	drg_flag = 2;
	gotoxy(66,2);
	printf("%6.2f %6.2f", temp1, drg_rate * 60.0);
	}					/* track_drg */


void display_pump_status()
	{					/*  display_pump_status  */
	static long prior_beep = 0;
	static long prior_write = 0;
	static long prior_toggle = 0;
	static int graseby_toggle = 0;
	long now;

	static char *comm_status_codes[] = 
	/* 0  */       {"OK                                     ",
	/* 1  */        "No response: check pump power and cable",
	/* 2  */		"Checksum error                         ",
	/* 3  */        "String protocol error                  ",
	/* 4  */        "Communications error                   "};

	static char *pump_status_codes[] = 
	/* 0  */       {"OK                                     ",
	/* 1  */        "Press computer enable                  ",
	/* 2  */        "Low battery                            ",
	/* 3  */        "Line occluded                          ",
	/* 4  */        "Air in line                            ",
	/* 5  */        "Door open                              ",
	/* 6  */        "User time out                          ",
	/* 7  */        "Computer time out                      ",
	/* 8  */        "Empty container detector removed       ",
	/* 9  */        "Check infusion set                     ",
	/* 10 */        "Invalid infusion rate                  ",
	/* 11 */        "Invalid pump status                    ",
	/* 12 */        "Syringe empty.  Please change now.     ",
	/* 13 */        "Stalled: check syringe & tubing        ",
	/* 14 */		"Check and clear alarm on Graseby pump  ",
	/* 15 */		"Use F7 to reset maximum Graseby rate   ",
	/* 16 */		"Reseat syringe in pump and check clamp ",
	/* 17 */        "PRESS STOP BUTTON ON GRASEBY PUMP NOW  ",
	/* 18 */        "******LOSS OF COMPUTER CONTROL******   ",
	};               

	static char *sys_status_codes[] = 
	/* 0  */       {"OK                                     ",
	/* 1  */        "Syringe empty.  Please change now.     ",
	/* 2  */        "Pump paused: no serial communication   "};

	static char *warning_codes[] = 
	/* 0  */       {"OK                                     ",
	/* 1  */        "Syringe volume low, please change soon ",
	/* 2  */		"Loss of volume data, repair in progress"};

	// Priority: 
	// comm errors > pump errors > system status > warnings
	// Note that ALL status codes have the effect of shutting
	// down the pump.  Warning codes do not shut down the pump

	if (purge_flag == 1 && comm_status == 0)
		{
		gotoxy(13, 24);
		printf("Change syringe, purge line dialog            ");
		gotoxy(atx, aty);
		return;
		}

	now = absolute_clock();

	/* Comm errors */
	if (comm_status)
		{
		gotoxy(13, 24);
		printf("%s",comm_status_codes[comm_status]);
		if (now != prior_write)
			{
			gotoxy(atx, aty);
			fprintf(errorfile, "%s [%ld]: Communications error: %s\n", 
			clock_time(), relative_clock(), comm_status_codes[comm_status]);
			prior_write = now;
			}
		if (prior_beep > now || prior_beep + 2 < now)
			{
			printf("\a");
			prior_beep = now;
			}
		return;
		}

	/* Pump errors (reported by pump) */
	if (pump_status)
		{
		gotoxy(13, 24);

		if (pump_status == 14 || pump_status == 17)
			{
			if (now > prior_toggle + 2)
				{
				graseby_toggle = 1 - graseby_toggle;
				prior_toggle = now;
				}
			}
		else
			graseby_toggle = 0;

		printf("%s",pump_status_codes[pump_status + graseby_toggle]);
		gotoxy(atx, aty);
		if (now != prior_write)
			{
			fprintf(errorfile, "%s [%ld]: Pump error: %s\n", 
				clock_time(), relative_clock(), pump_status_codes[pump_status]);
			prior_write = now;
			}
		if (pump_status == 17 || prior_beep > now || prior_beep + 6 < now)
			{
			printf("\a");
			prior_beep = now;
			}
		return;
		}

	/* system messages (include syringe empty if calculated by computer) */
	if (sys_status)
		{
		gotoxy(13, 24);
		printf("%s",sys_status_codes[sys_status]);
		gotoxy(atx, aty);
		if (now != prior_write)
			{
			fprintf(errorfile, "%s [%ld]: System status: %s\n", 
				clock_time(), relative_clock(), sys_status_codes[sys_status]);
			prior_write = now;
			}
		if ((sys_status == 1) && (prior_beep > now || prior_beep + 2 < now) )
			{
			printf("\a");
			prior_beep = now;
			}
		return;
		}

	if (warning)
		{
		gotoxy(13, 24);
		printf("%s", warning_codes[warning]);
		gotoxy(atx, aty);
		if (now != prior_write)
			{
			fprintf(errorfile, "%s [%ld]: Warning: %s\n", 
				clock_time(), relative_clock(), warning_codes[warning]);
			prior_write = now;
			}
		if (prior_beep > now || prior_beep + 10 < now)
			{
			printf("\a");
			prior_beep = now;
			}
	    return;
		}
	gotoxy(13, 24);
	printf("%s", pump_status_codes[0]);
	gotoxy(atx, aty);
	return;
	}					/* display_pump_status  */

void display_init()
	{					/* display_init */
	char temp_char[80];
	cls();
	gotoxy(0, 0);
	if (hudson_flag)
		printf("Hudson Randomized Alfentanil / Sufentanil Study");
	else
		{
		if (propofol_study_flag == 0)
			printf("Computer Controlled Infusion Pump.  Revision: %s", revision_date);
		else
			printf("ICI Multicenter Clinical Trial.     Revision: %s", revision_date);
		}
	gotoxy(80 - strlen(pump_name[pump_type]), 0);
	printf("%s", pump_name[pump_type]);
	gotoxy(0, 1);
	if (hudson_flag == 0)
		{
		if (lorazepam_study_flag == 0)
			printf("Drug: %s    Parameters: %s", drug_name[drug_id], parameter_name);
		else
			printf("Roche Multicenter ICU trial: Patient randomized to midazolam or lorazepam");
		gotoxy(0, 6);
		printf("Location      Units      Predicted       Target");
		if (virtual_icu_drug > 0)
			{
			gotoxy(54,6);
			printf("Virtual      Sum");
			}
		}
	gotoxy(0, 3);
	printf("Current time:   %s", clock_time());
	gotoxy(0, 4);
	printf("Elapsed time:     0 minutes  0 seconds");
	if (force_end > 0)
		{
		gotoxy (5, 5);
		printf("<<Forced end after %ld minutes>>", force_end / 60);
		}
	gotoxy(0, 7);
	if (hudson_flag)
		printf("Desired concentration:   %4.0f %% of reference", desired / ref_con * 100.);
	else
		printf("Plasma        %s/ml", conc_units);
	gotoxy(0, 8);
	if (hudson_flag)
		printf("Predicted concentration:   0  %% of reference");
	else
		printf("Effect Site   %s/ml", conc_units);
	if (pd_data)
		{
		strcpy(temp_char, effect_text);
		temp_char[0] = toupper(temp_char[0]);
		gotoxy(0, 9);
		printf("%s", temp_char);
		gotoxy(14, 9);
		printf("%s", effect_units);
		}
	display_update();
	init_totals_display();
	display_prompts();
	gotoxy(0, 24);
	printf("Pump status: OK");
	}					/* display_init */

void init_totals_display()
	{
	if (hudson_flag == 0)
		{
		if (lorazepam_study_flag == 0)
			{
			gotoxy(0, 11);
			printf("Total infused:                     %s           %s/kg",
				infused_units, infused_units);
			}
		gotoxy(0, 12);
		printf("Total infused:                     mls");
		if (lorazepam_study_flag == 0)
			{
			gotoxy(0, 14);
			printf("Pump rate:          ml/hr,          %s/kg/hr", infused_units);
			}
		}
	display_update();
	}

void display_awake()
	{					/* display_awake */
	char temp_char[80];
	if (purge_flag == 0 && lorazepam_study_flag == 0)
		{
		gotoxy(0, 15);
		if (effect_flag < 2)
			{
			printf("%s level of %6.2lf expected in ",
				site[effect_flag], awake_concentration);
			}
		else
			{
			strcpy(temp_char, effect_text);
			temp_char[0] = toupper(temp_char[0]);
			printf("%s of %6.2f %s, expected in ",temp_char, pd_model(awake_concentration), effect_units);
			}
		if (time_ahead >= 30000)
			printf(">500 minutes       ");
		else
			printf("%5.1lf minutes      ", (double) time_ahead / 60.0);
		gotoxy(atx, aty);
		}
	}					/* display_awake */

void display_prompts()
	{					/* display_prompts */
	int line;		// line of current prompt
	// 40 character length mask
	// 1234567890123456789012345678901234567890
	gotoxy(0, 16);
    printf("---------------------------------- Functions -----------------------------------");
	gotoxy(0, 17);
	if (transport_flag == 0 && sys_status != 2)
		{
		if (zero_order_flag == 0)
			{
			if (effect_flag < 2)
				{
				if (propofol_prompts_flag == 1 && drug_id == 8)
						//  1234567890123456789012345678901234567890
					printf("F1:  raise or lower the propofol level  \n");
				else
						//  1234567890123456789012345678901234567890
					printf("F1:  change target concentration.       \n");
				}
			else
					//  1234567890123456789012345678901234567890
				printf("F1:  change target %s.                  \n", effect_text);
			}
		else
				//  1234567890123456789012345678901234567890
			printf("F1:  enter a bolus or infusion.         \n");
		}
	if (propofol_study_phase != 2 && nausea_study_flag == 0 && lorazepam_study_flag == 0)
		if (effect_flag < 2)
				//  1234567890123456789012345678901234567890
			printf("F2:  change look-ahead concentration.   \n");
		else
			{
			printf("                                        \r");
			printf("F2:  change look-ahead %s %s.\n", 
				effect_text, effect_units);
			}
	if (pump_type == 3 || pump_type == 4)
		{
		if (stalled_flag == 0)
				//  1234567890123456789012345678901234567890
			printf("F3:  change syringe.                    \n");
		else
			printf("F3:  resume drug infusion.              \n");
		}
	if (bayes_data)
		{
		if (bayes_processing_flag == 0)
			{
			if (pd_data == 0)
					//  1234567890123456789012345678901234567890
				printf("F4:  enter measured %s level.  \n", drug_name[drug_id]);
			else
				printf("F4:  enter observed %s.        \n", effect_text);
			}
		else
				//  1234567890123456789012345678901234567890
			printf("F4:  abort bayesian update.             \n");
		}
	if (pump_type == 1)
		{
		if (sim_speed == 1)
				//  1234567890123456789012345678901234567890
			printf("F5:  simulate in real time.             \n");
		else
				//  1234567890123456789012345678901234567890
			printf("F5:  simulate as fast as possible.      \n");
		}
	// Start second column
	line = 17;
	gotoxy(40, line);
	if (zero_order_flag)
		{
			//  1234567890123456789012345678901234567890
		printf("F6:  select target mode.                ");
		line++;
		}
	else
		{
		if (no_zero_order_flag == 0)
			{
				//  1234567890123456789012345678901234567890
			printf("F6:  select constant rate mode.         ");
			line++;
			}
		}
	gotoxy (40, line);
	if (zero_order_flag == 0 && effect_data && plasma_only_flag == 0)
		{
		switch(effect_flag)
			{
		case (0):
			{
				//  1234567890123456789012345678901234567890
			printf("F8:  target effect site concentration.  ");
			break;
			}
		case (1):
			{
			if (pd_data)
					//  1234567890123456789012345678901234567890
				printf("F8:  target the drug effect.            ");
			else
				printf("F8:  target the plasma concentration.   ");
			break;
			}
		case (2):
			{
				//  1234567890123456789012345678901234567890
			printf("F8:  target the plasma concentration.   ");
			break;
			}
			}
		line++;
		}
	gotoxy(40, line);
	if (pump_type > 1)
		{
		if (transport_flag == 0)
				//  1234567890123456789012345678901234567890
			printf("F9:  pause infusion.                    ");
		else
			printf("F9:  resume infusion.                   ");
		line++;
		}
	gotoxy(40, line);
		//  1234567890123456789012345678901234567890
	printf("F10: terminate infusion at end of study.");
	gotoxy(atx, aty);
	}					/* display_prompts */

void clear_prompt_area()
	{					/* clear_prompt_area */
	int i;
	gotoxy(0, 17);
	for (i = 17;  i < 24;  i++)
		{
		gotoxy(0, i);
			//  12345678901234567890123456789012345678901234567890123456789012345678901234567890
		printf("                                                                                ");
		}
	gotoxy(atx, aty);
	}					/* clear_prompt_area */

void display_update()
	{					/*  display_update  */
	display_desired();
	gotoxy(25, 8);
	if (hudson_flag)
		printf("%4.0lf", (p_state[1] + p_state[2] + p_state[3]) / ref_con * 100.0);
	else
		{
		gotoxy(27, 7);
		printf("%7.2lf", p_state[1] + p_state[2] + p_state[3]);
		if (virtual_icu_drug > 0)
			{
			gotoxy(54,7);
			printf("%7.2f  %7.2f", virtual_icu_drug, virtual_icu_drug + p_state[1] + p_state[2] + p_state[3]);
			}
		gotoxy(27, 8);
		if (effect_data)
			printf("%7.2lf", e_state[1] + e_state[2] + e_state[3] + e_state[4]);
		else
			printf("no data");
		if (pd_data)
			{
			gotoxy(27, 9);
			printf("%7.2lf", pd_model(e_state[1] + e_state[2] + e_state[3] + e_state[4]));
			}
		if (purge_flag == 0)	/* otherwise, interferes with syringe dialog */
			{
			if (lorazepam_study_flag == 0)
				{
				gotoxy(24, 11);
				printf("%10.3lf", amount_infused);
				gotoxy(38, 11);
				printf("%9.3lf", amount_infused / mass);
				}
			gotoxy(25, 12);
			printf("%9.3lf", prior_working_volume_infused);
			}
		}
	gotoxy(atx, aty);
	}					/*  display_update  */

void display_time(secs)
long secs;
	{					/* display_time */
	gotoxy(16, 3);
	printf("%s", clock_time());
	gotoxy(13, 4);
	printf("%6ld", secs / 60L);
	gotoxy(28, 4);
	printf("%2ld", secs % 60L);
	gotoxy(atx, aty);
	}					/* display_time */

void display_desired()
	{					/* display desired */
	if (zero_order_flag == 1)
		{
		gotoxy(40, 7);
		printf("   none");
		gotoxy(40, 8);
		printf("       ");
		gotoxy(40, 9);
		printf("       ");
		}
	else
		{
		if (hudson_flag)
			{
			gotoxy(25, 7);
			printf("%4.0lf", desired / ref_con * 100.0);
			}
		else
			{
			switch(effect_flag)
				{
			case(0):
				{
				gotoxy(40, 7);
				printf("%7.2lf", desired);
				gotoxy(40, 8);
				printf("        ");
				gotoxy(40, 9);
				printf("        ");
				break;
				}
			case(1):
				{
				gotoxy(40, 7);
				printf("        ");
				gotoxy(40, 8);
				printf("%7.2lf", desired);
				gotoxy(40, 9);
				printf("        ");
				break;
				}
			case(2):
				{
				gotoxy(40, 7);
				printf("        ");
				gotoxy(40, 8);
				printf("        ");
				gotoxy(40,9);
				printf("%7.2lf", desired_effect);
				break;
				}
				}
			}
		gotoxy(atx, aty);
		}
	}					/* display desired */

void display_rate()
	{					/* display rate */
	double temp;
	if (hudson_flag)
		return;
	if (purge_flag == 0 && lorazepam_study_flag == 0)	/* otherwise, interferes with syringe dialog */
		{
		temp = pump_rate_in_mls * infusate_concentration;
		gotoxy(11, 14);
		printf("%8.3lf", pump_rate_in_mls);
		gotoxy(26, 14);
		switch(display_infusion_units)
			{
		case(1):
			{
			printf("%9.2lf (%ss/min)      ", temp / 60.0, infused_units);
			break;
			}
		case(2):
			{
			printf("%9.2lf (%ss/hour)      ",temp, infused_units);
			break;
			}
		case(3):
			{
			printf("%9.2lf (%ss/kg/min)   ", temp / 60.0 / mass, infused_units);
			break;
			}
		case(4):
			{
			printf("%9.2lf (%ss/kg/min)   ", temp / 60.0 / mass * 1000, conc_units);
			break;
			}
		case(5):
			{
			printf("%9.2lf (%ss/kg/hour)   ", temp / mass, infused_units);
			break;
			}
			}
		}
	}					/* display rate */

void gotoxy(x, y)
int x, y;
	{					/* gotoxy */
	REGS ir, or;
	ir.h.dh = y;
	ir.h.dl = x;
	ir.h.ah = 2;
	ir.h.bh = 0;
	int86(0x10, &ir, &or);
	}					/* gotoxy */

void cls()
	{					/* clear screen */
	REGS ir, or;
	ir.x.dx = 0;
	ir.h.ah = 15;
	int86(0x10, &ir, &or);
	ir.h.al = or.h.al;
	ir.h.ah = 0;
	int86(0x10, &ir, &or);
	if (started_flag == 0)
		{
		gotoxy(0, 0);
		if (hudson_flag)
			printf("Hudson Randomized Alfentanil / Sufentanil Study");
		else
			{
			if (propofol_study_flag == 1)
				printf("ICI/Stuart Multicenter Propofol Trial");
			else
				printf("Computer Controlled Infusion Pump");
			}
		printf("\nRevision date: %s", revision_date);
		gotoxy(60, 0);
		printf("ESC (escape) to exit");
		}
	}					/* clear screen */

/************************************************************/
/* clock primitives - read relative_clock, read time of day */
/************************************************************/

long relative_clock()
	{					/* relative_clock */
	return (absolute_clock() - reference_time);
	}					/* relative_clock */

long absolute_clock()
	{					/* absolute_clock */
	REGS ir, or;
	long time;
	static long prior_days = 0;
	ir.h.ah = 44;
	int86(0x21, &ir, &or);
	time = (long) or.h.ch * 3600L + (long) or.h.cl * 60L + (long) or.h.dh;
	time = time + prior_days;
	if (time < prior_absolute_time)
		{
		time += 86400L;
		prior_days += 86400L;
		}
	prior_absolute_time = time;
	return time;
	}					/* absolute_clock */

void simulate_clock(d)
int d;
	{
	reference_time -= d;
	}

void toggle_sim_speed()
	{					/* toggle sim_speed() */
	if (sim_speed == 1)
		{				/* return to real time */
		sim_speed = 0;
		}
	else
		{
		sim_speed = 1;
		}
	}					/* toggle sim_speed() */
	
char *clock_time()
	{					/* time of day */
	long clock;
	int hours;
	int minutes;
	int secs;
	int days;
	static char timestring[50];
	strcpy(timestring, "Day: 00 Time: 00:00:00");
	/* clock = absolute_clock() - reference_time; */
	days = (int) (absolute_clock() / 86400L) + 1;
	if (reference_time < 0)
		days += (int) (-reference_time / 86400L) + 1;
	clock = absolute_clock();
	clock %= 86400L;
	hours = (int) (clock / 3600L);
	clock %= 3600L;
	minutes = (int) (clock / 60L);
	clock %= 60L;
	secs = (int) (clock);
	timestring[5] = days / 10 + '0';
	timestring[6] = days % 10 + '0';
	timestring[14] = hours / 10 + '0';
	timestring[15] = hours % 10 + '0';
	timestring[17] = minutes / 10 + '0';
	timestring[18] = minutes % 10 + '0';
	timestring[20] = secs / 10 + '0';
	timestring[21] = secs % 10 + '0';
	return &timestring[0];
	}					/* time of day */

/*****************************************************/
/* Utility Routines: enter_real,  etc.               */
/*****************************************************/

double enter_real(r, x, y)
double r;
int x;
int y;
	{					/*  enter_real  */
	char s[9];
	int cint, i, imax;
	double factor;
	int dpflag;
	int c;
	long now;
	
	i = -1;
	imax = -1;
	do	{
		if (started_flag == 1)
			c = keyin();
		else
			c = enter_key();
		gotoxy(x, y);
		if (graseby_max_rate_flag == 1)
			{
			if (now != absolute_clock())
				{
				write_graseby_3400( START_INFUSION, 0.0 );
				now = absolute_clock();
                }
            }
		switch (c)
			{
		case ('0'):
		case ('1'):
		case ('2'):
		case ('3'):
		case ('4'):
		case ('5'):
		case ('6'):
		case ('7'):
		case ('8'):
		case ('9'):
		case ('.'):
			{
			s[++i] = c;
			putchar(c);
			x++;
			if (i > imax)
				imax = i;
			break;
			}

		case (8):
			{
			if (i >= 0)
				{
				if (i == imax)
					imax--;
				i--;
				putchar(c);
				putchar(' ');
				putchar(c);
				x--;
				}
			break;
			}
		case (' '):
		case ('\r'):
		case ('\n'):
			{
			i = 99;
			break;
			}
		default:
			{
			}
			}
		}
	while (i < 5);
	i = 0;
	factor = 0.1;
	dpflag = 0;
	c = 0;
	if (imax >= 0)
		{
		r = 0;
		do	{
			c = s[i++];
			cint = c - 48;
			if (c == '.')
				dpflag = 1;
			else if (dpflag || (c == '.'))
				{
				dpflag = 1;
				r = r + factor * cint;
				factor = factor / 10;
				}
			else
				r = r * 10.0 + cint;
			}
		while (i <= imax);
		}
	return r;
	}					/*  enter_real  */

int enter_key()
	{					/* enter_key */
	REGS ir, or;
	ir.h.ah = 11;
	int86(0x21, &ir, &or);
	if (or.h.al == 0)
		return 0;
	ir.h.ah = 6;
	ir.h.dl = 255;
	int86(0x21, &ir, &or);
	switch (or.h.al)
		{
	case 0:
		{
		int86(0x21, &ir, &or);
		switch (or.h.al)
			{
		case (59):		/* F1 */
			return 'c';
		case (60):		/* F2 */
			return 'n';
		case (61):		/* F3 */
			return 'r';
		case (62):		/* F4 */
			return 'b';
		case (63):		/* F5 */
			return 's';
		case (64):		/* F6 */
			return 'z';         
		case (65):      /* F7 */
			return 'm'; 
		case (66):		/* F8 */
			return 'p';
		case (67):		/* F9 */
			return 't';
		case (68):		/* F10 */
			return 'q';
		case (79):		/* END */
			return 'e';
		default:
			return 0;
			}
		}
	case ('Y'):
	case ('y'):
	case ('N'):
	case ('n'):
		if (yesno_flag == 1)
			return or.h.al;
		else
			return 0;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case '.':
	case 8:
	case 13:
		{
		return or.h.al;
		}
	case (0x1b): /* esc */
		{
		if (started_flag == 0)
			{
			cls();
			gotoxy(0, 2);
			printf("STANPUMP terminated at user's request\n");
			restore_keyboard();
			stop_pump();
			gotoxy(0,18);
			exit(0);
			}
		}
	default:
		{
		}
		}
	return 0;
	}					/* enter_key */

char *enter_string(x, y)
int x;
int y;
	{					/*  enter_string  */
	REGS ir, or;
	static char s[80];
	int i, imax;
	int c;
	
	i = -1;
	imax = -1;
	do	{
		ir.h.ah = 11;
		int86(0x21, &ir, &or);
		if (or.h.al == 0)
			c = 0;
		else
			{
			ir.h.ah = 6;
			ir.h.dl = 255;
			int86(0x21, &ir, &or);
			c = or.h.al;
			}
		if (c == 0x1b) /* esc */
			{
			if (started_flag == 0)
				{
				cls();
				gotoxy(0, 2);
				printf("STANPUMP terminated at user's request\n");
				restore_keyboard();
				stop_pump();
				gotoxy(0, 18);
				exit(0);
				}
			}
		switch(c)
			{			
		case 0:
			break;
		case (8):
			if (i >= 0)
				{
				if (i == imax)
					imax--;
				i--;
				putchar(c);
				putchar(' ');
				putchar(c);
				x--;
				}
			break;
		case ('\r'):
		case ('\n'):
			s[++i] = 0;
			i = 99;
			break;
		case (' '):
    		gotoxy(x, y);
			s[++i] = c;
			putchar(c);
			x++;
			if (i > imax)
				imax = i;
			break;
		default:
			if (isalnum(c) || c > 'z')
				{
        		gotoxy(x, y);
				s[++i] = c;
				putchar(c);
				x++;
				if (i > imax)
					imax = i;
				break;
				}
			}
		} while (i < 30);
	return s;
	}					/*  enter_string  */


/***********************************************************************/
/* Added routines for multiple kinetics, command line arguments, and   */
/* Randomization of administered drug                                  */
/***********************************************************************/

void initialize_globals()
	{					/* initialize_globals */
	subject[0] = 0;
	user[0] = 0;
	pump_rate_in_mls = 0;
	next_time = 0;
	amount_infused = 0;
	prior_working_volume_infused = 0;
	prior_reported_volume_infused = 0;
	prior_pump_volume = 0;
	seconds = 0;
	force_end = 0;
	warning = 0;
	syringe_type = 0;
	syringe_volume = 0;
	prior_syringes = 0;
	effect_data = 1;
	pd_data = 0;
	mass = 0;
	height = 0;
	age = 0;
	gender = 0;
	desired = -1;
	desired_effect = 0;
	sim_speed = 0;
	slow_sim = 0;
	output_file[0] = 0;
	kinetic_file[0] = 0;
	delta_seconds = 10;
	infusate_concentration = 0;
	prior_infuse_time = -1;
	strcpy(species, "Patient");
	strcpy(conc_units, "ng");
	strcpy(infused_units, "ug");
	com_port = 0;
	compression_factor = 0.05;	/* 5% initial compression */
	bayes_data = 0;		/* will usually default to 0 */
	n_measure = 0;
	n_infs = 0;
	p_state[1] = 0;
	p_state[2] = 0;
	p_state[3] = 0;
	e_state[1] = 0;
	e_state[2] = 0;
	e_state[3] = 0;
	e_state[4] = 0;
	t_peak = 0;
	heart_failure = 2;
	purge_flag = 0;
	hudson_flag = 0;
	transport_flag = 0;
	zero_order_flag = -1;	/* 0 = target, 1 = bolus/infusion */
	C2_sim_flag = 0;
	reproduce_flag = 0;
	gibbs_flag = 0;
	prior_flag = 0;
	delete_flag = 0;
	quick_flag = 0;
	virtual_icu_drug = 0;
	virtual_real_sum;
	bayes_processing_flag = 0;
	no_zero_order_flag = 0;
	plasma_only_flag = 0;
	propofol_prompts_flag = 1;
	if (propofol_study_flag == 0)
		{
		effect_flag = 0;
		drug_id = 0;
		kinetic_set = 0;
		pump_type = 0;
		one_syringe_type = 0;
		}
	else
		{
		drug_id = 0;
		kinetic_set = 0;
		effect_flag = 1;
		}
	}					/* initialize_globals */

void set_flags()
	{
	if (propofol_study_flag == 1 && propofol_study_phase == 2)
		{
		no_zero_order_flag = 1;
		one_syringe_type = 1;
		plasma_only_flag = 1;
		propofol_prompts_flag = 1;
		}
	if (nausea_study_flag == 1)
		{
		one_syringe_type = 1;
		plasma_only_flag = 1;
		no_zero_order_flag = 1;
		propofol_prompts_flag = 1;
		}
	if (lorazepam_study_flag == 1)
		{
		one_syringe_type = 1;
		plasma_only_flag = 1;
		no_zero_order_flag = 1;
		}
	if (effect_data == 0)
		plasma_only_flag == 1;
	}				

void opening_screen()
	{
	cls();                      
	gotoxy(0,3);
	printf("     Welcome to STANPUMP, a program for Computer Controlled Drug           \n");
	printf("     Administration and pharmacokinetic simulation.  Stanpump was written  \n");
	printf("     by S. Shafer.  The bayesian routines were developed by P. Maitre.     \n");
	printf("     The contributions of K. Gregg, J. Cheng, H. Schwilden, J. Schuttler,  \n");
	printf("     J. Reves, J. Jacobs, and P. Glass, are gratefully acknowledged.       \n");
	gotoxy(42, 21);
	printf("Stanpump is copyrighted by S. Shafer,");
	gotoxy(42, 22);
	printf("1986-1995.  Portions of STANPUMP are");
	gotoxy(42, 23);
	printf("are copyrighted by Microsoft, Inc., ");
	gotoxy(42, 24);
	printf("and by Greenleaf, Inc.");
    
    gotoxy(0, 10);
    printf("Subject identifier: ");
	if (subject[0] == 0)
	    strcpy(subject, enter_string(20, 10));
	else
		printf("%s", subject);
	
	while (mass < .01 || mass > 500)
		{
		gotoxy(0, 11);
		printf("Weight (in kg):       ");
		mass = enter_real(0.0, 16, 11);
		}
	gotoxy(0, 11);
	printf("Weight (in kg): %6.3f    ", mass);
	
	while (height < .01 || height > 250)
		{
		gotoxy(0, 12);
		printf("Height (in cm):       ");
		height = enter_real(0.0, 16, 12);
		}
	gotoxy(0, 12);
	printf("Height (in cm): %6.2f    ", height);

	while (age < .1 || age > 130)
		{
		gotoxy(0, 13);
		printf("Age (in years):        ");
		age = enter_real(0.0, 16, 13);
		}
	gotoxy(0, 13);
	printf("Age (in years): %4.1f    ", age);
	
	while (gender != 1 && gender != 2)
		{
		gotoxy(0, 14);
		printf("Gender (1 = male, 2 = female):    ");
		gender = (int) enter_real(0.0, 31, 14);
		}
	gotoxy(0, 14);
	printf("Gender (1 = male, 2 = female): %d   ", gender);
		
	bsa = pow(mass, 0.425) * pow(height, 0.725) * 0.007184;
	if (gender == 1)
		lbm = 1.1 * mass - 128 * (mass/height) * (mass/height);
	else
		lbm = 1.07 * mass - 148 * (mass/height) * (mass/height);

	gotoxy(0, 16);
	printf("Body surface area = %6.3f\n", bsa);
	printf("Lean body mass (adults) = %5.2f\n", lbm);
	
    gotoxy(0, 20);
    printf("Who are you? ");
	if (user[0] == 0)
	    strcpy(user, enter_string(13, 20));
	else
		printf("%s", user);
    }


/* set up the kinetic parameters                        */
/* this is the routine which specifies the kinetics,    */
/* the drug concentrations, max levels,                 */
/* the maximum infusion rates, default wake-up Cp, etc. */

void define_kinetics()
	{					/* define_kinetics */
	int c;
	int length;
	int i;
	int randomcode;
	void (*drug_routine[21])();
	int maxline;

	/* define pointers to different drug routines */
	drug_routine[1] = fentanyl;
	drug_routine[2] = alfentanil;
	drug_routine[3] = sufentanil;
	drug_routine[4] = dexmedetomidine;
	drug_routine[5] = thiopental;
	drug_routine[6] = midazolam;
	drug_routine[7] = diazepam;
	drug_routine[8] = propofol;
	drug_routine[9] = lidocaine;
	drug_routine[10] = ketamine;
	drug_routine[11] = methohexital;
	drug_routine[12] = etomidate;
	drug_routine[13] = methadone;
	drug_routine[14] = pancuronium;
	drug_routine[15] = vecuronium;
	drug_routine[16] = atracurium;
	drug_routine[17] = rocuronium;
	drug_routine[18] = lorazepam;
	drug_routine[19] = remifentanil;
	drug_routine[max_drug_id] = external;
	do	
		{
		opening_screen();
		cls();
        /* get drug */
		if (propofol_study_flag || nausea_study_flag || lorazepam_study_flag)
			{
			no_zero_order_flag = 1;
			if (propofol_study_flag)
				{
				while (drug_id < 1 || drug_id > 2)
					{
					gotoxy(0, 4);
					printf("Please select drug\n");
					printf("     1 = propofol\n");
					printf("     2 = sufentanil\n");
					printf("     Your selection:    ");
					drug_id = (int) enter_real(0.0, 21, 7);
					}
				switch (drug_id)
					{
				case (1):
					{
					drug_id = 8;
					kinetic_set = 2;
					effect_flag = 1;
					break;
					}
				case (2):
					{
					drug_id = 3;
					kinetic_set = 1;
					effect_flag = 2;
					break;
					}
					}
				}
			if (nausea_study_flag)
				/* Nausea & Vomiting Study */
				{
				drug_id = 8;
				kinetic_set = 2;
				effect_flag = 1;
				desired = 1;
				no_zero_order_flag = 1;
				}
			if (lorazepam_study_flag)
				{
				effect_flag = 1;
				desired = 0;
				no_zero_order_flag = 1;
				strcpy(output_file,"Roche");
				plasma_only_flag = 1;
				while (drug_id != 6 && drug_id != 18)
					{
					gotoxy(0, 4);
					printf("You have selected the midazolam/lorazepam study.\n");
					printf("Please enter your randomization code:        ");
					randomcode = (int) enter_real(0.0, 38, 5);
					if (randomcode > 0 && randomcode < 97)
						{
						drug_id = roche_codes[randomcode] % seed;
						sprintf(output_file+5,"%02d",randomcode);
						output_file[7]=0;
						while(desired != 25 && desired != 50)
							{
							gotoxy(0, 10);
							printf("Please entered the desired initial target concentration.\n");
							printf("You must enter either 25 or 50 mcg/ml:        ");
							desired = (int) enter_real(0.0, 39, 11);
							}
						}
					}
				}
			}
		else
			{
			maxline = (int)((float) max_drug_id/2.0 + 1);
			while (drug_id < 1 || drug_id > max_drug_id)
				{
				gotoxy(0, 3);
				printf("Please select drug\n");
				for (i = 1;  i <= max_drug_id;  i++)
					{
					if (i < maxline)
						{
						gotoxy(0, i+3);
						printf("     %d = %s", i, drug_name[i]);
						}
					else
						{
						gotoxy(41, i+4-maxline);
						printf("     %d = %s", i, drug_name[i]);
						}
					}
				gotoxy(0, maxline+3);
				printf("     Your selection:    ");
				drug_id = (int) enter_real(0.0, 21, 3 + maxline);
        		}
			}
        cls();
		gotoxy(0, 4);
		if (propofol_study_flag == 1 || lorazepam_study_flag == 1)
			{
			if (propofol_study_flag == 1)
				printf("ICI Multicenter Study using %s\n", drug_name[drug_id]);
			if (lorazepam_study_flag == 1)
				{
				cls();
				printf("Midazolam vs Lorazepam ICU Trial\n");
				}
			}
   		else
			printf("Drug selected: %s\n", drug_name[drug_id]);

		(*drug_routine[drug_id])();	/* call specific routine          */
										/* note that individual values of */
										/* drug_routine are pointers to   */
										/* the subroutines                */

//				printf("k10 = %f\n", k10);
//				exit(-1);
        /**********************************/
        /* Drug and kinetic set selected  */
        /**********************************/

		ve = vc * .0001;
		if (no_zero_order_flag == 1)
			zero_order_flag = 0;
		else
			{
			while (zero_order_flag != 0 && zero_order_flag != 1)
				{
				gotoxy(0, 18);
				printf("How do you want STANPUMP to run?\n");
				printf("     1 = in TARGET mode (STANPUMP aims for a target)\n");
				printf("     2 = in bolus / infusion mode, (you set the dose)\n");
				printf("     Your selection:     ");
				zero_order_flag = (int) enter_real(0.0, 21, 21);
				zero_order_flag--;
				}
			}

		if (zero_order_flag == 1)
			{
			desired = 0;
			desired_effect = 0;
			}
		else
			{
			if (effect_data)
				{
				while (effect_flag < 1 || effect_flag > (2 + pd_data))
					{
					gotoxy(0, 18);
					printf("Please select mode:              \n");
					printf("     1 = target plasma concentration                      \n");
					printf("     2 = target effect site concentration                 \n");
					if (pd_data == 1)
					printf("     3 = target effect\n");
					printf("     Your selection:     ");
					effect_flag = (int) enter_real(0.0, 21, 21 + pd_data);
					}
				--effect_flag;
				}
			else
				effect_flag = 0;

			if (effect_flag < 2)
				{
				length = strlen(drug_name[drug_id]) + strlen(site[effect_flag]) + 26;
				gotoxy(0, 22 + pd_data);
				printf("%s %s concentration (%s/ml)?",
					site[effect_flag], drug_name[drug_id], conc_units);
				while (desired < 0.0 || desired > max_concentration)
					{
					gotoxy(length, 22);
					printf("          ");
					desired = enter_real(0.0, length, 22 + pd_data);
					}
				}
			else
				{
				length = strlen(effect_text) + 10;
				gotoxy(0, 22 + pd_data);
				printf("Desired %s?", effect_text);
				while (desired < e0 || desired >= (e0 + emax) * .99999)
					{
					gotoxy(length, 22 + pd_data);
					printf("               ");
					desired = enter_real(0.0, length, 22 + pd_data);
					}
				desired_effect = desired;
				desired = inverse_pd_model(desired);
				}
			gotoxy(length, 22 + pd_data);
			printf("%.2lf  ", desired);
			}

        /*  remove this comment if you want to enter infusate_concentration
        printf("Infused %s concentration (%s/ml)? ", drug_name[drug_id],infused_units);
        infusate_concentration = enter_real(0.0, 0, 23);
        */

		cls();
		gotoxy(0, 4);
		if (hudson_flag == 0 && lorazepam_study_flag == 0)
			{
			printf("Drug Selected: %s\n", drug_name[drug_id]);
			printf("Parameter set: %s\n\n", parameter_name);
			printf("     Vc  = %6.4lf liters\n", vc);
			printf("     k10 = %6.4lf / minute\n", k10);
			printf("     k12 = %6.4lf / minute\n", k12);
			printf("     k13 = %6.4lf / minute\n", k13);
			printf("     k21 = %6.4lf / minute\n", k21);
			printf("     k31 = %6.4lf / minute\n\n", k31);
			if (effect_data)
				{
				if (t_peak == 0)
					printf("     ke0 = %6.4lf / minute\n\n", k41);
				else
					printf("     peak effect is at %6.4f min\n\n", t_peak);
				}
			printf("Desired %s concentration = %6.2lf %s/ml\n",
				drug_name[drug_id], desired, conc_units);
			printf("Infused %s concentration: %6.2lf %s/ml\n",
				drug_name[drug_id], infusate_concentration, infused_units);
			if (zero_order_flag == 1)
				printf("Mode: you will select the drug dosage\n");
			else
				{
				printf("Mode: STANPUMP will maintain a constant concentration in the %s.\n",
					site[effect_flag]);
				if (effect_flag == 2)
					printf("and will target the effect itself\n");
				}
			if (drug_id == 4 && kinetic_set > 5)
				printf("Height = %6.2lf cm.\n", height);
			printf("\n");
			}
		else
			{
			if (hudson_flag == 1)
				printf("Kinetics randomized to alfentanil or sufentanil\n");
			if (lorazepam_study_flag == 1)
				printf("Pharmacokinetics randomized to midazolam or lorazepam\n");
			gotoxy(0,22);
			}
		printf("Weight = %2.2lf kg, height = %2.2lf, age = %2.2lf\n", mass, height, age);
		printf("Lean body mass = %2.2lf kg, body surface area = %2.2lf meters squared",lbm,bsa);

		gotoxy(0, 24);
		printf("Please select 1 to confirm, 2 to change:");
		c = 0;
		if (quick_flag == 1)
			{
			c = 1;
			}
		else
			{
			while (c != 1 && c != 2)
				c = (int) enter_real(0.0, 41, 24);
			}
		if (c == 2)
			{
			gotoxy(0, 24);
			printf("Restoring defaults                        ");
			initialize_globals();
			strcpy(drug_name[max_drug_id], "external");
			}
		}
	while (c == 2);

	/*  remove this comment if you want to enter time increment
    printf("Time increment (secs)? ");
    delta_seconds = (int) enter_real(10.0, 0, 6);
	*/

    /* calculate maximum pump rate */
    /* in units of cc/min */
	maximum_rate = maximum_rate * mass / infusate_concentration;
	maximum_rate = maximum_rate * 60.0;	/* cc/hour */
	}					/* define_kinetics */

	
void calculate_udfs()
	{					/* calculate_udfs() */
	/* calculate unit disposition functions */
	int i;
	double prior;
	double temp1, temp2, temp3, temp4;
	double l1, l2, l3, l4;

	fprintf(pkfile, "Subject:   %s\n", subject);
	fprintf(pkfile, "Weight:    %10.4f\n", mass);
	fprintf(pkfile, "Height:    %10.4f\n", height);
	fprintf(pkfile, "Age:       %10.4f\n", age);
	if (gender == 1)
		fprintf(pkfile, "Gender:    male\n");
	else
		fprintf(pkfile, "Gender:    female\n");
	fprintf(pkfile, "BSA:       %10.4f\n", bsa);
	fprintf(pkfile, "LBM        %10.4f\n", lbm);	
	fprintf(pkfile, "User:      %s\n", user);
	fprintf(pkfile, "Revision:  %s\n", revision_date);
	if (lorazepam_study_flag == 0 || debug_flag == 1)
		{
		fprintf(pkfile, "Drug:      %s\n", drug_name[drug_id]);
		fprintf(pkfile, "PK set:    %s\n", parameter_name);
		fprintf(pkfile, "Infusate   %10.4f %s/ml\n", infusate_concentration,
			infused_units);
		}
	fprintf(pkfile, "Pump type: %s\n", pump_name[pump_type]);

	if (lorazepam_study_flag == 0 || debug_flag == 1)
		{	
		fprintf(pkfile,"Vc  = %6.4lf liters\n", vc);
		fprintf(pkfile,"k10 = %6.4lf / minute\n", k10);
		fprintf(pkfile,"k12 = %6.4lf / minute\n", k12);
		fprintf(pkfile,"k13 = %6.4lf / minute\n", k13);
		fprintf(pkfile,"k21 = %6.4lf / minute\n", k21);
		fprintf(pkfile,"k31 = %6.4lf / minute\n", k31);
	
		if (k21 > 0)
			fprintf(pkfile,"V2 = %6.4lf liters\n", vc*k12/k21);
		if (k31 > 0)
			fprintf(pkfile,"V3 = %6.4lf liters\n", vc*k13/k31);
	
		fprintf(pkfile,"Cl1 = %6.4lf liters / minute\n", vc*k10);
		if (k21 > 0)
			fprintf(pkfile,"Cl2 = %6.4lf liters / minute\n", vc*k12);
		if (k31 > 0)
			fprintf(pkfile,"Cl3 = %6.4lf liters / minute\n", vc*k13);
		
		if (effect_data)
			fprintf(pkfile,"ke0 = %6.4lf / minute\n", k41);
		}

    /* convert units from /minute to /second */
	k10 /= 60.0;
	k12 /= 60.0;
	k13 /= 60.0;
	k21 /= 60.0;
	k31 /= 60.0;
	k41 /= 60.0;
	cube(k10, k12, k21, k13, k31, lambda);

	p_coef[4] = 0;
	lambda[4] = k41;
	if (k31 > 0)
		{
		p_coef[1] = (k21 - lambda[1]) * (k31 - lambda[1]) / 
			(lambda[1] - lambda[2]) / 
			(lambda[1] - lambda[3]) / 
			vc / lambda[1];
		p_coef[2] = (k21 - lambda[2]) * (k31 - lambda[2]) / 
			(lambda[2] - lambda[1]) / 
			(lambda[2] - lambda[3]) / 
			vc / lambda[2];
		p_coef[3] = (k21 - lambda[3]) * (k31 - lambda[3]) / 
			(lambda[3] - lambda[2]) / 
			(lambda[3] - lambda[1]) / 
			vc / lambda[3];
		e_coef[1] = p_coef[1] / (k41 - lambda[1]) * k41;
		e_coef[2] = p_coef[2] / (k41 - lambda[2]) * k41;
		e_coef[3] = p_coef[3] / (k41 - lambda[3]) * k41;
		e_coef[4] = (k41 - k21) * (k41 - k31) / 
			(lambda[1] - k41) / 
			(lambda[2] - k41) / 
			(lambda[3] - k41) / vc;
		}
	else
		{
		if (k21 > 0)
			{
			p_coef[1] = (k21 - lambda[1]) / (lambda[2] - lambda[1]) / vc / lambda[1];
			p_coef[2] = (k21 - lambda[2]) / (lambda[1] - lambda[2]) / vc / lambda[2];
			p_coef[3] = 0;
			e_coef[1] = p_coef[1] / (k41 - lambda[1]) * k41;
			e_coef[2] = p_coef[2] / (k41 - lambda[2]) * k41;
			e_coef[3] = 0;
			e_coef[4] = (k21 - k41) / (lambda[1] - k41) / (lambda[2] - k41) / vc;
			}
		else
			{
			p_coef[1] = 1 / lambda[1] / vc;
			p_coef[2] = 0;
			p_coef[3] = 0;
			e_coef[1] = p_coef[1] / (k41 - lambda[1]) * k41;
			e_coef[2] = 0;
			e_coef[3] = 0;
			e_coef[4] = 1 / (lambda[1] - k41) / vc;
			}
		}

	if (lorazepam_study_flag == 0 || debug_flag == 1)
		{
		fprintf(pkfile,"t 1/2 alpha = %6.4lf min\n", .693/lambda[1] / 60);
		if (k21 > 0)                                                    
			fprintf(pkfile,"t 1/2 beta  = %6.4lf min\n", .693/lambda[2] / 60);
		if (k31 > 0)
			fprintf(pkfile,"t 1/2 gamma = %6.4lf min\n", .693/lambda[3] / 60);
		if (k41 > 0)
			fprintf(pkfile,"t 1/2 ke0   = %6.4lf min\n", .693/ k41 / 60);

		fprintf(pkfile, "lambda[1] = %15.8f\n", lambda[1] * 60);
		fprintf(pkfile, "lambda[2] = %15.8f\n", lambda[2] * 60);
		fprintf(pkfile, "lambda[3] = %15.8f\n", lambda[3] * 60);
		fprintf(pkfile, "lambda[4] = %15.8f\n", lambda[4] * 60);
	
		fprintf(pkfile, "Bolus Coefficients\n");
		fprintf(pkfile, "p_coef[1] = %15.8f\n", p_coef[1] * lambda[1] );
		fprintf(pkfile, "p_coef[2] = %15.8f\n", p_coef[2] * lambda[2] );
		fprintf(pkfile, "p_coef[3] = %15.8f\n", p_coef[3] * lambda[3] );
		fprintf(pkfile, "e_coef[1] = %15.8f\n", e_coef[1] * lambda[1] );
		fprintf(pkfile, "e_coef[2] = %15.8f\n", e_coef[2] * lambda[2] );
		fprintf(pkfile, "e_coef[3] = %15.8f\n", e_coef[3] * lambda[3] );
		fprintf(pkfile, "e_coef[4] = %15.8f\n", e_coef[4] * lambda[4] );
	
		fprintf(pkfile, "Infusion Coefficients\n");
		fprintf(pkfile, "p_coef[1] = %15.8f\n", p_coef[1] / 60);
		fprintf(pkfile, "p_coef[2] = %15.8f\n", p_coef[2] / 60);
		fprintf(pkfile, "p_coef[3] = %15.8f\n", p_coef[3] / 60);
		fprintf(pkfile, "e_coef[1] = %15.8f\n", e_coef[1] / 60);
		fprintf(pkfile, "e_coef[2] = %15.8f\n", e_coef[2] / 60);
		fprintf(pkfile, "e_coef[3] = %15.8f\n", e_coef[3] / 60);
		fprintf(pkfile, "e_coef[4] = %15.8f\n", e_coef[4] / 60);
		}

	t_peak *= 60; // convert peak time to seconds
	if (t_peak > 0)
		{
		k41 = recalculate_ke0();
		lambda[4] = k41;
		if (k31 > 0)
			{
			e_coef[1] = p_coef[1] / (k41 - lambda[1]) * k41;
			e_coef[2] = p_coef[2] / (k41 - lambda[2]) * k41;
			e_coef[3] = p_coef[3] / (k41 - lambda[3]) * k41;
			e_coef[4] = (k41 - k21) * (k41 - k31) / 
				(lambda[1] - k41) / 
				(lambda[2] - k41) / 
				(lambda[3] - k41) / vc;
			}
		else
			{
			if (k21 > 0)
				{
				e_coef[1] = p_coef[1] / (k41 - lambda[1]) * k41;
				e_coef[2] = p_coef[2] / (k41 - lambda[2]) * k41;
				e_coef[3] = 0;
				e_coef[4] = (k21 - k41) / (lambda[1] - k41) / (lambda[2] - k41) / vc;
				}
			else
				{
				e_coef[1] = p_coef[1] / (k41 - lambda[1]) * k41;
				e_coef[2] = 0;
				e_coef[3] = 0;
				e_coef[4] = 1 / (lambda[1] - k41) / vc;
				}
			}
        if (lorazepam_study_flag == 0 || debug_flag == 1)
        	{
			fprintf(pkfile,"New ke0 calculated to give peak effect at %f minutes\n", t_peak / 60);
			fprintf(pkfile, "ke0 (lambda[4]) = %15.8f\n", lambda[4] * 60);
			fprintf(pkfile,"t 1/2 ke0   = %6.4lf min\n", .693 / k41 / 60);
			
			fprintf(pkfile, "New Bolus Coefficients\n");
			fprintf(pkfile, "e_coef[1] = %15.8f\n", e_coef[1] * lambda[1] );
			fprintf(pkfile, "e_coef[2] = %15.8f\n", e_coef[2] * lambda[2] );
			fprintf(pkfile, "e_coef[3] = %15.8f\n", e_coef[3] * lambda[3] );
			fprintf(pkfile, "e_coef[4] = %15.8f\n", e_coef[4] * lambda[4] );
			
			fprintf(pkfile, "New Infusion Coefficients\n");
			fprintf(pkfile, "e_coef[1] = %15.8f\n", e_coef[1] / 60);
			fprintf(pkfile, "e_coef[2] = %15.8f\n", e_coef[2] / 60);
			fprintf(pkfile, "e_coef[3] = %15.8f\n", e_coef[3] / 60);
			fprintf(pkfile, "e_coef[4] = %15.8f\n", e_coef[4] / 60);
			}
		}

	temp1 = 0;
	temp2 = 0;
	temp3 = 0;
	temp4 = 0;

	l1 = exp(-lambda[1]);
	l2 = exp(-lambda[2]);
	l3 = exp(-lambda[3]);
	l4 = exp(-lambda[4]);

    /* calculate udf, plasma concentration, for an infusion of 1/second */
	p_udf[0] = 0;
	for (i = 1;  i < 199;  i++)
		{
		temp1 = temp1 * l1 + p_coef[1] * (1 - l1);
		temp2 = temp2 * l2 + p_coef[2] * (1 - l2);
		temp3 = temp3 * l3 + p_coef[3] * (1 - l3);
		p_udf[i] = temp1 + temp2 + temp3;
		}

    /* now calculate udf, effect site, until peak.  Note peak as peak_time */

	if (effect_data)
		{
		temp1 = 0;
		temp2 = 0;
		temp3 = 0;
		temp4 = 0;
		e_udf[0] = 0;

		for (i = 1;  i <= delta_seconds;  i++)
			{
			temp1 = temp1 * l1 + e_coef[1] * (1 - l1);
			temp2 = temp2 * l2 + e_coef[2] * (1 - l2);
			temp3 = temp3 * l3 + e_coef[3] * (1 - l3);
			temp4 = temp4 * l4 + e_coef[4] * (1 - l4);
			e_udf[i] = temp1 + temp2 + temp3 + temp4;
			}

		i = delta_seconds;
		prior = e_udf[i - 1];
		while (prior < e_udf[i])
			{
			prior = e_udf[i];
			i++;
			temp1 = temp1 * l1;
			temp2 = temp2 * l2;
			temp3 = temp3 * l3;
			temp4 = temp4 * l4;
			e_udf[i] = temp1 + temp2 + temp3 + temp4;
			if (i > 2698)
				{
				printf("Internal error: UDF definition exceeds 2700 elements (45 minutes)");
				restore_keyboard();
				stop_pump();
				exit(1);
				}
			}
		peak_time = i - 1;
		prior_peak_time = peak_time;
		fprintf(pkfile, "peak time = %4i seconds\n", peak_time);
		}
	}					/* calculate_udfs() */

double recalculate_ke0()
	{
	double too_large, ke0, too_small;
	int result, count;
	
	too_small = 0;
	ke0 = k41;
	count = 0;

	// find too_large first estimate
	too_large = k41*1.5;
	while (before_or_after(too_large) > -1)
		too_large *= 1.5;

	// Now find new value of ke0 using binary search
	result = before_or_after(ke0);
	while (result != 0)
		{
		if (result == -1)
			too_large = ke0;
		else
			too_small = ke0;
		ke0 = (too_large + too_small) / 2;
		result = before_or_after(ke0);
		count++;
		if (count > 100)
			exit(-1);
	    }
	return ke0;
	}

int before_or_after(ke0)
double ke0;
	{
	double time1, time2, time3;
	double result1, result2, result3;

	lambda[4] = ke0;
	if (k31 > 0)
		{
		e_coef[1] = p_coef[1] / (ke0 - lambda[1]) * ke0;
		e_coef[2] = p_coef[2] / (ke0 - lambda[2]) * ke0;
		e_coef[3] = p_coef[3] / (ke0 - lambda[3]) * ke0;
		e_coef[4] = (ke0 - k21) * (ke0 - k31) / 
			(lambda[1] - ke0) / 
			(lambda[2] - ke0) / 
			(lambda[3] - ke0) / vc;
		}
	else
		{
		if (k21 > 0)
			{
			e_coef[1] = p_coef[1] / (ke0 - lambda[1]) * ke0;
			e_coef[2] = p_coef[2] / (ke0 - lambda[2]) * ke0;
			e_coef[3] = 0;
			e_coef[4] = (k21 - ke0) / (lambda[1] - ke0) / (lambda[2] - ke0) / vc;
			}
		else
			{
			e_coef[1] = p_coef[1] / (ke0 - lambda[1]) * ke0;
			e_coef[2] = 0;
			e_coef[3] = 0;
			e_coef[4] = 1 / (lambda[1] - ke0) / vc;
			}
		}
	
	// convert from infusion coefficients to udf coefficients
	e_coef[1] *= lambda[1];
	e_coef[2] *= lambda[2];
	e_coef[3] *= lambda[3];
	e_coef[4] *= lambda[4];

	time1 = t_peak - .01;
	time2 = t_peak;
	time3 = t_peak + .01;

	result1 = 	e_coef[1]*exp(-lambda[1]*time1)+
				e_coef[2]*exp(-lambda[2]*time1)+
				e_coef[3]*exp(-lambda[3]*time1)+
				e_coef[4]*exp(-lambda[4]*time1);

	result2 = 	e_coef[1]*exp(-lambda[1]*time2)+
				e_coef[2]*exp(-lambda[2]*time2)+
				e_coef[3]*exp(-lambda[3]*time2)+
				e_coef[4]*exp(-lambda[4]*time2);

	result3 = 	e_coef[1]*exp(-lambda[1]*time3)+
				e_coef[2]*exp(-lambda[2]*time3)+
				e_coef[3]*exp(-lambda[3]*time3)+
				e_coef[4]*exp(-lambda[4]*time3);

	if (result1 < result2 && result3 < result2)
		return 0;	// peak found
	if (result1 > result2)
		return -1;  // peak is earlier, must lower ke0
	else
		return 1;	// peak is later, must raise ke0
	}

void get_prior_infusions()
	{					/* get_prior_infusions */
	int temp;
	double T;			/* Infusion duration */
	double I_start;		/* Infusion start */
	double I_stop;		/* Infusion stop */
	double rate;		/* infusion rate */
	int i;				/* for loops */
	double prior_time;
	double rate60;
	double I_stop60;

	cls();
	prior_time = 99999;
	while (TRUE)
		{
		temp = -1;
		while (temp < 0 || temp > 2)
			{
			gotoxy(0, 1);
			printf("Enter dosing history in chronological order.\n\n");
			printf("Please select:\n");
			printf("     0 = no additional doses\n");
			printf("     1 = prior bolus\n");
			printf("     2 = prior infusion\n");
			printf("     Your selection:   ");
			temp = (int) enter_real(0.0, 21, 7);
			}
		if (temp == 0)
			return;
		gotoxy(0, 9);
		for (i = 1;  i < 4;  i++)
			printf("                                              \n");
		I_start = prior_time + 1;
		if (temp == 1)
			{
			while (I_start > prior_time || I_start < 1)
				{
				gotoxy(0, 9);
				printf("Minutes ago bolus was given:      ");
				I_start = enter_real(0.0, 29, 9);
				}
			printf("\nBolus amount (%s):     ", infused_units);
			rate = enter_real(0.0, 19, 10) * 60;
			I_stop = I_start - 1.0 / 60.0;	/* bolus = 1 second infusion */
			}
		else
			{
			while (I_start > prior_time)
				{
				gotoxy(0, 9);
				printf("Minutes ago infusion was started:    ");
				I_start = enter_real(0.0, 34, 9);
				}
			I_stop = prior_time + 1;
			while (I_stop > I_start || I_stop < 0)
				{
				gotoxy(0, 10);
				printf("Minutes ago infusion was stopped:    ");
				I_stop = enter_real(0.0, 34, 10);
				}
			printf("\nInfusion rate (%s/min):      ", infused_units);
			rate = enter_real(0.0, 24, 11);
			}
		T = (I_start - I_stop) * 60;	/* in seconds */
		rate60 = rate / 60;	/* per second */
		I_stop60 = I_stop * 60;
		p_state[1] += p_coef[1] * rate60 * (1 - exp(-lambda[1] * T)) * exp(-lambda[1] * I_stop60);
		p_state[2] += p_coef[2] * rate60 * (1 - exp(-lambda[2] * T)) * exp(-lambda[2] * I_stop60);
		p_state[3] += p_coef[3] * rate60 * (1 - exp(-lambda[3] * T)) * exp(-lambda[3] * I_stop60);
		if (effect_data)
			{
			e_state[1] += e_coef[1] * rate60 * (1 - exp(-lambda[1] * T)) * exp(-lambda[1] * I_stop60);
			e_state[2] += e_coef[2] * rate60 * (1 - exp(-lambda[2] * T)) * exp(-lambda[2] * I_stop60);
			e_state[3] += e_coef[3] * rate60 * (1 - exp(-lambda[3] * T)) * exp(-lambda[3] * I_stop60);
			e_state[4] += e_coef[4] * rate60 * (1 - exp(-lambda[4] * T)) * exp(-lambda[4] * I_stop60);
			}
		/* add to infusion history */
		b_time[n_infs] = -I_start;
		b_rate[n_infs] = rate;
		n_infs++;
		b_time[n_infs] = -I_stop;
		b_rate[n_infs] = 0;
		n_infs++;
		prior_time = I_stop;
		}
	}					/* get_prior_infusions */

/* pharmacodynamic models */
double pd_model(ce)
double ce;
	{					/* pd_model */
	double ce_gamma;
	ce_gamma = pow(ce, gamma);
	return e0 + emax * ce_gamma / (ic50_gamma + ce_gamma);
	}

double inverse_pd_model(effect)
double effect;
	{					/* inverse_pd_model */
	return (pow((effect - e0)*ic50_gamma / (e0 + emax - effect), 1/gamma));
	}					/* inverse_pd_model */

void strip_white(a)
char *a;
	{
	char *b;
	b = a;
	while (*b)
		{
		switch (*b)
			{
		case (','):
		case ('\n'):
		case ('\t'):
		case ('\r'):
		case ('"'):		// double quote
		case ('\''):	// single quote
			*b = ' ';
			break;
			}
		b++;
		}
	}

/* routine for Hudson's Study */
void hudson_study(code)
char *code;
	{					/* hudson_study */
	int code1;
	int code2;
	int i;
	char test[3];

	if (strlen(code) != 4)
		{
		printf("illegal study code");
		restore_keyboard();
		stop_pump();
		exit(1);
		}

	for (i = 0;  i < 4;  i++)
		{
		if (isdigit(code[i]) == 0)
			{
			printf("illegal study code");
			restore_keyboard();
			stop_pump();
			exit(1);
			}
		}

	test[2] = 0;
	test[0] = code[0];
	test[1] = code[1];
	code1 = atoi(test);
	code1 %= 7;
	code1 %= 2;
	test[0] = code[2];
	test[1] = code[3];
	code2 = atoi(test);
	code2 %= 7;
	code2 %= 2;
	if (code1 != code2)
		{
		printf("illegal study code");
		restore_keyboard();
		stop_pump();
		exit(1);
		}
	if (code1 == 0)
		{
		drug_id = 3;	/* sufentanil */
		kinetic_set = 1;	/* Hudson sufentanil data set */
		ref_con = 3.5;	/* initial and reference Cp */
		}
	else
		{
		drug_id = 2;	/* alfentanil */
		kinetic_set = 4;	/* Hudson alfentanil kinetic set */
		ref_con = 800;	/* initial and reference Cp */
		}
	hudson_flag = 1;
	}					/* hudson_study */
