#include <stdio.h>          
#include <math.h>
#include <string.h>
#include "stanpump.h"
#include "bayes.h"

void fentanyl()
	{					/* fentanyl */
	while (kinetic_set < 1 || kinetic_set > 3)
		{
		gotoxy(0, 6);
		printf("Fentanyl kinetic set:\n");
		printf("     1 = Hug, not weight adjusted\n");
		printf("     2 = Scott, not weight adjusted\n");
		printf("     3 = Shafer, not weight adjusted\n");
		printf("     Your selection:    ");
		kinetic_set = (int) enter_real(0.0, 21, 10);
		}
	max_concentration = 100;	/* maximum reasonable fentanyl Cp */
	awake_concentration = 1.0;	/* target concentration at end of case */
	if (infusate_concentration == 0)
		infusate_concentration = 50;
		/* concentration of fentanyl */
	maximum_rate = 50;	/* maximum rate in ug/kg/min */
	if (bard_flag == 1)
		maximum_rate = 2.5;
	k41 = .147;			/* from Scott & Stanski */
	t_peak = 3.694;		/* from Shafer/Varvel, t_peaks.xls */
	switch (kinetic_set)
		{
	case (1):
		// McClain DA, Hug CC Jr. Intravenous fentanyl kinetics.
		// Clin Pharmacol Ther 1980 Jul;28(1):106-114 
		{
		strcpy(parameter_name, "McClain and Hug's");
		vc = 26.91;
		k10 = .041;
		k12 = .185;
		k13 = .141;
		k21 = .103;
		k31 = .020;
		break;
		}
	case (2):
			// Scott JC, Stanski DR. Decreased fentanyl and alfentanil 
			// dose requirements with age. A simultaneous pharmacokinetic 
			// and pharmacodynamic evaluation.J Pharmacol Exp Ther 1987 Jan;240(1):159-166 
    	{
		strcpy(parameter_name, "Scott's");
		vc = 12.7;
		k10 = .056;
		k12 = .373;
		k13 = .180;
		k21 = .096;
		k31 = .0077;
		break;
		}
	case (3):
		// Shafer SL, Varvel JR, Aziz N, Scott JC
		// Pharmacokinetics of fentanyl administered by 
		// computer-controlled infusion pump.
		// Anesthesiology 1990 Dec;73(6):1091-1102 
		{
		strcpy(parameter_name, "Shafer's");
		vc = 6.09;		/* From Shafer, Varvel, Aziz, & Scott */
		k10 = .0827;	/* Rate constants */
		k12 = .471;
		k13 = .225;
		k21 = .102;
		k31 = .00600;
		break;
		}
		}
	}					/* fentanyl */

void alfentanil()
	{					/* alfentanil */
	double v2, v3, cl1, cl2, cl3;
	while (kinetic_set < 1 || kinetic_set > 7)
		{
		gotoxy(0, 6);
		printf("Alfentanil kinetic set:\n");
		printf("     1 = Scott, not weight adjusted\n");
		printf("     2 = Scott, weight adjusted\n");
		printf("     3 = Maitre, weight adjusted\n");
		printf("     4 = Hudson, weight adjusted\n");
		printf("     5 = Goresky, < 1 year\n");
		printf("     6 = Goresky, 1 - 18 years\n");
		printf("     7 = Shafer, BSA adjusted\n");
		/* printf("     8 = Fiset: Pediatric cardiac (not CPB adjusted)\n"); */
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 14);
		}
	max_concentration = 4000;	/* maximum alfentanil Cp */
	awake_concentration = 100;	/* target concentration at end of case */
	maximum_rate = 300;	/* maximum rate in ug/kg/min */
	if (bard_flag == 1)
		maximum_rate = 50;
	k41 = .77;			/* from Scott & Stanski */
	t_peak = 1.376;		/* see t_peaks.xls */
	if (hudson_flag)
		{
    	/* Bob: Here is where you will want to set parameters   */
		/* for your patients getting alfentanil                 */
		maximum_rate = 50;
		if (infusate_concentration == 0)
			infusate_concentration = 250;
		}
	else
		{
		if (infusate_concentration == 0)
			infusate_concentration = 500;	/* undiluted drug */
		}
	switch (kinetic_set)
		{
	case (1):			/* Jim Scott J Pharmacol Exp Ther. 1987 Jan; 240(1): 159-166*/
		{
		strcpy(parameter_name, "Scott's, not weight adjusted");
		vc = 2.1853;
		k10 = .091;
		k12 = .656;
		k13 = .113;
		k21 = .214;
		k31 = .017;
		break;
		}
	case (2):			/* Jim Scott, weight adjusted */
		{
		strcpy(parameter_name, "Scott's, weight adjusted");
		vc = 2.1853 / 70.0 * mass;
		k10 = .091;
		k12 = .656;
		k13 = .113;
		k21 = .214;
		k31 = .017;
		break;
		}
	case (3):			/* Pierre Maitre */
		{
		bayes_data = 1;	/* first bayes data set in system */
		strcpy(parameter_name, "Maitre's");
		if (gender == 1)
			{
			vc = .111 * mass;
			}
		else
			{
			vc = .111 * 1.15 * mass;
			}
		if (age <= 40)
			{
			k10 = .356 / vc;
			k31 = .0126;
			}
		else
			{
			k10 = (.356 - (.00269 * (age - 40.0))) / vc;
			k31 = .0126 - (.000113 * (age - 40.0));
			}
		k12 = .104;
		k13 = .0170;
		k21 = .0673;
		break;
		}
	case (4):			/* Bob Hudson */
		{
		strcpy(parameter_name, "Hudson's, weight adjusted");
               /* Bob: Add your kinetics here once they are available */
		vc = .044 * mass;
		k10 = 0.1758;
		k12 = 0.4299;
		k13 = 0.0845;
		k21 = 0.1179;
		k31 = 0.0071;
		break;
		}
	case (5):			/* Goresky, < 1 year */
		{
		strcpy(parameter_name, "Goresky's, < 1 year");
		vc = 0.246 * mass;
		k10 = 0.036;
		k12 = 0.015;
		k13 = 0.0;
		k21 = 0.018;
		k31 = 0.0;
		break;
		}
	case (6):			/* Goresky's, 1 - 18 years */
		{
		strcpy(parameter_name, "Goresky's, 1 - 18 years");
		vc = 0.213 * mass;
		k10 = 0.0368;
		k12 = 0.0168;
		k13 = 0.0;
		k21 = 0.0188;
		k31 = 0.0;
		break;
		}
	case (7):			/* Shafer */
		{
		strcpy(parameter_name, "Shafer's");
		gotoxy(0, 16);
		vc = .825 * bsa;
		k10 = .0748;
		k12 = .515;
		k13 = .231;
		k21 = .142;
		k31 = .0185;
		break;
		}
	case (8):			/* Fiset, without CPB adjustment */
						/* Not implemented: needs further review */
		{
		strcpy(parameter_name, "Fiset, not CPB adjusted");
		vc = 0.236 * mass;
		v2 = 1.036 * mass;
		v3 = 25.96 * mass;
		cl1 = .021 * mass;
		cl2 = 0.413 * mass;
		cl3 = 0.158 * mass;
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		break;
		}
		}
	}					/* alfentanil */

void sufentanil()
	{					/* sufentanil */
	while (kinetic_set < 1 || kinetic_set > 3)
		{
		gotoxy(0, 6);
		printf("Sufentanil kinetic set:\n");
		printf("     1 = Hudson, weight adjusted\n");
		printf("     2 = Bovill, weight adjusted\n");
		printf("     3 = Gepts, not weight adjusted\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 10);
		}
	max_concentration = 30.0;	/* max reasonable Cp */
	awake_concentration = .25;	/* Desired Cp on emergence */
	maximum_rate = 50;	/* maximum rate in ug/kg/min */
	if (bard_flag == 1)
		maximum_rate = 1.5;
	k41 = .119;			/* from Scott, Cooke, & Stanski */
	t_peak = 5.588;		/* from Shafer/Varvel, t_peaks.xls */
	if (hudson_flag)
		{
		/* Bob: Here is where you will define the */
		/* concentration and maximum rate for your */
		/* study */
		if (infusate_concentration == 0)
			infusate_concentration = 5.0;
		maximum_rate = 10;
		}
	else
		{
		if (infusate_concentration == 0)
			infusate_concentration = 50.0;
		}
	switch (kinetic_set)
		{
	case (1):			/* Bob Hudson */
		{
		strcpy(parameter_name, "Hudson's");
		vc = 0.254 * mass;
		k10 = .0653;
		k12 = .2722;
		k13 = .0723;
		k21 = .1023;
		k31 = .0027;
		break;
		}
	case (2):			/* Bovill */
		{
		strcpy(parameter_name, "Bovill's");
		vc = 0.164 * mass;
		k10 = .089;
		k12 = .35;
		k13 = .077;
		k21 = .161;
		k31 = .010;
		break;
		}
	case (3):			/* Gepts */
		// Gepts E, Shafer SL, Camu F, Stanski DR, Woestenborghs R, Van Peer A, Heykants JJP
		// Linearity of pharmacokinetics and model estimation of sufentanil
		// Anesthesiology 83:1194-1204, 1995
		{
		strcpy(parameter_name, "Gepts'");
		vc = 14.3;
		k10 = 0.0645;
		k12 = 0.1086;
		k13 = 0.0229;
		k21 = 0.0245;
		k31 = 0.0013;
		break;
		}
		}
	}					/* sufentanil */

void dexmedetomidine()
	{					/* dexmedetomidine */
	double v2, v3, cl1, cl2, cl3;
	while (kinetic_set < 1 || kinetic_set > 9)
		{
		gotoxy(0, 6);
		printf("Dexmedetomidine kinetic set:\n");
		printf("     1 = Dog Study\n");
		printf("     2 = Horse Study\n");
		printf("     3 = Rat Study 1\n");
		printf("     4 = Rat Study 2\n");
		printf("     5 = Human Study\n");
		printf("     6 = Human Study: Height Adjusted\n");
		printf("     7 = Human, Markku \n");
		printf("     8 = Eisenach, epidural\n");
		printf("     9 = Eisenach, epidural 2\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 16);
		}
	max_concentration = 100;	/* maximum reasonable dexmedetomidine Cp */
	awake_concentration = 5.0;	/* target Cp at end of case */
	maximum_rate = 50;	/* maximum rate in ug/kg/min */
	k41 = 10;			/* no idea */
	effect_data = 0;
	switch (kinetic_set)
		{
	case (1):			/* Dog Study: from BA-85-15 */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 80;
		strcpy(parameter_name, "Dexmedetomidine: Dog Study Only");
		vc = 17.97 / 9.3 * mass;
		k10 = .001281;
		k12 = .0578;
		k13 = .01016;
		k21 = .1178;
		k31 = .02226;
		break;
		}
	case (2):			/* Horse Study, name unknown, usual telefax data */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 5000;
		strcpy(parameter_name, "Detomidine: Horse Study Only");
		vc = 0.136 * mass;
		k10 = 0.0248;
		k12 = 0.00932;
		k13 = 0.0295;
		k21 = 0.143;
		k31 = 0.0301;
		break;
		}
	case (3):			/* Bill's rat data, added 12/10/90 */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 20;
		strcpy(parameter_name, "Dexmedetomidine: Rat Study Only");
		vc = 0.258 * mass;	/* V1 in l/kg */
		k10 = .217;
		k12 = .239;
		k13 = .118;
		k21 = .0678;
		k31 = .008303;
		break;
		}
	case (4):			/* Bill's new rat data, fit using INFPUMP3 on 6/11/91 */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 20;
		strcpy(parameter_name, "Dexmedetomidine: Rat PK Set 2");
		vc = 0.322 * mass;	/* V1 in l/kg */
		k10 = .137;
		k12 = .287;
		k13 = .243;
		k21 = .0567;
		k31 = .0153;
		break;
		}
	case (5):			/* Human Data, from Barry Dyck */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 20;
		strcpy(parameter_name, "Dexmedetomidine: Human Data");
		vc = 8.0574;
		k10 = .0552;
		k12 = .258;
		k13 = .247;
		k21 = .163;
		k31 = .0112;
		break;
		}
	case (6):			/* Height Adjusted Human Data, from Barry Dyck */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 2.0;
		strcpy(parameter_name, "Dexmedetomidine: Height Adusted");
		vc = 7.99;
		v2 = 13.8;
		v3 = 187;
		cl1 = -0.927 + 0.00791 * height;
		cl2 = 2.26;
		cl3 = 1.99;
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		break;
		}
	case (7):			/* "Markku PK parameters, from Mo Noursalehi at RSDI, via Pekka, from an
						unknown source) */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 4.0;
		strcpy(parameter_name, "Dexmedetomidine: ""Markku parameters""");

		vc = 0.792 * mass;
		k10 = 0.0146;
		k12 = 0.029;
		k13 = 0;
		k21 = 0.0223;
		k31 = 0;
		break;
		}
	case (8):			/* For epidural administration to maintain constant */
						/* intrathecal level */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 20.0;
		maximum_rate = 10;
		max_concentration = 200;	/* maximum reasonable dexmedetomidine Cp */
		strcpy(parameter_name, "Epidural (target effect site!)");
		vc = .271;
		k10 = .0415;
		k12 = .038;
		k13 = 0;
		k21 = .0135;
		k31 = 0;
		k41 = .0302;
		effect_data = 1;
		break;
		}
	case (9):			/* For epidural administration to maintain constant */
						/* intrathecal level */
		{
		if (infusate_concentration == 0)
			infusate_concentration = 10.0;
		maximum_rate = 10;
		max_concentration = 300;	/* maximum reasonable dexmedetomidine Cp */
		strcpy(parameter_name, "Epidural set 2 (target effect site!)");
		vc = .0173;
		k10 = .006478;
		k12 = .698;
		k13 = 0;
		k21 = .002641;
		k31 = 0;
		k41 = .0289;
		effect_data = 1;
		break;
		}
		}
	}					/* dexmedetomidine */

void thiopental()
	{					/* thiopental */
//	while (kinetic_set < 1 || kinetic_set > 4)
//		{
//		gotoxy(0, 6);
//		printf("Thiopental kinetic set:\n");
//		printf("     1 = Stanski/Maitre\n");
//		printf("     2 = Human 2, weight adjusted\n");
//		printf("     3 = Rats, initial parameter set (Lars)\n");
//		printf("     4 = Rats, subsequent paramter set (Lars)\n");
//		printf("     Your selection:     ");
//		kinetic_set = (int) enter_real(0.0, 21, 11);
//		}
	kinetic_set = 1;
	max_concentration = 100.0;	/* max reasonable Cp */
	awake_concentration = 5;	/* Desired Cp on emergence */
	maximum_rate = 40;	/* maximum rate in mg/kg/min */
	k41 = .693 / 1.17;	/* from Stanski/Maitre */
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	switch (kinetic_set)
		{
	case (1):			/* Stanski/Maitre */
		// Stanski and Maitre
		// Population pharmacokinetics and pharmacodynamics of thiopental
		// The effect of age revisited
		// Anesthesiology 72:412-422, 1990
		{
		strcpy(parameter_name, "Stanski/Maitre");
		if (infusate_concentration == 0)
			infusate_concentration = 50;
		vc = 0.079 * mass;
		cl1 = 0.00307*mass;
		k10 = cl1 / vc;
		if (age <= 35)
			k12 = 0.48;
		else
			k12 = 0.48-0.00288*(age - 35);
		k21 = 0.0787;
		k13 = 0.107;
		k31 = 0.00389;
		break;
		}
		}				/* close kinetic_set switch */
	}					/* thiopental */

void midazolam()
	{					/* midazolam */
	double v2, v3, cl1, cl2, cl3;
	if (lorazepam_study_flag == 1)
		{
		kinetic_set = 4;
		infusate_concentration = 1000;
		}
	while (kinetic_set < 1 || kinetic_set > 5)
		{
		gotoxy(0, 6);
		printf("Midazolam kinetic set:\n");
		printf("     1 = Greenblatt, weight adjusted\n");
		printf("     2 = Buhrer, not weight adjusted\n");
		printf("     3 = Palo Alto VA ICU kinetics, BSA adjusted\n");
		printf("     4 = Zomorodi, 1997\n");
		printf("     5 = Palo Alto VA ICU, no covariates + virtual drug\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 12);
		}
	max_concentration = 500.0;	/* max reasonable Cp */
	awake_concentration = 15;	/* Desired Cp on emergence */
	maximum_rate = 400;	/* maximum rate in ug/kg/min */
	k41 = .693 / 4.0;	/* From Hung, Buhrer, & Stanski, unpublished, approx. */
	strcpy(conc_units, "ng");
	strcpy(infused_units, "ug");
	switch (kinetic_set)
		{
	case (1):			/* Greenblatt Pharmacokinetics (Clin Pharm Ther, April 1989) */
		{
		strcpy(parameter_name, "Greenblatt's");
		if (infusate_concentration == 0)
			infusate_concentration = 5000;
		vc = 0.73 * mass;
		k10 = 0.0151;
		k12 = 0.0526;
		k13 = 0.0;
		k21 = 0.0241;
		k31 = 0.0;
		break;
		}
	case (2):
		{				/* Buhrer */
		strcpy(parameter_name, "Buhrer's");
		if (infusate_concentration == 0)
			infusate_concentration = 5000;
		vc = 3.3;
		k10 = 0.162181;
		k12 = 0.610464;
		k13 = 0.252155;
		k21 = 0.114700;
		k31 = 0.008600;
		break;
		}
	case (3):			/* From Geller/Sladen/Ramsey ICU study */
		{
		strcpy(parameter_name, "ICU Multicenter Study, BSA adjusted");
		if (infusate_concentration == 0)
			infusate_concentration = 5000;
		vc = 33;
		v2 = 3.32+32.1*bsa;
		v3 = 365;
		cl1 = 0.0889+0.151*bsa;
		cl2 = 0.622;
		cl3 = 0.264;
		
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		effect_data = 0;
		break;
		}
	case (4):			/* From Midazolam/Lorazepam comparison study */
		{
		strcpy(parameter_name, "Zomorodi, 1979)");
		if (infusate_concentration == 0)
			infusate_concentration = 5000;
		vc = 27.1;
			
		k10 = 0.00786;
		k12 = 0.025498;
		k13 = 0;
		k21 = 0.005758;
		k31 = 0;
		effect_data = 0;
		break;
		}
	case (5):			/* From Geller/Sladen/Ramsey ICU study */
		{
		strcpy(parameter_name, "ICU Multicenter Study + Virtual Drug");
		if (infusate_concentration == 0)
			infusate_concentration = 5000;
		vc = 32.2;
		v2 = 53;
		v3 = 245;
		cl1 = 0.43;
		cl2 = 0.56;
		cl3 = 0.39;
		
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		effect_data = 0;
		virtual_icu_drug = 256; // Reference: Somma, ICU, Anesthesiology
		virtual_real_sum = desired;
		break;
		}

		}
	if (lorazepam_study_flag == 1)
		effect_data = 0;
	}					/* midazolam */
void diazepam()
	{					/* diazepam */
	kinetic_set = 1;	/* only 1 kinetic set so far */
	max_concentration = 1600.0;	/* max reasonable Cp */
	awake_concentration = 120;	/* Desired Cp on emergence */
	maximum_rate = 100;	/* maximum rate in ug/kg/min */
	k41 = .693 / 1.2;	/* From Hung, Buhrer, & Stanski, unpublished, approx. */
	strcpy(conc_units, "ng");
	strcpy(infused_units, "ug");
	switch (kinetic_set)
		{
	case (1):			/* Greenblatt Pharmacokinetics (Clin Pharm Ther, April 1989) */
		{
		strcpy(parameter_name, "Greenblatt, 1989");
		if (infusate_concentration == 0)
			infusate_concentration = 50;
		vc = 0.13 * mass;
		k10 = 0.0385;
		k12 = 0.00324;
		k13 = 0.0;
		k21 = 0.00038;
		k31 = 0.0;
		break;
		}
		}
	}					/* diazepam */

void propofol()
	{					/* propofol */
	double v2, v3, cl1, cl2, cl3, FBM;
	while (kinetic_set < 1 || kinetic_set > 9)
		{
		gotoxy(0, 6);
		printf("Propofol kinetic set:\n");
		printf("     1 = Marsh (Diprifusor)\n");
		printf("     2 = Kataria (Pediatric PK)\n");
		printf("     3 = ICU (LBM/FBM adjusted)\n");
		printf("     4 = Mora (cardiac surgery)\n");
		printf("     5 = Schnider: Integrated PK/PD\n");
		printf("     Your selection:     \n\n");
		printf("T peak is now set to 1.6 min. This will give a larger dose\n");
		printf("if the effect site is targetted than the prior version of\n");
		printf("STANPUMP. The t-peak of 1.6 min is based on Schnider, et al,\n");
		printf("Anesthesiology 90:1502-1516, 1999.");
		kinetic_set = (int) enter_real(0.0, 21, 12);
		}
	max_concentration = 40.0;	// max reasonable Cp
	awake_concentration = 0.5;	// Desired Cp on emergence
	maximum_rate = 15;	// maximum rate in mg/kg/min
	if (bard_flag == 1)
		maximum_rate = 1;
	k41 = 0.456;	// From The Influence of Age on Propofol Pharmacodynamics. 
	t_peak = 1.600; // Anesthesiology 90:1502-1516, 1999
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	
	// The following PK have been removed:
	
	//	case (1):			// Gepts, as suggested by P. Glass Anesthesiology A277, 1989
	//		{
	//	strcpy(parameter_name, "Gept's");
	//	if (infusate_concentration == 0)
	//		infusate_concentration = 10;
	//	vc = 0.159 * mass;
	//	k10 = 0.152;
	//	k12 = 0.207;
	//	k13 = 0.040;
	//	k21 = 0.092;
	//	k31 = 0.0048;
	//	break;
	//	}
	
	//case (2):			/* Dyck/Shafer, weight and age adjusted */
	//					/* As reported in Seminars in Anesthesia */
	//	{
	//	strcpy(parameter_name, "Dyck/Shafer, age and weight adjusted");
	//	if (infusate_concentration == 0)
	//		infusate_concentration = 10;
	//	vc = 9.64 - 0.0512 * age;
	//	v2 = 19.4;
	//	v3 = 571 - 1.66 * age;
	//	cl1 = 0.652 + 0.0148 * mass;
	//	cl2 = 1.68;
	//	cl3 = 2.67 - .0145 * age;
	//	k10 = cl1 / vc;
	//	k12 = cl2 / vc;
	//	k13 = cl3 / vc;
	//	k21 = cl2 / v2;
	//	k31 = cl3 / v3;
	//	break;
	//	}
	
	//case (6):			/* Shafer (Audrey, the better half)/White kinetic set */
	//	{				/* Anesthesiology 69:348-356, 1988					  */
	//					/* Data taken from file I2.out, microrate constants   */
	//					/* calculated from average volumes (23.5, 135.6) and  */
	//					/* clearances (2.09, 1.46)							  */
	//	strcpy(parameter_name, "Shafer/White");
	//	if (infusate_concentration == 0)
	//		infusate_concentration = 10;
	//	vc = .35 * mass;
	//	k10 = .0889;
	//	k12 = .0621;
	//	k13 = 0;
	//	k21 = .0108;
	//	k31 = 0;
	//	break;
	//	}
	
	//case (7):			/* Dyck/Shafer, Round4b, adult OR */
	//	{
	//	strcpy(parameter_name, "Dyck/Shafer Round 4b");
	//	if (infusate_concentration == 0)
	//		infusate_concentration = 10;
    //
	//	vc = 6.5;
	//	v2 = 26.0 - 0.15 * age;
	//	v3 = 488;
	//	cl1 = 0.19 + 0.027 * lbm;
	//	cl2 = 2.7 - 0.019 * age;
	//	cl3 = 2.3 - 0.0075 * age;
	//	k10 = cl1 / vc;
	//	k12 = cl2 / vc;
	//	k13 = cl3 / vc;
	//	k21 = cl2 / v2;
	//	k31 = cl3 / v3;
	//	break;
	//	}
	
	//	case (8):			/* Tackley et al, Group 2 	*/
	//	{				/* BJA 62, 46-53, 1989  	*/
	//	strcpy(parameter_name, "Tackley");
	//	if (infusate_concentration == 0)
	//		infusate_concentration = 10;
	//	vc = .32 * mass;
	//	k10 = .0827;
	//	k12 = .105;
	//	k13 = .022;
	//	k21 = .064;
	//	k31 = .0034;
	//	break;
	//	}

	
	switch (kinetic_set)
		{
	case (1):			/* Marsh (same as Diprifusor) */
		{				/* BJA 67, 41-48, 1991  	*/
		strcpy(parameter_name, "Marsh (Diprifusor)");
		if (infusate_concentration == 0)
			infusate_concentration = 10;
		vc = .228 * mass; // "obtained from our own pilot studies" - no data given
		k10 = .119;  // From Gepts, A&A 66:1256-1263, 1987
		k12 = .112;  // From Gepts, except that it is a typo: Gepts reported .114
		k13 = .0419; // From Gepts, A&A 66:1256-1263, 1987
		k21 = .055;  // From Gepts, A&A 66:1256-1263, 1987
		k31 = .0033; // From Gepts, A&A 66:1256-1263, 1987
		break;
		}

	case (2):
		// Kataria et al.
		// The pharmacokinetics of propofol in children using three 
		// different data analysis approaches
		// Anesthesiology 80:104-122, 1994
		{
		k41 = 10;	/* can't use adult ke0 with venous pediatric data */
		effect_data = 0;
		strcpy(parameter_name, "Kataria - pediatric model");
		if (infusate_concentration == 0)
			infusate_concentration = 10;
		vc = mass * 0.41;
		v2 = mass * 0.78 + 3.1 * age - 15.5;
		v3 = mass * 6.9;
		cl1 = mass * .035;
		cl2 = mass * .077; 
		cl3 = mass * .026;
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		break;
		}

	case (3):			/* ICU-CCIP: lbm/FBM adjusted */
		// From study by Barr et al
		{
		strcpy(parameter_name, "ICU-CCIP: lbm/FBM adjusted");
		if (infusate_concentration == 0)
			infusate_concentration = 10;
		FBM = mass - lbm;
		vc = lbm * .601;
		v2 = lbm * 6.34;
		v3 = lbm * 32.4 + FBM * 107;
		cl1 = lbm * .0502 - FBM * .0171;
		cl2 = lbm * .0159; 
		cl3 = lbm * .0251;
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		break;
		}
		
	case (4):			/* Mora CCIP in cardiac surgery model */
		{
		strcpy(parameter_name, "Mora cardiac surgery model");
		if (infusate_concentration == 0)
			infusate_concentration = 10;
		vc = 3.26;
		k10 = .351;
		k12 = .262;
		k13 = .282;
		k21 = .130;
		k31 = .0064;
		break;
		}
	
	case (5):
		{
		// Schnider et al.
		// The Influence of Method of Administration and Covariates on 
		// the Pharmacokinetics of Propofol in Adult Volunteers
		// Anesthesiology 1998 (in press at this moment)
		strcpy(parameter_name, "Schnider - no opioids");
		if (infusate_concentration == 0)
			infusate_concentration = 10;
		vc = 4.27;
		v2 = 18.9-0.391*(age-53);
		v3 = 238;
		cl1 = 1.89+0.0456*(mass-77)-0.0681*(lbm-59)+0.0264*(height-177);
		cl2 = 1.29-0.024*(age-53); 
		cl3 = 0.836;
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;
		k41 = 0.456;
		break;
		}
		}
		gotoxy(0,14);
		printf("                                                              \n");
		printf("                                                              \n");
		printf("                                                              \n");
		printf("                                                              \n");
		printf("                                                              \n");
	}					/* propofol */

void lidocaine()
	{					/* lidocaine */
	while (kinetic_set < 1 || kinetic_set > 4)
		{
		gotoxy(0, 6);
		printf("Lidocaine kinetic set:\n");
		printf("     1 = Rowland\n");
		printf("     2 = Vozeh\n");
		printf("     3 = Rat\n");
		printf("     4 = Schnider, pain patients\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 11);
		}
	max_concentration = 10;	/* max reasonable Cp */
	awake_concentration = 1.0;	/* Desired Cp on emergence */
	maximum_rate = .75;	/* maximum rate in mg/kg/min */
	k41 = 10;			/* no idea */
	effect_data = 0;
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	switch (kinetic_set)
		{
	case (1):			/* Rowland, Ann NY Acad Sci 179:383-398, 1971 */
		{
		strcpy(parameter_name, "Rowland's");
		if (infusate_concentration == 0)
			infusate_concentration = 20;
		vc = 34.5;
		k10 = 0.0253;
		k12 = 0.0576;
		k13 = 0.0;
		k21 = 0.0328;
		k31 = 0.0;
		break;
		}
	case (2):			/* Vozeh/Maitre (Pierre: what is the reference?) */
		{
		strcpy(parameter_name, "Vozeh's");
		if (infusate_concentration == 0)
			infusate_concentration = 20;	/* Pierre: 20 mg/ml OK? */
		bayes_data = 2;
		/* in globals:		heart_failure = 2; */
		while (heart_failure > 1 || heart_failure < 0)
			{
			gotoxy(0, 16);
			printf("1 = heart failure, 0 = no failure:    ");
			heart_failure = (int) enter_real(0.0, 35, 16);
			}
		vc = (0.67 - 0.16 * heart_failure) * mass;
		cl1 = (0.58 - 0.26 * heart_failure) * mass;
		k10 = cl1 / vc / 60;	/* convert to /minute */
		k12 = 1.30 / 60;	/* convert to /minute */
		k13 = 0.0;
		k21 = 0.53 / 60;	/* convert to /minute */
		k31 = 0.0;
		break;
		}
	case (3):			/* Rat kinetics: initially Rowland, pending more */
						/* Data from Chaplin and Yaksh */
		{
		strcpy(parameter_name, "Rowland, scaled");
		if (infusate_concentration == 0)
			infusate_concentration = 5;	/* Sandy: 20 mg/ml OK? */
		vc = 1.64 * mass;
		k10 = 0.0253;
		k12 = 0.0576;
		k13 = 0.0;
		k21 = 0.0328;
		k31 = 0.0;
		break;
		}
	case (4):			/* Schnider Pain Clinic PK/PD patients */
		{
		strcpy(parameter_name, "Schnider");
		if (infusate_concentration == 0)
			infusate_concentration = 20;	/* Tom: 20 mg/ml OK? */
		vc = 0.088 * mass;
		k10 = 0.227273;
		k12 = 0.636364;
		k13 = 0.0;
		k21 = 0.14;
		k31 = 0.0;
		break;
		}
		}
	}					/* lidocaine */

void ketamine()
	{					/* ketamine */
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Ketamine kinetic set:\n");
		printf("     1 = Human\n");
		printf("     2 = Horse\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	max_concentration = 10;	/* max reasonable Cp */
	awake_concentration = .25;	/* Desired Cp on emergence */
	maximum_rate = .75;	/* maximum rate in mg/kg/min */
	k41 = 10;			/* no idea */
	effect_data = 0;
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	switch (kinetic_set)
		{
	case (1):			/* Domino et al. Clin Pharmacol Ther 36:645-653, 1991 */
		{
		strcpy(parameter_name, "Domino's");
		if (infusate_concentration == 0)
			infusate_concentration = 100;
		vc = .063 * mass;
		k10 = 0.438;
		k12 = 0.592;
		k13 = 0.590;
		k21 = 0.247;
		k31 = 0.0146;
		break;
		}
	case (2):			/* Waterman A.E., Robertson J.G. Lane */
						/* Research in Vet Science 42:162-166, 1987 */
		{
		strcpy(parameter_name, "Waterman's horse PK");
		if (infusate_concentration == 0)
			infusate_concentration = 100;
		vc = .492 * mass;
		k10 = 0.0722;
		k12 = 0.1638;
		k13 = 0.0;
		k21 = 0.0423;
		k31 = 0.0;
		break;
		}
		}
	}					/* ketamine */

void methohexital()
	{					/* methohexital */
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Methohexital kinetic set:\n");
		printf("     1 = Hudson and Stanski\n");
		printf("     2 = Bloom\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	if (infusate_concentration == 0)
		infusate_concentration = 10;
	max_concentration = 100;	/* max reasonable Cp */
	awake_concentration = 2;	/* Desired Cp on emergence */
	maximum_rate = 10;	/* maximum rate in mg/kg/min */
	k41 = .693 / 1.2;	/* from Homer & Stanski for thiopental */
	effect_data = 1;
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	switch (kinetic_set)
		{
	case (1):			/* Hudson, Stanski, & Burch */
						/* Anesthesiology 59:215-219, 1983 */
		{
		strcpy(parameter_name, "Hudson's");
		vc = .35 * mass;
		k10 = 0.0311;
		k12 = 0.0596;
		k13 = 0.016;
		k21 = 0.0267;
		k31 = 0.00525;
		break;
		}
	case (2):			/* Bloom, initial data analysis of first 2 subjects */
		{
		strcpy(parameter_name, "Bloom's");
		vc = .0495 * mass;
		k10 = 0.010646;
		k12 = 0.173737;
		k13 = 0.383838;
		k21 = 0.049143;
		k31 = 0.002259;
		break;
		}
		}
	}					/* methohexital */

void etomidate()
	{					/* etomidate */
	kinetic_set = 1;
	if (infusate_concentration == 0)
		infusate_concentration = 2;
	max_concentration = 10;	/* max reasonable Cp */
	awake_concentration = .2;	/* Desired Cp on emergence */
	maximum_rate = 1;	/* maximum rate in mg/kg/min */
	k41 = .693/1.6; /* Arden et all, Anesthesiology 65 */
	effect_data = 1;
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	switch (kinetic_set)
		{
	case (1):			/* Arden, Holley, Stanski */
						/* Anesthesiology 65:19-27, 1986 */
		{
		strcpy(parameter_name, "Arden's");
		vc = .090 * mass;
		k10 = 0.205;
		k12 = 0.284;
		k13 = 0.209;
		k21 = 0.131;
		k31 = 0.00479;
		break;
		}
		}
	}					/* etomidate */

void methadone()
	{					/* methadone */
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Methadone kinetic set:\n");
		printf("     1 = Inturissi\n");
		printf("     2 = Schnider\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	max_concentration = 2000;	/* max reasonable Cp */
	awake_concentration = 1.0;	/* Desired Cp on emergence */
	maximum_rate = 1000;	/* maximum rate in ug/kg/min */
	k41 = 10;			/* no idea */
	effect_data = 0;
	strcpy(conc_units, "ng");
	strcpy(infused_units, "ug");
	switch (kinetic_set)
		{
	case (1):	/*  Inturissi, et al, Clin Pharm Ther 41:1987 392-401 */
				/*  Median values from table 2 (mean values give internally */
				/*  inconsistent results) */
		{
		strcpy(parameter_name, "Inturissi's");
		if (infusate_concentration == 0)
			infusate_concentration = 2000;
		vc  = 0.18 * mass;
		k10 = 0.00806;
		k12 = 0.408;
		k13 = 0.112;
		k21 = 0.06;
		k31 = 0.00114;
		bayes_data = 3;
		break;
		}
	case (2):	/* Schnider results, from Stanford Pain Clinic */
				/* NPD results */
		{
		strcpy(parameter_name, "Schnider's");
		if (infusate_concentration == 0)
			infusate_concentration = 2000;
		vc = .57 * mass;
		k10 = 0.004;
		k12 = 0.1035;
		k13 = 0.01667;
		k21 = 0.018438;
		k31 = 0.002111;
		bayes_data = 4;
		prior[1] = vc * k10;	/* clearance */
		prior[2] = vc;			/* volume */
		break;
		}
		}
	}					/* methadone */

void pancuronium()
	{					/* pancuronium */
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Pancuronium kinetic set:\n");
		printf("     1 = Rupp: Young\n");
		printf("     2 = Rupp: Elderly\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	if (infusate_concentration == 0)
		infusate_concentration = 1;
	max_concentration = .500;	/* max reasonable Cp 			*/
	maximum_rate = 10;		/* maximum rate in mg/kg/min 	*/
	effect_data = 1;
	pd_data = 1;
	strcpy(effect_text, "blockade");
	strcpy(effect_units, "percent");
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	bayes_data = 4;		/* Generic volume/clearance model */
	switch (kinetic_set)
		{
	case (1):	/*  Rupp et all, Anesthesiology 67:45-49, 1987 */
		{
		strcpy(parameter_name, "Rupp: young");
		vc =  0.043 * mass;
		k10 = 0.03488372;
		k12 = 0.08374754;
		k13 = 0.04842603;
		k21 = 0.09684828;
		k31 = 0.01579709;
		k41 = 0.173;
		e0 = 0;
		emax = 100;
		gamma = 4.77;
		ic50 = .107;
		ic50_gamma = pow(ic50, gamma);
		break;
		}
	case (2):	/*  Rupp et all, Anesthesiology 67:45-49, 1987 */
		{
		strcpy(parameter_name, "Rupp: elderly");
		vc =  0.057 * mass;
		k10 = 0.02105263;
		k12 = 0.09964047;
		k13 = 0.03021546;
		k21 = 0.10132721;
		k31 = 0.01641067;
		k41 = 0.135;
		e0 = 0;
		emax = 100;
		gamma = 4.86;
		ic50 = .123;
		ic50_gamma = pow(ic50, gamma);
		break;
		}
		}
	awake_concentration = inverse_pd_model(80.0);	/* Desired Cp on emergence 		*/
	/* bayesian stuff */
	prior[1] = vc * k10;	/* clearance */
	prior[2] = vc;			/* volume */
	}					/* pancuronium */

void vecuronium()
	{					/* vecuronium */
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Vecuronium kinetic set:\n");
		printf("     1 = Rupp: Young\n");
		printf("     2 = Rupp: Elderly\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	if (infusate_concentration == 0)
		infusate_concentration = 1;
	max_concentration = .500;	/* max reasonable Cp 			*/
	maximum_rate = 10;		/* maximum rate in mg/kg/min 	*/
	if (bard_flag == 1)
		maximum_rate = 0.050;
	effect_data = 1;
	pd_data = 1;
	strcpy(effect_text, "blockade");
	strcpy(effect_units, "percent");
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	bayes_data = 4;		/* Generic volume/clearance model */
	switch (kinetic_set)
		{
	case (1):	/*  Rupp et all, Anesthesiology 67:45-49, 1987 */
		{
		strcpy(parameter_name, "Rupp: young");
		vc =  0.052 * mass;
		k10 = 0.1;
		k12 = 0.1436;
		k13 = 0.03436;
		k21 = 0.1102;
		k31 = 0.014384;
		k41 = 0.169;
		e0 = 0;
		emax = 100;
		gamma = 5.80;
		ic50 = .092;
		ic50_gamma = pow(ic50, gamma);
		break;
		}
	case (2):	/*  Rupp et all, Anesthesiology 67:45-49, 1987 */
		{
		strcpy(parameter_name, "Rupp: elderly");
		vc =  0.029 * mass;
		k10 = 0.1276;
		k12 = 0.32322;
		k13 = 0.08267;
		k21 = 0.2266;
		k31 = 0.02207;
		k41 = 0.165;
		e0 = 0;
		emax = 100;
		gamma = 4.84;
		ic50 = .106;
		ic50_gamma = pow(ic50, gamma);
		break;
		}
		}
	awake_concentration = inverse_pd_model(80.0);	/* Desired Cp on emergence 		*/
	/* for bayesian updating */
	prior[1] = vc * k10;	/* clearance */
	prior[2] = vc;			/* volume */
	}					/* vecuronium */

void atracurium()
	{					/* atracurium */
	/*
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Atracurium kinetic set:\n");
		printf("     1 = Marathe\n");
		printf("     2 = ?????\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	*/
	kinetic_set = 1;
	if (infusate_concentration == 0)
		infusate_concentration = 10;
	max_concentration = 2.000;	/* max reasonable Cp 			*/
	maximum_rate = 3;		/* maximum rate in mg/kg/min 	*/
	if (bard_flag == 1)
		maximum_rate = .4;
	effect_data = 1;
	pd_data = 1;
	strcpy(effect_text, "blockade");
	strcpy(effect_units, "percent");
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	bayes_data = 4;		/* Generic volume/clearance model */
	switch (kinetic_set)
		{
	case (1):	/*  Marathe et all, Anesthesiology 70:752-755, 1989 */
		{
		strcpy(parameter_name, "Marathe");
		vc =  0.066 * mass;
		k10 = 0.087776;
		k12 = 0.059124;
		k13 = 0;
		k21 = 0.081600;
		k31 = 0;
		k41 = 0.074;
		e0 = 0;
		emax = 100;
		gamma = 4.55;
		ic50 = .669;
		ic50_gamma = pow(ic50, gamma);
		break;
		}
		}
	awake_concentration = inverse_pd_model(80.0);	/* Desired Cp on emergence 		*/
	/* for bayesian updating */
	prior[1] = vc * k10;	/* clearance */
	prior[2] = vc;			/* volume */
	}						/* atracurium */
void rocuronium()
	{					/* rocuronium */
	double v2, v3, cl1, cl2, cl3;
	
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Rocuronium kinetic set:\n");
		printf("     1 = Szenohradszky (pk only)\n");
		printf("     2 = Plaud (pk\\pd model)\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	
	if (infusate_concentration == 0)
		infusate_concentration = 10;
	max_concentration = 10.000;	/* max reasonable Cp 			*/
	maximum_rate = 10;		/* maximum rate in mg/kg/min 	*/
	effect_data = 0;
	pd_data = 0;
	strcpy(effect_text, "blockade");
	strcpy(effect_units, "percent");
	strcpy(conc_units, "ug");
	strcpy(infused_units, "mg");
	bayes_data = 0;		/* Generic volume/clearance model */
	switch (kinetic_set)
		{
	case (1):	/*  Szenohradszky et all, Anesthesiology 77:899-904, 1992 */
		{
		strcpy(parameter_name, "Szenohradszky");

		vc =  0.077 * mass;
		v2 =  0.050 * mass;
		v3 =  0.080 * mass;
		cl1 = 0.00289 * mass;
		cl2 = 0.00879 * mass;
		cl3 = 0.00151 * mass;

		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;

		k41 = 10;
		/*
		k41 = 
		e0 = 0;
		emax = 100;
		gamma = 
		ic50 = .
		ic50_gamma = pow(ic50, gamma);
		*/
		awake_concentration = 0.5;
		break;
		}
	case (2):	/*  Plaud,  CPT, in press */
		{
		strcpy(parameter_name, "Plaud");

		vc =  0.056 * mass;
		k10 = 0.1746;
		k12 = 0.100381;
		k13 = 0;
		k21 = 0.0245;
		k31 = 0;

		k41 = 0.168;
		e0 = 0;
		emax = 100;
		gamma = 4.79;
		ic50 = 0.823;
		ic50_gamma = pow(ic50, gamma);
		awake_concentration = inverse_pd_model(80.0);

		bayes_data = 1;
		effect_data = 1;
		pd_data = 1;
		prior[1] = vc * k10;	/* clearance */
		prior[2] = vc;			/* volume */
		break;
		}
		}
	}						/* rocuronium */

void lorazepam()
	{					/* lorazepam */
	if (lorazepam_study_flag == 1)
		{
		kinetic_set = 2;
		}
	while (kinetic_set < 1 || kinetic_set > 2)
		{
		gotoxy(0, 6);
		printf("Lorazepam kinetic set:\n");
		printf("     1 = Greenblatt, 1979\n");
		printf("     2 = Zomorodi, 1997 VA\n");
		printf("     Your selection:     ");
		kinetic_set = (int) enter_real(0.0, 21, 9);
		}
	max_concentration = 1600.0;	/* max reasonable Cp */
	awake_concentration = 120;	/* Desired Cp on emergence */
	maximum_rate = 100;	/* maximum rate in ug/kg/min */
	k41 = .693 / 1.2;
	strcpy(conc_units, "ng");
	strcpy(infused_units, "ug");
	effect_data = 0;
	switch (kinetic_set)
		{
	case (1):		
		{ // From Greenblatt, J Pharm Sci: 68:57-63, 1979
		  // Based on geometric mean of individual values of V1, k10, k12, k13
		  // Spreadsheet = lorazepam.xls
		strcpy(parameter_name, "Greenblatt, 1979");
		if (infusate_concentration == 0)
			infusate_concentration = 2000;
		vc = 0.4598 * mass;
		k10 = 0.00223;
		k12 = 0.092677;
		k13 = 0.0;
		k21 = 0.07166;
		k31 = 0.0;
		break;
		}
	case (2):		
		{ // Midazolam/Lorazepam comparison trial, no covariate fit
		strcpy(parameter_name, "Zomorodi, 1997");
		if (infusate_concentration == 0)
			infusate_concentration = 2000;
		vc = 41;
		k10 = 0.00276;
		k12 = 0.04366;
		k13 = 0.0;
		k21 = 0.01738;
		k31 = 0.0;
		break;
		}
		}
	if (lorazepam_study_flag == 1)
		{
		effect_data = 0;
		infusate_concentration = 1000;
		}
	}					/* lorazepam */

void remifentanil()
	{					/* remifentanil */
	double v2, v3, cl1, cl2, cl3;
	kinetic_set = 1;	/* only 1 kinetic set so far */
	max_concentration = 200.0;	/* max reasonable Cp */
	awake_concentration = 3;	/* Desired Cp on emergence */
	maximum_rate = 100;	/* maximum rate in ug/kg/min */
	k41 = .693 / 1.2;
	strcpy(conc_units, "ng");
	strcpy(infused_units, "ug");
	switch (kinetic_set)
		{
	case (1):		
		{
		strcpy(parameter_name, "Minto/Schnider");
		if (infusate_concentration == 0)
			infusate_concentration = 50;
		vc = 5.1-0.0201*(age-40)+0.072*(lbm-55);
		v2=9.82-0.0811*(age-40)+0.108*(lbm-55);
		v3=5.42;
		cl1=2.6-0.0162*(age-40)+0.0191*(lbm-55);
		cl2=2.05-0.0301*(age-40);
		cl3=0.076-0.00113*(age-40);
		
		k10 = cl1 / vc;
		k12 = cl2 / vc;
		k13 = cl3 / vc;
		k21 = cl2 / v2;
		k31 = cl3 / v3;

		k41 = 0.595-0.007*(age-40);
		
		break;
		}
		}
	}					/* remifentanil */




void external()
	{					/* external */
	int flag;
	FILE *kin_file;
	do	{
		gotoxy(0, 10);
		printf("External file: ");
		if (kinetic_file[0] == 0)
			scanf("%s", kinetic_file);
		else
			printf("%s\n", kinetic_file);
		kin_file = fopen(kinetic_file, "r");
		if (kin_file != (FILE *) 0)
			{
	        /* File Format:
    	        Line    Description
        	    1       drug name
            	2       parameter name
	            3       species identification
    	        4       units flag:
        	                0:  plasma:         ng
            	                infusate:       ug
                	        1:  plasma:         ug
                    	        infusate:       mg
	            5       Effect Flag:
    	                    0 = no effect data
        	                1 = effect data
            	6       Weight Scale Flag
                	        0 = don't scale vc to weight
                    	    1 = scale vc to weight
	            7       infusate concentration (in units, as above)
    	        8       maximum permissible concentration
        	    9       desired awakening concentration
	            10      maximum rate (in infusate units/kg/min)
    	        11      vc
        	    12      k10
	            13      k12
    	        14      k13
        	    15      k21
            	16      k31
	            17      ke0
    		        */

			fscanf(kin_file, "%s\n", drug_name[max_drug_id]);
			printf("Drug referenced in %s: %s\n", kinetic_file, drug_name[max_drug_id]);
			fscanf(kin_file, "%s\n", &parameter_name[0]);
			fscanf(kin_file, "%s\n", &species[0]);
			fscanf(kin_file, "%d", &flag);
			if (flag == 1)
				{
				strcpy(conc_units, "ug");
				strcpy(infused_units, "mg");
				}
			fscanf(kin_file, "%d", &effect_data);
			fscanf(kin_file, "%d", &scaled_mass_flag);
			fscanf(kin_file, "%lf", &infusate_concentration);
			fscanf(kin_file, "%lf", &max_concentration);
			fscanf(kin_file, "%lf", &awake_concentration);
			fscanf(kin_file, "%lf", &maximum_rate);
			fscanf(kin_file, "%lf", &vc);
			fscanf(kin_file, "%lf", &k10);
			fscanf(kin_file, "%lf", &k12);
			fscanf(kin_file, "%lf", &k13);
			fscanf(kin_file, "%lf", &k21);
			fscanf(kin_file, "%lf", &k31);
			if (effect_data == 1)
				fscanf(kin_file, "%lf", &k41);
			else
				k41 = 10;
			fclose(kin_file);
			}
		else
			{
			cls();
			gotoxy(0, 9);
			printf("No such drug input file: %s\n", kinetic_file);
			kinetic_file[0] = 0;
			}
		}
	while (kin_file == (FILE *) 0);
	if (scaled_mass_flag == 1)
		vc *= mass;
	}					/* external */

