 /***************** BAYES.C  *********************************
 * Bayesian optimization written by PIERRE O. MAITRE,        *
 * Service d'Anesthesiologie, Clinique de Genolier,          *
 * CH-1261 GENOLIER,  and Unite de Pharmacologie Clinique,   *
 * University Hospital of Geneva, Switzerland                *
 * and Steven Shafer, Stanford University                    *
 ************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "stanpump.h"

/*************** Bayesian subroutines ***********************************/
void save_infusion();		/* saves infusion for bayesian regression   */
void bayes();				/* bayesian master routine                  */
void bayes_setup();			/* actually set up parameter values         */
void initialize_bayes();	/* set initial bayesian state variables     */
double funk();				/* returns y for given estimate of theta    */
void bayes_model();			/* analytical solution to pk model          */
void amoeba();				/* search routine                           */
void cube();				/* convert k values to lambda values        */

/*************** Global variables  for Bayes ****************************/
int n_dim;					/* number of parameters estimated           */
int n_measure = 0;			/* number of measured levels                */
double v1;					/* estimate of v1                           */
double b_lambda[5];			/* lambda of b_kxx values                   */
double v_temp[4];			/* amount in each virtual compartment       */
double _far b_time[3000];	/* time each infusion started               */
double _far b_rate[3000];	/* rate of each infusion                    */
int n_infs = 0;				/* number of infusions to present time      */
double b_tm[62];			/* times of measurements                    */
double b_cpm[62];			/* values of measurements                   */
double b_cpp[62];			/* values of predictions of measurements    */
int first_displayed;		/* first measured value on screen			*/
double b_k10;				/* Rate constants for bayes routines        */
double b_k12;
double b_k13;
double b_k21;
double b_k31;
double theta[10];			/* pk parameter to be estimated             */
double sigma;				/* residual variability                     */
double ie[5][5];			/* array for theta values                   */
double pop[5];				/* array for population mean estimates      */
double omega[5];			/* variance around each parameter estimate  */
double pp[5][5];			/* working space for parameter estimates	*/
double prior[5];			/* prior population estimate for parameters	*/
int bayes_processing_flag;	/* 0 = not processing, 1 = initializing     */
                            /* 2 = simplex,        3 = updating         */
double bayes_time;			/* Time of blood sample                     */
double bayes_level;			/* Measured level                           */
int bayes_data;				/* 0 = no data; other = data set            */
int n_funk;					/* number of calls to funk                  */
double c[4];				/* coefficients for 3 compartment model     */

void bayes()
	{					/* bayes , called from keyin */
	int i, n, j, ic, measured;
	double ftol = 0.1e-04;
	double p1, p2, p3, es1, es2, es3, es4;	/* used to rapidly update model */
	static double y[10];
	static int flag_new;
	double b_interval, b_rate1;
	double l1t, l2t, l3t, l4t;
	double inf_start, inf_stop;

	switch (bayes_processing_flag)
		{
	case (1):
		{
		bayes_setup();	/* set up array values */
		n_funk = 0;

		initialize_bayes();
        /*    calculate the value of the objective function for each */
        /*    pair of initial estimates for the parameters theta:*/
        /* Pierre: here I have taken the liberty of generalizing the code */
        /* in terms of n_dim rather than assume n_dim = 2 */
		for (i = 1;  i <= 3;  i++)
			{
			for (n = 1;  n <= n_dim;  n++)
				{
				theta[n] = pp[i][n];
				}
			y[i] = funk(theta);
			}
		flag_new = 1;	/* used in amoeba */
		if (n_measure > 0)
			bayes_processing_flag = 2;	/* signal we are initialized */
		else
			{
			bayes_processing_flag = 3;	/* restore original pharmacokinetics */
			for (i = 1; i <= n_dim; i++)
				pp[1][i] = pop[i];
			}
		break;
		}
	case (2):
		{
		amoeba(y, ftol, &flag_new);
		if (flag_new == 2)
			{
			bayes_processing_flag = 0;
			display_prompts();
            /* flag_new = 1; */
			break;
			}
		if (flag_new == 1)
			bayes_processing_flag = 3;	/* signal we are done */
		break;
		}
	case (3):
		{
        /* update current condition in compartments 1, 2, 3 */
        /* according to new bayesian parameters */
        /* update the parameters for stanpump */
		k10 *= 60.0;
		k12 *= 60.0;
		k13 *= 60.0;
		k21 *= 60.0;
		k31 *= 60.0;
		k41 *= 60.0;
        /* Plug in new estimates */
		switch (bayes_data)
			{
		case (1):		/* Alfentanil 										*/
		case (2):		/* Lidocaine (same parameterization as alfentanil) 	*/
		case (3):		/* Methadone (also same parameterization) 			*/
		case (4):		/* Generic volume, clearance model					*/
			{
			vc = pp[1][2];
			k10 = pp[1][1] / vc;
			break;
			}
		default:
			{
			}
			}
		ve = vc * .0001;
		calculate_udfs();
		p1 = 0;
		p2 = 0;
		p3 = 0;
		es1 = 0;
		es2 = 0;
		es3 = 0;
		es4 = 0;
		j = 1;
		ic = 0;
		inf_start = b_time[0];
		/* revised to display final estimates */
		while (ic <= n_infs)
			{
			main_loop();	/* continue to service pump and user */
			inf_stop = b_time[ic + 1];
			b_rate1 = b_rate[ic] / 60.0;
			if (j <= n_measure && inf_stop >= b_tm[j])
				{
				measured = 1;
				b_interval = b_tm[j] - inf_start;
				inf_start = b_tm[j];
				}
			else
				{
				measured = 0;
				b_interval = inf_stop - inf_start;
				inf_start = inf_stop;
				ic++;
				}
			b_interval *= 60.0;
			l1t = exp(-lambda[1] * b_interval);
			l2t = exp(-lambda[2] * b_interval);
			l3t = exp(-lambda[3] * b_interval);
			l4t = exp(-lambda[4] * b_interval);
			p1 = p1 * l1t + p_coef[1] * b_rate1 * (1 - l1t);
			p2 = p2 * l2t + p_coef[2] * b_rate1 * (1 - l2t);
			p3 = p3 * l3t + p_coef[3] * b_rate1 * (1 - l3t);
			if (effect_data)
				{
				es1 = es1 * l1t + e_coef[1] * b_rate1 * (1 - l1t);
				es2 = es2 * l2t + e_coef[2] * b_rate1 * (1 - l2t);
				es3 = es3 * l3t + e_coef[3] * b_rate1 * (1 - l3t);
				es4 = es4 * l4t + e_coef[4] * b_rate1 * (1 - l4t);
				}
			if (measured)
				{
				b_cpp[j] = p1 + p2 + p3;
				if (j >= first_displayed && bayes_processing_flag > 0)
					{
					gotoxy(51, 4 + j - first_displayed);
					if (pd_data == 0)
						printf("%8.2lf %9.3lf  %9.3lf", b_tm[j], b_cpm[j], b_cpp[j]);
					else
						printf("%8.2lf %9.3lf  %9.3lf", b_tm[j], 
							pd_model(b_cpm[j]), pd_model(b_cpp[j]));
					}
				j++;
				}
			}
		p_state[1] = p1;
		p_state[2] = p2;
		p_state[3] = p3;
		e_state[1] = es1;
		e_state[2] = es2;
		e_state[3] = es3;
		e_state[4] = es4;

        /* refresh message */
		printf("\7");
		gotoxy(55, 0);
		printf("                    ");
		gotoxy(66, 2);
		printf("Done          ");
		gotoxy(80 - strlen(pump_name[pump_type]), 0);
		printf("%s", pump_name[pump_type]);
		if (p_state[1] < 0 || p_state[2] < 0 || p_state[3] < 0)
			{
			gotoxy(72,2);
			printf("(recirc)");
			}

        /* change the target concentration to the predicted value */
        /* to avoid any abrupt change at conclusion of estimation */

		desired = p_state[1] + p_state[2] + p_state[3];
		if (pd_data)
			desired_effect = pd_model(desired);
		display_desired();

        /* Adjust infusion data now */
		infuse();
		bayes_processing_flag = 0;
		clear_prompt_area();
		display_prompts();
		/* display new PK values */
		break;
		}
	default:
		{
		}
		}
	}					/* bayes */

void bayes_setup()
	{						/* bayes_setup */
    /* initial estimates for triangulation */
	switch (bayes_data)
		{
	case (1):			/* Maitre Alfentanil Parameters */
		{
		n_dim = 2;
		ie[1][1] = 0.356;
		ie[2][1] = 0.495;
		ie[3][1] = 0.746;
		ie[1][2] = 10.8;
		ie[2][2] = 4.0;
		ie[3][2] = 7.0;

        /* set population estimates */
		if (age <= 40)
			{
			pop[1] = 0.356;		/* Clearance */
			}
		else
			{
			pop[1] = 0.356 - (.00269 * (age - 40.0));	/* Clearance */
			}
		if (gender == 1)
			pop[2] = .111 * mass;  /* V1 */
		else
			pop[2] = .111 * 1.15 * mass;  /* V1 */
            /* Set error terms */
		omega[1] = 0.154;	/* Clearance */
		omega[2] = 0.081;	/* omega and sigma are the inter and intra-    */
		sigma = 0.049;	/* individual variability terms for alfentanil */
		break;
		}
	case (2):			/* Lidocaine, Vozeh, via Maitre */
		{
		n_dim = 2;
		ie[1][1] = 0.2;
		ie[2][1] = 0.37485;
		ie[3][1] = 0.8;
		ie[1][2] = 60.0;
		ie[2][2] = 35.7;
		ie[3][2] = 20.0;

            /* set population estimates */
		pop[1] = (0.58 - 0.26 * heart_failure) * mass / 60;
		pop[2] = (0.67 - 0.16 * heart_failure) * mass;
            /* Set error terms */
		omega[1] = 0.22; 	/* Pierre: the /60 has been removed */
		omega[2] = 0.10;
		sigma = 0.05;	/* individual variability terms for alfentanil */
		break;
		}
	case (3):			/* Methadone, Inturissi, no actual priors */
		{
		n_dim = 2;
		ie[1][1] = 0.057;	/* Clearance, l/m */
		ie[2][1] = 0.122;
		ie[3][1] = 0.224;
		ie[1][2] = 30;		/* Vc */
		ie[2][2] = 12;
		ie[3][2] = 5;

        /* set population estimates */
		pop[1] = 0.00806 * .18 * mass;
		pop[2] = 0.18 * mass;
         /* Set error terms */
		omega[1] = .26;	/* Assume 30% variability, ln(1.30) = .26 */
		omega[2] = .26; /* Assume 30% variability, ln(1.30) = .26 */
		sigma = 0.05;	/* individual variability terms for methadone */
		break;
		}
	case (4):			/* Generic volume, clearance */
		{
		n_dim = 2;
		ie[1][1] = prior[1] / 2;	/* Clearance, l/m */
		ie[2][1] = prior[1];
		ie[3][1] = prior[1] * 2;
		ie[1][2] = prior[2] / 2;		/* Vc */
		ie[2][2] = prior[2];
		ie[3][2] = prior[2] * 2;

        /* set population estimates */
		pop[1] = prior[1];	/* initial estimates */
		pop[2] = prior[2];  /* initial estimates */
         /* Set error terms */
		omega[1] = .26;	/* Assume 30% variability, ln(1.30) = .26 */
		omega[2] = .26; /* Assume 30% variability, ln(1.30) = .26 */
		sigma = 0.05;	/* individual variability terms for methadone */
		break;
		}
	default:
		{
		}
		}

	}					/* bayes_setup */

void initialize_bayes()
	{					/* initialize_bayes */
	int i, j;

	b_k12 = k12 * 60;
	b_k21 = k21 * 60;
	b_k13 = k13 * 60;
	b_k31 = k31 * 60;

	/* determine screen lines */
	if (n_measure < 20)
		first_displayed = 1;
	else
		first_displayed = n_measure - 19;
	gotoxy(55, 3);
	printf("Time  Measured  Predicted");

    /* restore starting triangulation estimates into pp[][] */
	for (j = 1;  j <= n_dim;  j++)
		{
		for (i = 1;  i <= 3;  i++)
			{
			pp[i][j] = ie[i][j];
			}
		}
	}					/* end initialize_bayes */

/******************  AMOEBA  ********************************************/
/*                         from Numerical Recipes in C , pages 305-309  */
/************************************************************************/
#define NMAX 200
#define ALPHA 1.0
#define BETA 0.5
#define GAMMA 2.0

void amoeba(y, ftol, flag_new)
double y[], ftol;
int *flag_new;
	{					/* amoeba */
	static int ilo, ihi, inhi;
	static double ytry, ysave, sum, rtol;
	static double psum[10];
	double amotry();
	int i, ii, j, mpts = n_dim + 1;

	if (*flag_new > 0)
		{
		for (j = 1;  j <= n_dim;  j++)
			{
			sum = 0.0;
			for (i = 1;  i <= mpts;  i++)
				sum += pp[i][j];
			psum[j] = sum;
			}
		}

	for (ii = 1;  ii <= 1;  ii++)	/* do 1 iteration and come back later */
		{
		ilo = 1;
        /* Pierre: this next line appears specific to n_dim = 2.  If */
        /* so, could you please generalize it in terms of n_dim? */
		ihi = y[1] > y[2] ? (inhi = 2,  1) : (inhi = 1,  2);
		for (i = 1;  i <= mpts;  i++)
			{
			if (y[i] < y[ilo])
				ilo = i;
			if (y[i] > y[ihi])
				{
				inhi = ihi;
				ihi = i;
				}
			else if (y[i] > y[inhi])
				if (i != ihi)
					inhi = i;
			}
		rtol = 2.0 * fabs(y[ihi] - y[ilo]) / (fabs(y[ihi]) + fabs(y[ilo]));

		if (rtol < ftol)
			{
			*flag_new = 1;	/* end of iteration process = set up
			                        for a new bayesian process */
			break;
			}

		if (n_funk >= NMAX)
			{
			gotoxy(55, 2);
			printf("Too many iterations     ");
			*flag_new = 2;
			break;
			}
		ytry = amotry(y, psum, ihi, -ALPHA);
		if (ytry <= y[ilo])
			{
			ytry = amotry(y, psum, ihi, GAMMA);
			}
		else if (ytry >= y[inhi])
			{
			ysave = y[ihi];
			ytry = amotry(y, psum, ihi, BETA);
			if (ytry >= ysave)
				{
				for (i = 1;  i <= mpts;  i++)
					{
					if (i != ilo)
						{
						for (j = 1;  j <= n_dim;  j++)
							{
							psum[j] = 0.5 * (pp[i][j] + pp[ilo][j]);
							pp[i][j] = psum[j];
							}
						y[i] = funk(psum);
						}
					}
				for (j = 1;  j <= n_dim;  j++)
					{
					sum = 0.0;
					for (i = 1;  i <= mpts;  i++)
						sum += pp[i][j];
					psum[j] = sum;
					}
				}
			}
		*flag_new = 0;
		}
	}					/* amoeba */

double amotry(y, psum, ihi, fac)
double *y, *psum, fac;
/* modified by P. Maitre in order to constrain parameters to be positive */
int ihi;
	{					/* amotry */
	int j;
	double fac1, fac2, ytry;
	double ptry[20];

	fac1 = (1.0 - fac) / n_dim;
	fac2 = fac1 - fac;
	for (j = 1;  j <= n_dim;  j++)
		{
		ptry[j] = psum[j] * fac1 - pp[ihi][j] * fac2;
		if (ptry[j] <= 0.0)
			{
			ptry[j] = 0.00000001;	/* modification for log
			                                       functions in funk  */
			}
		}
	ytry = funk(ptry);
	if (debug_flag)
		{
		gotoxy(50, 8);
		printf("Funk: %7.5f", ytry);
		}
	if (ytry < y[ihi])
		{
		y[ihi] = ytry;
		for (j = 1;  j <= n_dim;  j++)
			{
			psum[j] += ptry[j] - pp[ihi][j];
			pp[ihi][j] = ptry[j];
			}
		}
	return ytry;

	}					/* amotry */

#undef ALPHA
#undef BETA
#undef GAMMA
#undef NMAX

/**** FUNK ************************************************************/
/*    C language   translated from fortran 77                         */
/*    minimization function taken from : S. Vozeh et al ; Journal of  */
/*    Pharmacokinetics and Biopharmaceutics, 13:203-212 (1985)        */
/**********************************************************************/

double funk(x)
double *x;
	{					/* funk */
	int ic, i, j;
	int measured = 0;
	double ercpsq, erthsq, sumsq;
	double cp_measured;
	double cp_predicted;
	double inf_start, inf_stop;
	double k0_time;		/* interval between iterations              */
	double k0;			/* infusion rate (b_rate[]) at iteration    */

	n_funk++;
	if (bayes_processing_flag > 0)
		{
		gotoxy(66, 2);
		printf("%d", n_funk);
		}

	v_temp[1] = 0;
	v_temp[2] = 0;
	v_temp[3] = 0;

	for (i = 1;  i <= n_dim;  i++)
		{
		if (x[i] <= 0.0)
			{
			x[i] = 1.0e-8;	/* safety belt for the log functions below */
			}
		}
	switch (bayes_data)
		{
	case (1):			/* Alfentanil 	*/
	case (2):			/* Lidocaine 	*/
	case (3):			/* Methadone 	*/
	case (4):			/* Generic		*/
		{
		v1 = x[2];
		b_k10 = x[1] / x[2];
		break;
		}
	default:
		{
		}
		}
	cube(b_k10, b_k12, b_k21, b_k13, b_k31, b_lambda);
                        /* cube calculates the hybrid rate constants */
                        /* (b_lambdas) from the microconstants k */
	if (b_k31 > 0)
		{
		c[1] = (b_k21 - b_lambda[1]) * (b_k31 - b_lambda[1]) / 
			(b_lambda[1] - b_lambda[2]) / 
			(b_lambda[1] - b_lambda[3]) / 
			v1 / b_lambda[1];
		c[2] = (b_k21 - b_lambda[2]) * (b_k31 - b_lambda[2]) / 
			(b_lambda[2] - b_lambda[1]) / 
			(b_lambda[2] - b_lambda[3]) / 
			v1 / b_lambda[2];
		c[3] = (b_k21 - b_lambda[3]) * (b_k31 - b_lambda[3]) / 
			(b_lambda[3] - b_lambda[2]) / 
			(b_lambda[3] - b_lambda[1]) / 
			v1 / b_lambda[3];
		}
	else
		{
		if (b_k21 > 0)
			{
			c[1] = (b_k21 - b_lambda[1]) / (b_lambda[2] - b_lambda[1]) / v1 / b_lambda[1];
			c[2] = (b_k21 - b_lambda[2]) / (b_lambda[1] - b_lambda[2]) / v1 / b_lambda[2];
			c[3] = 0;
			}
		else
			{
			c[1] = 1 / lambda[1] / v1;	/* / lambda[1]?? */
			c[2] = 0;
			c[3] = 0;
			}
		}

	sumsq = 0.0;
	for (i = 1;  i <= n_dim;  i++)
		{
		erthsq = (log(pop[i]) - log(x[i])) * 
			(log(pop[i]) - log(x[i])) / omega[i];
		sumsq = sumsq + erthsq;
		}
	j = 1;
	ic = 0;
	inf_start = b_time[0];
	while (ic <= n_infs)
		{
		inf_stop = b_time[ic + 1];
		k0 = b_rate[ic];
		if (j <= n_measure && inf_stop >= b_tm[j])
			{
			measured = 1;
			cp_measured = b_cpm[j];
			k0_time = b_tm[j] - inf_start;
			inf_start = b_tm[j];
			}
		else
			{
			measured = 0;
			k0_time = inf_stop - inf_start;
			inf_start = inf_stop;
			ic++;
			}
        /* call the pharmacokinetic model with parameter estimates */
		bayes_model(k0_time, k0);
		if (measured)
			{
			cp_predicted = v_temp[1] + v_temp[2] + v_temp[3];
			if (j >= first_displayed && bayes_processing_flag > 0)
				{
				gotoxy(51, 4 + j - first_displayed);
				if (pd_data == 0)
					printf("%8.2lf %9.3lf  %9.3lf", b_tm[j], cp_measured, cp_predicted);
				else
					printf("%8.2lf %9.3lf  %9.3lf", b_tm[j], 
						pd_model(cp_measured), pd_model(cp_predicted));
				}
			if (cp_predicted < 0 && bayes_processing_flag > 0)
				{
				gotoxy(51, 4);
				printf("Aborted: negative prediction!");
				return 9999.99;
				}
			ercpsq = (log(cp_measured) - log(cp_predicted)) * 
				(log(cp_measured) - log(cp_predicted)) / sigma;
			sumsq = sumsq + ercpsq;
			j++;
			}
		if (j > n_measure)
			break;
		}

	return sumsq * 0.5;	/* (Vozeh uses -0.5 instead of 0.5  */
	                    /* and he maximizes the function) */
	}					/* funk */

void bayes_model(b_interval, b_infusion_rate)
double b_interval, b_infusion_rate;
	{					/* bayes_model */
	double l1t, l2t, l3t;

	main_loop();		/* keeps pump running while processing bayesian stuff */
	if (debug_flag)
		{
		gotoxy(50, 5);
		printf("L1: %12.10f\n", b_lambda[1]);
		gotoxy(50, 6);
		printf("L2: %12.10f\n", b_lambda[2]);
		gotoxy(50, 7);
		printf("L3: %12.10f\n", b_lambda[3]);
		}

	l1t = exp(-b_lambda[1] * b_interval);
	l2t = exp(-b_lambda[2] * b_interval);
	l3t = exp(-b_lambda[3] * b_interval);

	v_temp[1] = v_temp[1] * l1t + c[1] * b_infusion_rate * (1 - l1t);
	v_temp[2] = v_temp[2] * l2t + c[2] * b_infusion_rate * (1 - l2t);
	v_temp[3] = v_temp[3] * l3t + c[3] * b_infusion_rate * (1 - l3t);

	if (v_temp[1] < 0 || v_temp[2] < 0 || v_temp[3] < 0)
		{
		if (debug_flag == 1)
			{
			gotoxy (50,5);
			printf("Interval %15.10f\n", b_interval);
			gotoxy(50,6);
			printf("Rate: %15.10f\n", b_infusion_rate);
			gotoxy(50,7);
			printf("l1t: %15.10f\n", l1t);
			gotoxy(50,8);
			printf("l2t: %15.10f\n", l2t);
			gotoxy(50,9);
			printf("l3t: %15.10f\n", l3t);
			gotoxy(50,10);
			printf("c[1]: %15.10f\n", c[1]);
			gotoxy(50,11);
			printf("c[2]: %15.10f\n", c[2]);
			gotoxy(50,12);
			printf("c[3]: %15.10f\n", c[3]);
			gotoxy(50,13);
			printf("v_temp[1]: %15.10f\n", v_temp[1]);
			gotoxy(50,14);
			printf("v_temp[2]: %15.10f\n", v_temp[2]);
			gotoxy(50,15);
			printf("v_temp[3]: %15.10f\n", v_temp[3]);
			}
		else
			{
			if (bayes_processing_flag > 0)
				{
				gotoxy(72,2);
				printf("(recirc)");
				}
			}
		}
	else
		{
		if (bayes_processing_flag > 0)
			{
			gotoxy(72,2);
			printf("        ");
			}
		}
	}					/* bayes_model */

void save_infusion(seconds_on, seconds_off, amount_infused_now)
long seconds_on, seconds_off;
double amount_infused_now;
	{					/* save_infusion */

	static double initial_rate = 0;
	static double max_dif = 0;
	static double cumulative_amount = 0;
	double rate;
	double infusion_on, infusion_off;

	if (seconds_on == -1)
		return;

	if (seconds_off == seconds_on)
		{
		cumulative_amount += amount_infused_now;
		return;
		}
	infusion_on = (double) seconds_on / 60.0;
	infusion_off = (double) seconds_off / 60.0;

	rate = amount_infused_now / (infusion_off - infusion_on);

    /* first save entire infusion in drgfile */
	if (seconds_on <= 1)
		{
		b_time[n_infs] = 0.0;
		initial_rate = rate;
		b_rate[n_infs] = rate;
		b_time[n_infs + 1] = infusion_off;	/* simplifies bayesian update */
		b_rate[n_infs + 1] = 0.0;	/*            "               */
		max_dif = initial_rate * compression_factor;
		cumulative_amount = amount_infused_now;
		return;
		}
	if (n_infs > 2990)
		{
		bayes_data = 0;	/* Bayes disabled if unable to save */
		return;			/* full infusion function */
		}
    /* otherwise, check if it's outside of acceptable range limit */
	if (fabs(rate - initial_rate) > max_dif)
		{
		b_rate[n_infs] = cumulative_amount / (infusion_on - b_time[n_infs]);
		n_infs++;
		b_time[n_infs] = infusion_on;
		b_rate[n_infs] = rate;
		b_time[n_infs + 1] = infusion_off;	/* simplifies bayesian update */
		b_rate[n_infs + 1] = 0.0;	/*            "               */
		initial_rate = rate;
		max_dif = initial_rate * compression_factor;
		cumulative_amount = amount_infused_now;
		return;
		}
    /* otherwise just add the total and keep the rate updated, in case
       a bayesian regression is processing   */
	cumulative_amount += amount_infused_now;
	b_rate[n_infs] = cumulative_amount / (infusion_off - b_time[n_infs]);
	b_time[n_infs + 1] = infusion_off;
	}					/* save_infusion     */
