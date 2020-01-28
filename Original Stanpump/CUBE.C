#include <math.h>
void swap();

void cube(k10,k12,k21,k13,k31,r)
double k10, k12, k21, k13, k31;
double *r;
	{					/* cube */
	double a0, a1, a2;	/* factors in cubic equation */
	double p, q;		/* factors in transformed equation */
	double phi;			/* used for root solving */
	double r1;			/* also used for root solving */
	double toradian;	/* mathematical conversion from degrees to radians */

	toradian = asin(1.0) * 2.0 / 180.0;	/* pi/180 */

	if (k31 > 0)
		{
	    /* first take roots of X^3 + a2X^2 + a1X^1 + a0 = 0 */
    	/* where the coefficients are : */
		a0 = k10 * k21 * k31;
		a1 = k10 * k31 + k21 * k31 + k21 * k13 + k10 * k21 + k31 * k12;
		a2 = k10 + k12 + k13 + k21 + k31;

	    /* now transform to x^3 + px + q = 0 */
		p = a1 - (a2 * a2 / 3.0);
		q = (2 * a2 * a2 * a2 / 27.0) - (a1 * a2 / 3.0) + a0;
		r1 = sqrt(-(p * p * p) / 27.0);
		phi = (-q / 2.0) / r1;
		if (phi > 1)
			phi = 1;
		else if (phi < -1)
			phi = -1;
		phi = (acos(phi) / 3.0);
		r1 = 2.0 * exp(log(r1) / 3.0);
		r[1] = -(cos(phi) * r1 - a2 / 3.0);
		r[2] = -(cos(phi + 120.0 * toradian) * r1 - a2 / 3.0);
		r[3] = -(cos(phi + 240.0 * toradian) * r1 - a2 / 3.0);
		}
	else
		{
		if (k21 > 0)
			{
		    /* first take roots of X^2 - a1X^1 + a0 = 0 */
    		/* where the coefficients are : */
			a0 = k10 * k21;
			a1 = -(k10 + k12 + k21);
			r[1] = (-a1 + sqrt(a1 * a1 - 4 * a0)) / 2;
			r[2] = (-a1 - sqrt(a1 * a1 - 4 * a0)) / 2;
			r[3] = 0;
			}
		else
			{
			/* one compartment model */
			r[1] = k10;
			r[2] = 0;
			r[3] = 0;
			}
		}

    /* sort - nothing fancy is needed */
	if (r[2] > r[1])
		swap(&r[2], &r[1]);
	if (r[3] > r[1])
		swap(&r[3], &r[1]);
	if (r[3] > r[2])
		swap(&r[3], &r[2]);
	}					/* cube */

void swap(a, b)
double *a, *b;
	{
	double temp;
	temp = *a;
	*a = *b;
	*b = temp;
	}

