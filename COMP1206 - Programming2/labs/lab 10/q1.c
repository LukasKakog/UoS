#include <stdio.h>
#include <math.h>

/*
 * main function declared
 */
int CACHE(double *prod, double *sum, double d1, double d2);

/*
 * 1 is returned if the two given doubles ahve the same sign
 * otherwise returns -1
 */
int CACHE(double *prod, double *sum, double d1, double d2) {
	double d1Closest = floor(d1);
	double d2Closest = floor(d2);

	*prod = d1Closest * d2Closest;
	*sum = d1Closest + d2Closest;

	if (d1Closest > 0.0 && d2Closest > 0.0)
		return 1;
	else if (d1Closest == 0.0 && d2Closest == 0.0)
		return 1;
	else if (d1Closest < 0.0 && d2Closest < 0.0)
		return 1;
	else
		return -1;
}