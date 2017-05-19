/*###################################################################################################
#Engagement		-	UCLA MAS - STAT 404 - Project 													#
#FileName		-	kernden.c										  								#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	2/16/2017									  									#
#																	  								#
#Purpose:		-	1. Gaussian Kernel Density with Rejection Sampling.								#
#																									#
####################################################################################################*/

#include <R.h>
#include <RMath.h>

//x 		- data
//lenx 		- length of x
//grd 		- grid to estimate over
//lengrd 	- length of xpts
//bw 		- bandwidth
//res 		- output

void kernden(double *x, int *lenx, double *grd, int *lengrd, double *bw, double *res) {
    /*
	Kernel Density Estimator using Scott's bandwidth
	*/
		int i;
		int j;
		double d; 
		double kest;

		for(i = 0; i < *lengrd; i++){
			kest = 0.0;
			for(j=0; j < *lenx; j++){
				d = grd[i] - x[j];
				kest = kest + dnorm( (d / *bw) , 0, 1, 0) ;
			}
			
			res[i] = kest / ( (*lenx) * (*bw) );
		}
	}

