/*###################################################################################################
#Engagement		-	UCLA MAS - STAT 404 - Project 													#
#FileName		-	rejectsmpl.c									  								#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	2/16/2017									  									#
#																	  								#
#Purpose:		-	1. Gaussian Kernel Density with Rejection Sampling.								#
#																									#
####################################################################################################*/

#include <R.h>
#include <RMath.h>
	
//dta		- Input Vector of data
//obs		- Observations to Return
//n			- Start of Uniform
//m 		- End of Uniform
//bw		- Bandwidth
//lenx		- Length of dta
//con 		- Height of uniform
//out		- Output

void rejectsmpl(double *dta, int *obs, double *n, double *m, double *bw, int *lenx, double *con, double *out) {
	
		int k=0,j	 	;  //k - Loop Variable; j - Internal Loop Variable				
		double u,t,w,b	;  //u - Uniform(0,1); t - Uniform(n,m); w - width of uniform; b - Height * Width of Uniform	
		double d		;  //d - Density Estimate
		double g		;  //g - Density of Uniform(n,m)
		double kest 	;  //kest - Kernel Estimate
		double res		;  //res - Output result

		w= *m - *n 		;  //Width of Uniform(n,m)	
		g= 1.0/w     	;  //Density of Uniform(n,m)
		b= *con * w 	;  //Height * Width of Uniform(n,m)
		 
		//Rejection Sampling Loop
		
		//Rprintf("w:%f,g:%f,b:%f,con:%f,m:%f,n:%f", w,g,b,con,*m,*n)
		
		GetRNGstate();
		while(k < *obs) {		

			u = ((runif(0,1)) * w) + *n ; 					//Returns one Random Variable from the Uniform Distribution over entire distribution
			t = runif(0,1) ;								//Returns one Random Variable from the Uniform Distribution from 0,1		
				
			res=0.0;
			kest = 0.0;
			
			for(j=0; j < *lenx; j++) {			
				d = u - dta[j];
				kest = kest + dnorm( (d / *bw) , 0, 1, 0) ;
			}			
			res = kest / ( (*lenx) * (*bw) );
			
			//Estimates Kernel Density for the one observation using the kernden function														
			if(t < (res / (b*g)) )   {							//If within acception zone then keep
				out[k] = u	; 
				k = k + 1 ; 
			}
		}
		PutRNGstate();		
}
