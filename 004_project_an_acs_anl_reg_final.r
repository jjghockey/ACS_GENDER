#####################################################################################################
#Engagement		-	UCLA MAS - STAT 404 - Project													#
#FileName		-	004_project_an_acs_anl_reg.r			  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	3/4/2017									  									#
#																	  								#
#Purpose:		-	Regression Modeling																#
#Notes:			- 																					#
#																									#
#####################################################################################################



#I. Setup

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	setwd("C:/Users/jguinta/Desktop/Working/005_GradSchool/003_Course/STAT404/project/001_DataAnalysis/progs/")
	
	#Package Install
	require(gdata)			#Excel processing 
	require(dplyr)			#Better Plyr
	require(ggplot2)		#Graphing Utilities
	require(stringr)		#String Functions
	require(reshape2)		#Data Reshape
	require(readr)			#Better loading tools
	require(data.table)		#Data table operations
	require(texreg)			#Nice Regression output
	require(GGally)			#Correlation
	require(lmtest)			#Linear Model Testing	
	require(car)			#Regression functions
	require(nortest)		#Regression normality tests
	require(MASS)			#Regression functions
	require(leaps)			#Regression subsetting functions
	require(tidyverse)		#Tidyr
	require(maps)			#Maps Package
	require(grid)			#Plotting utilities
	require(gridExtra)		#Plotting utilities
	require(gam)			#General Additive Models
	require(mgcv)			#General Additive Models
	
	#Set Options
	options(scipen=20)
	
	#Graphic Themes
		out_theme <- theme_bw() + 
		  theme(panel.grid.major=element_line(color="white"), 
				text=element_text(family="ArialMT"), 
				legend.position="bottom",
				plot.title = element_text(size = rel(1.0)),
				axis.text.x = element_text(size= rel(1.0)),
				axis.text.y = element_text(size= rel(1.0)))
				
		color_scheme <- c("#6495ED", "#001933", "#08519c", "#778899", "#B0C4DE", 
							  "#999999", "#000000", "#C90E17", "#800000", "#B23232", 
							  "#691b14")    

		color_scheme2 <- c("#6495ED", "#C90E17", "#001933", "#691b14", "#08519c", "#778899", "#B0C4DE", 
							  "#999999", "#000000",  "#800000", "#B23232")   	
	
	#Custom Defined Functions
			
		#Copied from http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2#4357932
		ggQQ <- function(LM) # argument: a linear model
		{
			y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
			x <- qnorm(c(0.25, 0.75))
			slope <- diff(y)/diff(x)
			int <- y[1L] - slope * x[1L]
			p <- ggplot(LM, aes(sample=.resid)) + stat_qq(alpha = 0.5) 
			p <- p + geom_abline(slope = slope, intercept = int, color="blue")
			return(p)
		}	
		
		ggQQ_gam <- function(GAM) # argument: a linear model
		{
			y <- quantile(residuals.gam(GAM, type="response")[!is.na(residuals.gam(GAM, type="response"))], c(0.25, 0.75))
			x <- qnorm(c(0.25, 0.75))
			slope <- diff(y)/diff(x)
			int <- y[1L] - slope * x[1L]
						
			.resid<-residuals.gam(gam1, type="response")	
			.resid<-as.data.frame(.resid)
			
			p <- ggplot(.resid, aes(sample=.resid) )
			p <- p + stat_qq(alpha = 0.5) 
			p <- p + geom_abline(slope = slope, intercept = int, color="blue")
			return(p)
		}	
		
		kd_sim<-function(dta, sim, obs, n, m, con) {
				#n:		#Number of times to simulate
				#obs:	#Number of points being simulated with each pass
				#con:   #Maximum of f(x) of original estimate
				#d: 	#Width of Uniform Coverage
				#b: 	#Constant
				#g: 	#Uniform Density
					
				for (j in 1:sim) {
					st<-Sys.time()
				
					smpl<-sample(dta,500)
					smpl.df<-as.data.frame(smpl)
					smpl.df$ord<-1:nrow(smpl.df)		
				
					bw<-bw.nrd(smpl.df$smpl)
					grd<-seq(from=min(smpl), to=max(smpl), length.out=100)
				
					reject_est<-rejectsmpl(smpl,obs,n,m,bw,con)	
						
					reject_est<-reject_est$out	
					
					bw_est<-bw.nrd(reject_est)
					
					kern_est<-kernden(reject_est, grd, bw_est)		
					kern_out<-cbind(as.data.frame(kern_est), as.data.frame(grd))
					names(kern_out)<-c("kern_est", "grd")					
		
					kd<-kern_out$kern_est
		
					if (j==1) {
						out<-kd
					} 
					else {
						out<-rbind(kd, out)
					}
						ed<-Sys.time()	
						tot<-round(as.numeric(difftime(ed,st, units="mins")),3)
						print(paste("Loop: ",j,"-",tot,"Minutes", sep=" "))
				}
			out<-as.data.frame(out)
			row.names(out)<-1:nrow(out)
			return(out)
		}	

		#Rejection Sampling		
			#Compile and Load C
			dyn.unload("rejectsmpl.dll")
			system("R CMD SHLIB rejectsmpl.c", intern=TRUE)
			dyn.load("rejectsmpl.dll")
			
			#Wrap Function
			rejectsmpl<-function(dta,obs,n,m,bw,con) {				
				out<-.C("rejectsmpl", dta=as.double(dta)
									, obs=as.integer(obs)
									, n=as.double(n)
									, m=as.double(m)
									, bw = as.double(bw)
									, lenx=as.integer(length(dta))
									, con=as.double(con)
									, out=double(obs))
			}
		
		#Kernel Density
			#Compile and Load C
			dyn.unload("kernden.dll")
			system("R CMD SHLIB kernden.c", intern=TRUE)
			dyn.load("kernden.dll")
		
			#Wrap Function
			kernden<-function(x, grd, bw) {
				lenx<-length(x)
				lengrd<-length(grd)
					
				out<-.C("kernden", 	x=as.double(x)
								 , 	lenx=as.integer(lenx)
								 , 	grd=as.double(grd)
								 , 	lengrd=as.integer(lengrd)
								 ,  bw = as.double(bw)
								 , 	y=double(lengrd)) 
				return(out$y)
			}		


	#Custom Functions
	source("CustomFunctions/merge2/merge2.r")
	
#II. Data Loading 
	#A. ACS
		load("../data/acs_anl.rda")		
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$occp_ind=="mgr"),] #Targeting salaried managers 
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$ftpt!="part time (less than 35 hours)"),] #Targeting full employeed 
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$occp_descr!="emergency management directors"),] #Removed due to sample size issues
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$wagp_infl!=0),] #Zero indicates no wage
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$below_min==0),] #Remove likely bad responses
		nrow(acs_anl)		 
		acs_anl$drop_flg<-0
		acs_anl$drop_flg[as.numeric(as.character(acs_anl$schg)) <=15]<-1 #Remove likely college / HS students
		acs_anl<-acs_anl[which(acs_anl$drop_flg==0),]
		nrow(acs_anl)
		
#III. Data Processing
	#A. Variable Transformation 
		#1. Wages needs to be a log	
		acs_anl$log_wagp_infl<-log(acs_anl$wagp_infl)
		acs_anl$log_agep<-log(acs_anl$agep)
		
	#B. County clean up
		acs_anl$cntyname<-as.character(acs_anl$cntyname)
		acs_anl$cntyname[acs_anl$cntyname=="Los Angeles CA/Orange CA"]<-"Orange CA"
		acs_anl$cntyname[acs_anl$cntyname=="Kern CA/Los Angeles CA/San Bernardino CA"]<-"Kern CA"
		acs_anl$cntyname[acs_anl$cntyname=="El Dorado CA/Sacramento CA"]<-"El Dorado CA"
		acs_anl$cntyname[acs_anl$cntyname=="El Dorado CA/Sacramento CA"]<-"El Dorado CA"
		acs_anl$cntyname[acs_anl$cntyname=="Los Angeles CA/San Bernardino CA"]<-"San Bernardino CA"						
		acs_anl$cntyname[acs_anl$cntyname=="Santa Barbara CA/Ventura CA"]<-"Santa Barbara CA"
		acs_anl$cntyname[acs_anl$cntyname=="Fresno CA/Merced CA"]<-"Merced CA"						
		acs_anl$cntyname[acs_anl$cntyname=="Nevada CA/Plumas CA/Sierra CA"]<-"Nevada CA/Sierra CA"
		acs_anl$cntyname[acs_anl$cntyname=="Fresno CA/Merced CA"]<-"Merced CA"	
		acs_anl$cntyname[acs_anl$cntyname=="Butte CA/Yuba CA"]<-"Butte CA"	
		acs_anl$cntyname<-gsub(" CA", "", acs_anl$cntyname)
		acs_anl$cntyname<-as.factor(acs_anl$cntyname)

#IV. Analysis
	#A. Simple Model - As a Baseline
		#1. Run Baseline Models
			#a. Wage on Sex
			reg_base<-lm(log_wagp_infl~sex+yr, weight=pwgtp, data=acs_anl)
			reg_base_sum<-summary(reg_base)
			
		#2. Simple Plots
			#Age
			plt<-ggplot(acs_anl, aes(log_agep))+geom_histogram(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Log Age", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist1<-plt			
			
			#Wages
			plt<-ggplot(acs_anl, aes(log_wagp_infl))+geom_histogram(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Log Wages", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist2<-plt						

			#Sex
			plt<-ggplot(acs_anl, aes(sex))+geom_bar(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Sex", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist3<-plt						
	
			#Industry
			tmp <- acs_anl %>% group_by(ind) %>% summarize(avg=n())
			tmp <- as.data.frame(tmp)
			tmp <- tmp[order(tmp$avg), c("ind")]
			tmp <- as.character(tmp)
			acs_anl$ind<-factor(acs_anl$ind, level=c(tmp))	
			
			plt<-ggplot(acs_anl, aes(ind))+geom_bar(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Industry", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist4<-plt+coord_flip()	
			
			#Occupation
			tmp <- acs_anl %>% group_by(occp_descr) %>% summarize(avg=n())
			tmp <- as.data.frame(tmp)
			tmp <- tmp[order(tmp$avg), c("occp_descr")]
			tmp <- as.character(tmp)
			acs_anl$occp_descr<-factor(acs_anl$occp_descr, level=c(tmp))	
			
			plt<-ggplot(acs_anl, aes(occp_descr))+geom_bar(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Occupation", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			plt<-plt+theme(axis.text.y = element_text(size= rel(0.7)))
			hist5<-plt+coord_flip()		

			#Education
			tmp <- acs_anl %>% group_by(educ) %>% summarize(avg=n())
			tmp <- as.data.frame(tmp)
			tmp <- tmp[order(tmp$avg), c("educ")]
			tmp <- as.character(tmp)
			acs_anl$educ<-factor(acs_anl$educ, level=c(tmp))	
			
			plt<-ggplot(acs_anl, aes(educ))+geom_bar(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Education", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist6<-plt+coord_flip()			

			#County
			tmp <- acs_anl %>% group_by(cntyname) %>% summarize(avg=n())
			tmp <- as.data.frame(tmp)
			tmp <- tmp[order(tmp$avg), c("cntyname")]
			tmp <- as.character(tmp)
			acs_anl$cntyname<-factor(acs_anl$cntyname, level=c(tmp))	
			
			plt<-ggplot(acs_anl, aes(cntyname))+geom_bar(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="County", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.3))
			plt<-plt+theme(axis.text.y = element_text(size= rel(0.4)))
			hist7<-plt+coord_flip()		

			#Year			
			plt<-ggplot(acs_anl, aes(yr))+geom_bar(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Year", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist8<-plt
			
			#Weights		
			plt<-ggplot(acs_anl, aes(pwgtp))+geom_histogram(fill="#6495ED")
			plt<-plt+out_theme
			plt<-plt+labs(x="Personal Weight Variable", y="Count")
			plt<-plt+theme(plot.title = element_text(hjust = 0.5))
			plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
			hist9<-plt			
			
			hist_final1<-grid.arrange(hist1,hist2,hist3,hist6,hist8,hist9, ncol=2)
						
		#3. Simple Summary Table
			#a. Create Summary
				sm<-as.data.frame(summary(acs_anl[c("wagp_infl", "log_wagp_infl", "sex", "yr", "educ", "agep", "log_agep", "occp_descr", "cntyname", "ind", "pwgtp")]))
				sm$Var1<-NULL

			#b. Reshape Object to so the columns across the top are the summary stats by variable
				sm<-sm %>% group_by(Var2) %>% mutate(ord=row_number())
				sm<-as.data.frame(sm)
				
				sm_w<-reshape(sm, idvar=c("Var2"), timevar=c("ord"), direction="wide", sep="")
			
			#c. Clean up
				lst<-names(sm_w)[grepl("Freq", names(sm_w))==TRUE]
				for (i in c(lst) ) {
					sm_w[,c(i)]<-tolower(trim(sm_w[,c(i)]))
					j<-1
					while ( j < 20 ) {
						sm_w[,c(i)]<-gsub("  ", " ", sm_w[,c(i)])
						j <- j + 1
					}
				}
				sm_w$Var2<-trim(tolower(sm_w$Var2))		
											
	#B. Complex Model
		#1. Run Complex Models - Focus on Management
			#a. All relevant variables on Sex			
				reg<-lm(log_wagp_infl~sex+yr+educ+log_agep+occp_descr+cntyname+ind, weight=pwgtp, data=acs_anl)
				reg_sum<-summary(reg)	
				
				vif_reg<-vif(reg)
				vif_reg<-as.data.frame(vif_reg)
				sum(vif_reg)/(sum(vif_reg$Df)-1) #Predictors minus 1
				
				gam1<-mgcv::gam(log_wagp_infl~sex+yr+educ+s(log_agep)+occp_descr+cntyname+ind, weight=pwgtp, data=acs_anl)
				gam1_sum<-summary(gam1)
	
			#b. Residual Plots / QQ Plots
			
				#i. OLS Regression
			
				#Residuals / Fitted Values
					res<-residuals(reg)
					fit<-fitted(reg)
					stures<-studres(reg)
									
					reg_res_fit<-cbind(acs_anl, res, fit, stures)
					
				#QQ Plot
					plt<-ggQQ(reg)+out_theme
					plt<-plt+labs(title=c("OLS Regression"), subtitle=c())
					plt<-plt+labs(x="Theoretical", y="Sample")
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					plt<-plt+ylim(-3,3)
					plt<-plt+xlim(-3,3)
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig11a<-plt

				#Residual Plots
					plt<-ggplot(reg_res_fit, aes(x=fit, y=stures)) + geom_density2d()
					plt<-plt+stat_density_2d(geom="raster", aes(fill=..density..), contour=FALSE, alpha=0.5)
					plt<-plt+scale_fill_gradient(low="white", high="black")
					plt<-plt+geom_point(shape=".", alpha="0.01")
					plt<-plt+labs(title=c("OLS Regression"), subtitle=c(""))
					plt<-plt+labs(x="Fitted Values", y="Studentized Residuals")
					plt<-plt+xlim(10,13)
					plt<-plt+ylim(-3,3)
					plt<-plt+out_theme
					plt<-plt+theme(legend.position="right")
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig12a<-plt
					
				#ii. GAM Regression
				#Residuals / Fitted Values
					res<-residuals.gam(gam1)
					fit<-predict.gam(gam1)
					stures<-residuals.gam(gam1, type="response")	
									
					gam1_res_fit<-cbind(acs_anl, res, fit, stures)
					
				#QQ Plot
					plt<-ggQQ_gam(gam1)+out_theme
					plt<-plt+labs(title=c("GAM Regression"))
					plt<-plt+labs(x="Theoretical", y="Sample")
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					plt<-plt+ylim(-3,3)
					plt<-plt+xlim(-3,3)
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig13a<-plt
					
				#Residual Plots
					plt<-ggplot(gam1_res_fit, aes(x=fit, y=stures)) + geom_density2d()
					plt<-plt+stat_density_2d(geom="raster", aes(fill=..density..), contour=FALSE, alpha=0.5)
					plt<-plt+scale_fill_gradient(low="white", high="black")
					plt<-plt+geom_point(shape=".", alpha="0.01")
					plt<-plt+labs(title=c("GAM Regression"))
					plt<-plt+labs(x="Fitted Values", y="Studentized Residuals")
					plt<-plt+xlim(10,13)
					plt<-plt+ylim(-3,3)
					plt<-plt+out_theme
					plt<-plt+theme(legend.position="right")
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig14a<-plt
					
					fig11<-grid.arrange(fig11a,fig13a, ncol=2, top=textGrob("Fig. 11: QQ Plot \n Log Wages = Sex + Year + Occupation + Education + County + Log Age + Industry", gp=gpar(fontsize=15)))
					fig12<-grid.arrange(fig12a,fig14a, ncol=2, top=textGrob("Fig. 12: Residual Density Log Wages = Sex + Year + Occupation + Education + County + Log Age + Industry", gp=gpar(fontsize=15)))
				
			#c. Other Plots
				#i. Interaction Terms
					#Occupation / Wages / Gender
					
					tmp <- acs_anl[which(acs_anl$sex=="Male"),] %>% group_by(occp_descr) %>% summarize(avg=mean(log_wagp_infl))
					tmp <- as.data.frame(tmp)
					tmp <- tmp[order(tmp$avg), c("occp_descr")]
					tmp <- as.character(tmp)
					
					acs_anl$occp_descr<-factor(acs_anl$occp_descr, level=c(tmp))
					
					plt<-ggplot(acs_anl, aes(x=occp_descr, y=log_wagp_infl, color=sex))
					plt<-plt+geom_point(position="jitter", alpha=0.25, shape=".")
					plt<-plt+stat_summary(aes(x=occp_descr, y=log_wagp_infl, group=sex, color=sex), fun.data="mean_se", geom="crossbar")
					plt<-plt+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
					plt<-plt+labs(title=c("Fig. 4: Log Wages By Gender and Occupation"), subtitle=c("2015 Dollars"))
					plt<-plt+labs(x="Occupation", y="Log Wages \n (2015 Dollars)")
					plt<-plt+theme(legend.text=element_text(size=7))
					plt<-plt+theme(text=element_text(family="ArialMT"))	
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig4<-plt+coord_flip()
	
					#Education / Wages / Gender
					tmp <- acs_anl[which(acs_anl$sex=="Male"),] %>% group_by(educ) %>% summarize(avg=mean(log_wagp_infl))
					tmp <- as.data.frame(tmp)
					tmp <- tmp[order(tmp$avg), c("educ")]
					tmp <- as.character(tmp)
					
					acs_anl$educ<-factor(acs_anl$educ, level=c(tmp))
					
					plt<-ggplot(acs_anl, aes(x=educ, y=log_wagp_infl, color=sex))
					plt<-plt+geom_point(position="jitter", alpha=0.25, shape=".")
					plt<-plt+stat_summary(aes(x=educ, y=log_wagp_infl, group=sex, color=sex), fun.data="mean_se", geom="crossbar")
					plt<-plt+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
					plt<-plt+labs(title=c("Fig 5: Log Wages By Gender and Education"), subtitle=c("2015 Dollars"))
					plt<-plt+labs(x="Education", y="Log Wages \n (2015 Dollars)")
					plt<-plt+theme(legend.text=element_text(size=7))
					plt<-plt+theme(text=element_text(family="ArialMT"))	
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig5<-plt+coord_flip()
					
					#Industry / Wages / Gender
					tmp <- acs_anl[which(acs_anl$sex=="Male"),] %>% group_by(ind) %>% summarize(avg=mean(log_wagp_infl))
					tmp <- as.data.frame(tmp)
					tmp <- tmp[order(tmp$avg), c("ind")]
					tmp <- as.character(tmp)
					
					acs_anl$ind<-factor(acs_anl$ind, level=c(tmp))
					
					plt<-ggplot(acs_anl, aes(x=ind, y=log_wagp_infl, color=sex))
					plt<-plt+geom_point(position="jitter", alpha=0.25, shape=".")
					plt<-plt+stat_summary(aes(x=ind, y=log_wagp_infl, group=sex, color=sex), fun.data="mean_se", geom="crossbar")
					plt<-plt+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
					plt<-plt+labs(title=c("Fig 6: Log Wages By Gender and NAICS Industry"), subtitle=c("2015 Dollars"))
					plt<-plt+labs(x="NAICS Industry", y="Log Wages \n (2015 Dollars)")
					plt<-plt+theme(legend.text=element_text(size=7))
					plt<-plt+theme(text=element_text(family="ArialMT"))	
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					fig6<-plt+coord_flip()
					
				#ii. Wages Density			
					ca <- map_data("county")
					ca <- ca[which(ca$region =="california"),]
					names(ca)<-c("lon", "lat", "group", "order", "region", "subregion")

					counties <- map_data("county")
					ca_county <- subset(counties, region == "california")
					names(ca_county)<-c("lon", "lat", "group", "order", "region", "subregion")			
			
					acs_anl2<-acs_anl[c("lat", "lon", "log_wagp_infl", "sex")]
			
					plt<-ggplot(acs_anl2, aes(x=lon, y=lat, color=sex)) + geom_point(position="jitter", alpha=0.25) + geom_density2d()
					plt<-plt+stat_density_2d(geom="raster", aes(fill=..density..), contour=FALSE, alpha=0.5)
					plt<-plt+out_theme
					plt<-plt+scale_color_manual(values=color_scheme2)
					plt<-plt+scale_fill_gradient(low="white", high="grey")
					plt<-plt+geom_path(data=ca,aes(x=lon, y=lat,group=group), colour="black")
					plt<-plt+labs(title=c("Fig. 7: Kernel Density Estimate of Log Wages \n (2010-2015)"), subtitle=c("2015 Dollars"))
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
					plt<-plt+labs(x="Longitude", y="Latitude")
					plt<-plt+theme(axis.text.x=element_blank(), axis.text.y=element_blank())
					plt<-plt+theme(axis.title.x=element_blank(), axis.title.y=element_blank())
					plt<-plt+theme(legend.position="none")
					plt<-plt+coord_fixed()
					fig7<-plt
				
		#2. Additional Report Graphics
			#a. Coefficient of Sex over time (complex model)
				for (i in 2010:2015) { 
					#Run Regression
					reg_yr<-lm(log_wagp_infl~sex+educ+agep+occp_descr+cntyname, weight=pwgtp, data=acs_anl[which(acs_anl$yr==i),])
					out<-as.data.frame(coef(reg_yr)["sexFemale"])
					names(out)<-"coef"
					
					#Convert Back into Percentage Change Units
					out$coef_pct<-round((exp(out$coef)-1),2)
					
					out$yr<-i
					if (i==2010) {
						sex_yr<-out
					}
					else {
						sex_yr<-rbind(sex_yr, out)
					}
				}
				plt<-ggplot(sex_yr, aes(x=as.factor(yr), y=coef_pct))+geom_bar(stat="identity", position="identity", fill="#6495ED")
				plt<-plt+out_theme+scale_fill_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				plt<-plt+labs(title=c("Fig. 10: Percentage Difference in Pay By Year \n Regression Model of Log Wages = Sex + Occupation + Education + County + Log Age + Industry \n (2010 - 2015)"), subtitle=c("Below Zero Indicates that Female was Paid Less than Male"))
				plt<-plt+labs(x="Year", y="Percentage Difference to Male Wages \n (2015 Dollars)")
				plt<-plt+theme(legend.text=element_text(size=7))
				plt<-plt+theme(text=element_text(family="ArialMT"))	
				plt<-plt+geom_hline(yintercept=0, color="#C90E17")
				plt<-plt+scale_y_continuous(limits=c(-0.40,0), breaks=c(0.0, -0.1, -0.2, -0.3, -0.4))
				plt<-plt+theme(plot.title = element_text(hjust = 0.5))
				plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
				fig10<-plt		
				
			#b. Coefficient of Sex over time (complex model)
				for (i in 2010:2015) { 
					#Run Regression
					reg_yr<-lm(log_wagp_infl~sex, weight=pwgtp, data=acs_anl[which(acs_anl$yr==i),])
					out<-as.data.frame(coef(reg_yr)["sexFemale"])
					names(out)<-"coef"
					
					#Convert Back into Percentage Change Units
					out$coef_pct<-round((exp(out$coef)-1),2)
					
					out$yr<-i
					if (i==2010) {
						sex_yr<-out
					}
					else {
						sex_yr<-rbind(sex_yr, out)
					}
				}
				plt<-ggplot(sex_yr, aes(x=as.factor(yr), y=coef_pct))+geom_bar(stat="identity", position="identity", fill="#6495ED")
				plt<-plt+out_theme+scale_fill_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				plt<-plt+labs(title=c("Fig. 3: Percentage Difference in Pay By Year \n Regression Model of Log Wages = Sex \n (2010 - 2015)"), subtitle=c("Below Zero Indicates that Female was Paid Less than Male"))
				plt<-plt+labs(x="Year", y="Percentage Difference to Male Wages \n (2015 Dollars)")
				plt<-plt+theme(legend.text=element_text(size=7))
				plt<-plt+theme(text=element_text(family="ArialMT"))	
				plt<-plt+geom_hline(yintercept=0, color="#C90E17")
				plt<-plt+scale_y_continuous(limits=c(-0.40,0), breaks=c(0.0, -0.1, -0.2, -0.3, -0.4))
				plt<-plt+theme(plot.title = element_text(hjust = 0.5))
				plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
				fig3<-plt		
			
			#b. Absolute Difference in Pay 
				tbl<-acs_anl %>% 
					 group_by(sex, yr) %>% 
					 summarize(
						avg_sal=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
					)
				tbl<-as.data.frame(tbl)
				plt<-ggplot(tbl, aes(x=as.numeric(as.character(yr)), y=avg_sal, color=sex))+geom_line(stat="identity", position="identity")
				plt<-plt+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				plt<-plt+labs(title=c("Fig. 2: Average Wages By Gender"), subtitle=c("2010 - 2015"))
				plt<-plt+labs(x="Gender", y="WAGP (2015 Dollars)")
				plt<-plt+theme(legend.text=element_text(size=7))
				plt<-plt+theme(text=element_text(family="ArialMT"))	
				plt<-plt+theme(plot.title = element_text(hjust = 0.5))
				plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
				fig2<-plt
				
			#c. Report Numbers
				#i. Total Obs / Pop
				nrow(acs_anl)
				sum(acs_anl$pwgtp)
				
				#ii. Overall Pay Difference 
				reg$coef["sexFemale"]
				round((exp(reg$coef)["sexFemale"]-1),3)
				
				reg_base$coef["sexFemale"]
				round((exp(reg_base$coef)["sexFemale"]-1),3)
				
			#d. Histogram
			
				#i. Build Rejection Sampling with Confidence Intervals	(Log Wages)	

					#Log Wages
					log_wage<-acs_anl$log_wagp_infl
					log_wage.df<-as.data.frame(log_wage)
					names(log_wage.df)<-"log_wage"
					log_wage.df$ord<-1:nrow(log_wage.df)

					#Simulation
					sim<-kd_sim(log_wage, sim=100, obs=length(log_wage), n=9,m=14,con=0.70)	

					grd<-seq(from=min(log_wage), to=max(log_wage), length.out=100)
					for (i in 1:length(grd) ) {
						print(i)
						y<-sim[,c(i)] #Take each column and convert into a vector
						y<-sort(y)
						lower<-y[5]
						upper<-y[95]
						
						if (i==1) {
							out<-cbind(lower,upper)
						}
						else {
							tmp<-cbind(lower,upper)
							out<-rbind(out,tmp)
						}
					}					

					#Build Full Data for Ploting
					log_perc<-out
					log_perc<-as.data.frame(log_perc)
					log_perc$ord<-grd

					kest<-kernden(log_wage, grd, bw.nrd(log_wage.df$log_wage))		
					kest<-as.data.frame(kest)
					names(kest)<-c("est")
					kest$ord<-grd		
			
					plt_log_est<-merge(kest, log_perc, by=c("ord"))	
					names(plt_log_est)<-c("ord", "est", "5%", "95%")
		
					plt_log_est_m<-melt(plt_log_est, id.var=c("ord"))
					plt<-ggplot(plt_log_est_m, aes(x=ord, y=value, color=variable))+geom_line()+out_theme
					plt<-plt+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
					plt<-plt+labs(title=c("Log Wages"))
					plt<-plt+labs(x="Log Wages", y="Kernel Density of Wages \n (2015 Dollars)")	
					plt<-plt+theme(axis.text.y=element_blank())
					plt<-plt+theme(legend.text=element_text(size=7))
					plt<-plt+theme(text=element_text(family="ArialMT"))	
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))				
					plt<-plt+theme(text=element_text(family="Arial"))
					fig1a<-plt
					
				#ii. Build Rejection Sampling with Confidence Intervals	(Actual Wages)

					#Wages
					wage<-acs_anl$wagp_infl
					wage.df<-as.data.frame(wage)
					names(wage.df)<-"wage"
					wage.df$ord<-1:nrow(wage.df)

					#Simulation
					sim2<-kd_sim(wage, sim=100, obs=length(wage), n=10000,m=500000,con=0.000009) 
					
					grd<-seq(from=min(wage), to=max(wage), length.out=100)
					for (i in 1:length(grd) ) {
						print(i)
						y<-sim2[,c(i)] #Take each column and convert into a vector
						y<-sort(y)
						lower<-y[5]
						upper<-y[95]
						
						if (i==1) {
							out<-cbind(lower,upper)
						}
						else {
							tmp<-cbind(lower,upper)
							out<-rbind(out,tmp)
						}
					}					

					#Build Full Data for Ploting
					perc<-out
					perc<-as.data.frame(perc)
					perc$ord<-grd

					kest<-kernden(wage, grd, bw.nrd(wage.df$wage))		
					kest<-as.data.frame(kest)
					names(kest)<-c("est")
					kest$ord<-grd		
			
					plt_est<-merge(kest, perc, by=c("ord"))	
					names(plt_est)<-c("ord", "est", "5%", "95%")
		
					plt_est_m<-melt(plt_est, id.var=c("ord"))
					plt<-ggplot(plt_est_m, aes(x=ord, y=value, color=variable))+geom_line()+out_theme
					plt<-plt+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
					plt<-plt+labs(title=c("Wages"))
					plt<-plt+labs(x="Wages", y="Kernel Density of Wages \n (2015 Dollars)")	
					plt<-plt+theme(axis.text.y=element_blank())
					plt<-plt+theme(legend.text=element_text(size=7))
					plt<-plt+theme(text=element_text(family="ArialMT"))	
					plt<-plt+theme(plot.title = element_text(hjust = 0.5))
					plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))				
					plt<-plt+theme(text=element_text(family="Arial"))
					fig1b<-plt
					
				fig1<-grid.arrange(fig1a,fig1b, ncol=2, top=textGrob("Fig. 1: Kernel Density with +- 5% Confidence Bands", gp=gpar(fontsize=15)))

			#e Density Estimate of Log Wages, by Age
				plt<-ggplot(acs_anl[c("log_wagp_infl", "log_agep", "sex")], aes(x=log_agep, y=log_wagp_infl, color=sex)) + geom_point(position="jitter", alpha=0.25, shape=".") + geom_density2d()
				plt<-plt+stat_density_2d(geom="raster", aes(fill=..density..), contour=FALSE, alpha=0.01, show.legend = FALSE)
				plt<-plt+out_theme
				plt<-plt+scale_color_manual(values=color_scheme2)
				plt<-plt+scale_fill_gradient(low="white", high="grey")
				plt<-plt+labs(title=c("Fig. 8: Kernel Density Estimate of Log Wages and Log Age by Gender \n (2010-2015)"), subtitle=c("2015 Dollars"))
				plt<-plt+labs(x="Log Age", y="Log Wages \n (2015 Dollars)")
				plt<-plt+theme(plot.title = element_text(hjust = 0.5))
				plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))
				plt<-plt+theme(legend.position="bottom")	
				fig8<-plt
			
				plt<-ggplot(acs_anl, aes(x=log_agep, y=log_wagp_infl, color=sex))+geom_point(alpha=0.01, position ="jitter")+out_theme
				plt<-plt+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())	
				plt<-plt+labs(title=c("Fig. 9: Kernel Density Regression of Log Wages and Log Age by Gender \n (2010-2015)"), subtitle=c("2015 Dollars"))
				plt<-plt+labs(x="Log Age", y="Log Wages \n (2015 Dollars)")
				plt<-plt+theme(legend.text=element_text(size=7))
				plt<-plt+theme(text=element_text(family="ArialMT"))	
				plt<-plt+theme(plot.title = element_text(hjust = 0.5))
				plt<-plt+theme(plot.subtitle = element_text(hjust = 0.5))			
				plt<-plt+stat_smooth(data=acs_anl, aes(color=sex)) 				
				fig9<-plt
							
	#C. Summary Table
		tbl1<-acs_anl %>% group_by(sex, yr) %>% summarize(
								avg_wage_pop=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
							,	total_n=n()
							,	total_people=sum(pwgtp)					
							)
		tbl1<-as.data.frame(tbl1)
		tbl1<-reshape(tbl1, idvar=c("yr"), timevar=c("sex"), direction="wide", sep="_")
		
		tbl1$total_people<-tbl1$total_people_Male+tbl1$total_people_Female
		tbl1$total_n<-tbl1$total_n_Male+tbl1$total_n_Female
		
		tbl1$pct1_M<-tbl1$total_people_Male/tbl1$total_people
		tbl1$pct1_F<-tbl1$total_people_Female/tbl1$total_people					
		
		tbl1$pct2_M<-tbl1$total_n_Male/tbl1$total_n
		tbl1$pct2_F<-tbl1$total_n_Female/tbl1$total_n					
				
		tbl1$pct_wage<-tbl1$avg_wage_pop_Female/tbl1$avg_wage_pop_Male
		tbl1 <- tbl1[c("yr", "avg_wage_pop_Male", "total_people_Male", "avg_wage_pop_Female", "total_people_Female", "total_people", "pct_wage")]
		
	#D. Trimmed Mean Regression 
		#Take 10% trim mean of wages (before logging)
		p5<-quantile(acs_anl$wagp_infl, probs=seq(0,1,0.025))["5%"]
		p95<-quantile(acs_anl$wagp_infl, probs=seq(0,1,0.025))["95%"]

		nrow(acs_anl) #58227
		acs_anl_trm<-acs_anl[which(acs_anl$wagp_infl>p5 & acs_anl$wagp_infl<p95),]
		nrow(acs_anl_trm) #52741
	
		reg_trm<-lm(log_wagp_infl~sex+yr+educ+log_agep+occp_descr+cntyname+ind, weight=pwgtp, data=acs_anl_trm)
		reg_base_trm<-lm(log_wagp_infl~sex+yr, weight=pwgtp, data=acs_anl_trm)
		
	#E. Kernel Smoothing Regression 
		ks_age_wage_m<-ksmooth(acs_anl[which(acs_anl$sex=="Male"),c("log_agep")], acs_anl[which(acs_anl$sex=="Male"), c("log_wagp_infl")]	, kernel="normal")
		ks_age_wage_m<-as.data.frame(ks_age_wage_m)
		ks_age_wage_m$sex<-"Male"
		ks_age_wage_f<-ksmooth(acs_anl[which(acs_anl$sex=="Female"),c("log_agep")], acs_anl[which(acs_anl$sex=="Female"), c("log_wagp_infl")]	, kernel="normal")
		ks_age_wage_f<-as.data.frame(ks_age_wage_f)
		ks_age_wage_f$sex<-"Female"
		
		ks_age_wage<-rbind(ks_age_wage_m, ks_age_wage_f)
		ks_age_wage$sex<-factor(ks_age_wage$sex, levels=c("Male", "Female"))
		
		reg_ks<-lm(y~x+sex, data=ks_age_wage)
		reg_ks_sum<-summary(reg_ks)
		
	#F. Regression with Age Restriction
		nrow(acs_anl) #
		acs_anl_trm<-acs_anl[which(acs_anl$agep<=28),]
		nrow(acs_anl_trm) #
	
		reg_age_trm<-lm(log_wagp_infl~sex+yr+educ+log_agep+occp_descr+cntyname+ind, weight=pwgtp, data=acs_anl_trm)
		reg_age_base_trm<-lm(log_wagp_infl~sex+yr, weight=pwgtp, data=acs_anl_trm)		
		
#V. Output
	#Regression 
	htmlreg(list(reg), file = "../output/005_project_regression.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "Regression Model: Log(Wages) = Intercept + Gender + Year + Education + Age + Occupation + County + Industry"
					,omit.coef = "(occp_descr)|(cnty)|(ind)"
					,custom.note = "[1] Dummy variables for county, occupation and industry were suppressed"
					)
					
	htmlreg(list(reg_trm), file = "../output/005_project_regression_trm.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "Regression Model: Log(Wages) = Intercept + Gender + Year + Education + Age + Occupation + County + Industry"
					,omit.coef = "(occp_descr)|(cnty)|(ind)"
					,custom.note = "[1] Dummy variables for county, occupation and industry were suppressed"
					)
					
	htmlreg(list(reg_age_trm), file = "../output/005_project_regression_age_trm.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "Regression Model: Log(Wages) = Intercept + Gender + Year + Education + LogAge + Occupation + County + Industry"
					,omit.coef = "(occp_descr)|(cnty)|(ind)"
					,custom.note = "[1] Dummy variables for county, occupation and industry were suppressed"
					)					
					
	htmlreg(list(reg_base), file = "../output/005_project_regression_base.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "Regression Model: Log(Wages) = Intercept + Gender + Year"
					,omit.coef = "yr"
					,custom.note = "[1] Dummy variables for year were suppressed"
					)
					
	htmlreg(list(reg_base_trm), file = "../output/005_project_regression_base_trm.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "Regression Model: Log(Wages) = Intercept + Gender + Year"
					,omit.coef = "yr"
					,custom.note = "[1] Dummy variables for year were suppressed"
					)
					
	htmlreg(list(reg_age_base_trm), file = "../output/005_project_regression_base_age_trm.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "Regression Model: Log(Wages) = Intercept + Gender + Year"
					,omit.coef = "yr"
					,custom.note = "[1] Dummy variables for year were suppressed"
					)	

	htmlreg(list(gam1), file = "../output/005_project_regression_gam1.doc"
					,inline.css = FALSE, doctype = TRUE, html.tag = TRUE,head.tag = TRUE, body.tag = TRUE
					,single.row=TRUE, digits=3, bold=0.05, stars = 0, caption.above = TRUE, 
					,caption = "GAM Regression Model: Log(Wages) = Intercept + Gender + Year + Education + Age + Occupation + County + Industry"
					,omit.coef = "yr"
					,custom.note = "[1] Dummy variables for county, occupation and industry were suppressed"
					)						
		
	ggsave(file="../output/005_fig1.png", height=8, width=11,fig1)	
	ggsave(file="../output/005_fig2.png", height=8, width=11,fig2)	
	ggsave(file="../output/005_fig3.png", height=8, width=11,fig3)	
	ggsave(file="../output/005_fig4.png", height=8, width=11,fig4)	
	ggsave(file="../output/005_fig5.png", height=8, width=11,fig5) 	
	ggsave(file="../output/005_fig6.png", height=8, width=11,fig6)	
	ggsave(file="../output/005_fig7.png", height=8, width=11,fig7)	
	ggsave(file="../output/005_fig8.png", height=8, width=11,fig8)
	ggsave(file="../output/005_fig9.png", height=8, width=11,fig9)	
	ggsave(file="../output/005_fig10.png", height=8, width=11,fig10)	
	ggsave(file="../output/005_fig11.png", height=8, width=11,fig11)	
	ggsave(file="../output/005_fig12.png", height=8, width=11,fig12)				
	ggsave(file="../output/005_hist1.png", height=8, width=11, hist_final1)
	ggsave(file="../output/005_hist2.png", height=8, width=11, hist4)
	ggsave(file="../output/005_hist3.png", height=8, width=11, hist5)
	ggsave(file="../output/005_hist4.png", height=8, width=11, hist7)
	
	write.csv(file="../output/005_tbl1.csv", tbl1)
	write.csv(file="../output/005_tbl2.csv", sm_w)