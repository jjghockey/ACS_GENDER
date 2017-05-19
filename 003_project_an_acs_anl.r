#####################################################################################################
#Engagement		-	UCLA MAS - STAT 404 - Project													#
#FileName		-	002_project_mk_acs_anl.r				  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	2/9/2017									  									#
#																	  								#
#Purpose:		-	Data Summaries						 											#
#Notes:			- 	The data gets very large in memory.  This script was run a machine with 32 GB 	#
#					of memory.  Run using Microsoft R Open 3.3.2									#
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
	require(GGally)			#Correlation Matrix
	
	options(scipen=20)
	
	#Custom Functions

	#Graphic Themes
	color_scheme <- c("#6495ED", "#001933", "#08519c", "#778899", "#B0C4DE", 
						  "#999999", "#000000", "#C90E17", "#800000", "#B23232", 
						  "#691b14")    

	color_scheme2 <- c("#6495ED", "#C90E17", "#001933", "#691b14", "#08519c", "#778899", "#B0C4DE", 
						  "#999999", "#000000",  "#800000", "#B23232")   	
	
	out_theme <- theme_bw() + 
	  theme(panel.grid.major=element_line(color="white"), 
			legend.position="bottom",
			text=element_text(family="ArialMT"), 
			plot.title = element_text(size = rel(1.0)),
			axis.text.x = element_text(size= rel(0.5)),
			axis.text.y = element_text(size= rel(0.5)))
	
#II. Data Loading 
	#A. ACS
		load("../data/acs_anl.rda")
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$occp_ind=="mgr"),] #Targeting salaried managers 
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$ftpt!="part time (less than 35 hours)"),] #Targeting full employeed 
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$occp_descr!="mgr-emergency management directors"),] #Removed due to sample size issues
		nrow(acs_anl)
		acs_anl<-acs_anl[which(acs_anl$wagp_infl!=0),] #Zero indicates no wage
		nrow(acs_anl)		
		acs_anl<-acs_anl[which(acs_anl$below_min==0),]
		nrow(acs_anl)
		
#III. Data Processing 
	#A. Variable Tranformation
		acs_anl$log_wagp_infl<-log(acs_anl$wagp_infl)
		acs_anl$occp_descr<-gsub("mgr-", "", acs_anl$occp_descr)	
		acs_anl$occp_descr<-as.factor(acs_anl$occp_descr)
					
#IV. Data Exploration
	
	#A. Basic Summary
		summary(acs_anl)
		
	#B. Summary Graphs and Tables
		#1. Plots	
			#a. Histogram of Wages
				hist1<-ggplot(acs_anl, aes(wagp_infl, color=sex))+stat_density(geom="line", bw="nrd", kernel="gaussian")
				hist1<-hist1+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				hist1<-hist1+labs(title=c("Kernel Density of WAGP Adjusted for Inflation \n (2015 Dollars)"))
				hist1<-hist1+labs(x="Log(WAGP)", y="Frequency")
				hist1<-hist1+theme(legend.text=element_text(size=7))
				hist1<-hist1+theme(text=element_text(family="ArialMT"))
				hist1<-hist1+stat_function(fun=dnorm,color="grey",args=list(mean=mean(acs_anl$wagp_infl), sd=sd(acs_anl$wagp_infl)))				
				hist1a<-hist1
				hist1b<-hist1+facet_wrap(~yr, scales="free")
				hist1d<-hist1+facet_wrap(~educ, scales="free")
				hist1e<-hist1+facet_wrap(~ftpt, scales="free")
				hist1f<-hist1+facet_wrap(~occp_descr, scales="free")

				hist2<-ggplot(acs_anl, aes(log_wagp_infl, color=sex))+stat_density(geom="line", bw="nrd", kernel="gaussian")
				hist2<-hist2+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				hist2<-hist2+labs(title=c("Kernal Density of Log WAGP Adjusted for Inflation \n (2015 Dollars)"))
				hist2<-hist2+labs(x="Log(WAGP)", y="Frequency")
				hist2<-hist2+theme(legend.text=element_text(size=7))
				hist2<-hist2+theme(text=element_text(family="ArialMT"))
				hist2<-hist2+stat_function(fun=dnorm,color="grey",args=list(mean=mean(acs_anl$log_wagp_infl), sd=sd(acs_anl$log_wagp_infl)))			
				hist2a<-hist2
				hist2b<-hist2+facet_wrap(~yr, scales="free")
				hist2d<-hist2+facet_wrap(~educ, scales="free")
				hist2e<-hist2+facet_wrap(~ftpt, scales="free")
				hist2f<-hist2+facet_wrap(~occp_descr, scales="free")
				
			#b. Basic Barplot
				tbl<-acs_anl[which(acs_anl$yr==2015),] %>% 
					 group_by(sex) %>% 
					 summarize(
						avg_sal=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
					)
				tbl<-as.data.frame(tbl)
				graph1<-ggplot(tbl, aes(x=sex, y=avg_sal, fill=sex))+geom_bar(stat="identity", position="dodge")
				graph1<-graph1+out_theme+scale_fill_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				graph1<-graph1+labs(title=c("Barchart of Average Wages By Gender in 2015 \n (2015 Dollars)"))
				graph1<-graph1+labs(x="Gender", y="WAGP")
				graph1<-graph1+theme(legend.text=element_text(size=7))
				graph1<-graph1+theme(text=element_text(family="ArialMT"))	
				graph1<-graph1+coord_flip()	
				
			#c. Boxplot
				#i. Sex
				graph2<-ggplot(acs_anl, aes(sex, log_wagp_infl))+geom_boxplot()+out_theme
				graph2<-graph2+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="none", legend.title=element_blank())
				graph2<-graph2+labs(title=c("Boxplot of Log WAGP Adjusted for Inflation by Gender \n (2015 Dollars)"))
				graph2<-graph2+labs(x="Gender", y="Log(WAGP)")
				graph2<-graph2+theme(legend.text=element_text(size=7))
				graph2<-graph2+theme(text=element_text(family="ArialMT"))		
		
				#ii. Occp Description
				graph3<-ggplot(acs_anl, aes(as.factor(occp_descr), log_wagp_infl))+geom_boxplot()+out_theme
				graph3<-graph3+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="none", legend.title=element_blank())
				graph3<-graph3+labs(title=c("Boxplot of Log WAGP Adjusted for Inflation by Year \n (2015 Dollars)"))
				graph3<-graph3+labs(x="Occupation Description", y="Log(WAGP)")
				graph3<-graph3+theme(legend.text=element_text(size=7))
				graph3<-graph3+theme(text=element_text(family="ArialMT"))	
				graph3<-graph3+coord_flip()				

				#iii. Year		
				graph4<-ggplot(acs_anl, aes(yr, log_wagp_infl))+geom_boxplot()+out_theme
				graph4<-graph4+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="none", legend.title=element_blank())
				graph4<-graph4+labs(title=c("Boxplot of Log WAGP Adjusted for Inflation by Year \n (2015 Dollars)"))
				graph4<-graph4+labs(x="Year", y="Log(WAGP)")
				graph4<-graph4+theme(legend.text=element_text(size=7))
				graph4<-graph4+theme(text=element_text(family="ArialMT"))		
				
				#iv. Educ
				graph5<-ggplot(acs_anl, aes(educ, log_wagp_infl))+geom_boxplot()+out_theme
				graph5<-graph5+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="none", legend.title=element_blank())
				graph5<-graph5+labs(title=c("Boxplot of Log WAGP Adjusted for Inflation by Education \n (2015 Dollars)"))
				graph5<-graph5+labs(x="Year", y="Log(WAGP)")
				graph5<-graph5+theme(legend.text=element_text(size=7))
				graph5<-graph5+theme(text=element_text(family="ArialMT"))					
			
				#v. FTPT
				graph6<-ggplot(acs_anl, aes(ftpt, log_wagp_infl))+geom_boxplot()+out_theme
				graph6<-graph6+out_theme+scale_color_manual(values=color_scheme2)+theme(legend.position="none", legend.title=element_blank())
				graph6<-graph6+labs(title=c("Boxplot of Log WAGP Adjusted for Inflation by Full Time/Part Time \n (2015 Dollars)"))
				graph6<-graph6+labs(x="Year", y="Log(WAGP)")
				graph6<-graph6+theme(legend.text=element_text(size=7))
				graph6<-graph6+theme(text=element_text(family="ArialMT"))	

			#d. Bar Charts
				#i. Average Wages By Occp / Sex 2015
				tbl<-acs_anl[which(acs_anl$yr==2015),] %>% 
					 group_by(occp_descr, sex) %>% 
					 summarize(
						avg_sal=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
					)
				tbl<-as.data.frame(tbl)
				graph7<-ggplot(tbl, aes(x=occp_descr, y=avg_sal, fill=sex))+geom_bar(stat="identity", position="dodge")
				graph7<-graph7+out_theme+scale_fill_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				graph7<-graph7+labs(title=c("Barchart of Average Wages By Occupation in 2015 \n (2015 Dollars)"))
				graph7<-graph7+labs(x="Occupation", y="WAGP")
				graph7<-graph7+theme(legend.text=element_text(size=7))
				graph7<-graph7+theme(text=element_text(family="ArialMT"))	
				graph7<-graph7+coord_flip()
				
				#ii. Average Wages By Educ / Sex 2015
				tbl<-acs_anl[which(acs_anl$yr==2015),] %>% 
					 group_by(educ, sex) %>% 
					 summarize(
						avg_sal=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
					)
				tbl<-as.data.frame(tbl)
				graph8<-ggplot(tbl, aes(x=educ, y=avg_sal, fill=sex))+geom_bar(stat="identity", position="dodge")
				graph8<-graph8+out_theme+scale_fill_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				graph8<-graph8+labs(title=c("Barchart of Average Wages By Education Level in 2015 \n (2015 Dollars)"))
				graph8<-graph8+labs(x="Education", y="WAGP")
				graph8<-graph8+theme(legend.text=element_text(size=7))
				graph8<-graph8+theme(text=element_text(family="ArialMT"))	
				graph8<-graph8+coord_flip()
				
				#iii. Average Wages By FTPT / Sex 2015
				tbl<-acs_anl[which(acs_anl$yr==2015),] %>% 
					 group_by(ftpt, sex) %>% 
					 summarize(
						avg_sal=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
					)
				tbl<-as.data.frame(tbl)
				graph9<-ggplot(tbl, aes(x=ftpt, y=avg_sal, fill=sex))+geom_bar(stat="identity", position="dodge")
				graph9<-graph9+out_theme+scale_fill_manual(values=color_scheme2)+theme(legend.position="bottom", legend.title=element_blank())
				graph9<-graph9+labs(title=c("Barchart of Average Wages By Full Time / Part Time in 2015 \n (2015 Dollars)"))
				graph9<-graph9+labs(x="Full Time / Part Time", y="WAGP")
				graph9<-graph9+theme(legend.text=element_text(size=7))
				graph9<-graph9+theme(text=element_text(family="ArialMT"))	
				graph9<-graph9+coord_flip()
				
		#2. Tables 
			#a. Cross Tables of Interesting Variables
				#i. Average Wage and Population by Sex
					tbl1<-acs_anl %>% group_by(sex, yr) %>% summarize(
											avg_wage_pop=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
										,	total_people=sum(pwgtp)					
										)
					tbl1<-as.data.frame(tbl1)
					tbl1<-reshape(tbl1, idvar=c("yr"), timevar=c("sex"), direction="wide", sep="_")
					
					tbl1$total_people<-tbl1$total_people_Male+tbl1$total_people_Female
					tbl1$pct1_M<-tbl1$total_people_Male/tbl1$total_people
					tbl1$pct1_F<-tbl1$total_people_Female/tbl1$total_people					
					tbl1$pct_wage<-tbl1$avg_wage_pop_Female/tbl1$avg_wage_pop_Male
		
				#ii. Average Wage by Occupation Description
					#Build components of the table
						tbl2a<-acs_anl %>% group_by(occp_descr) %>% summarize(
												avg_wage=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
											,	total_people=sum(pwgtp)
											)
						tbl2a<-as.data.frame(tbl2a)
					
						tbl2b<-acs_anl %>% group_by(occp_descr, sex) %>% summarize(
												avg_wage=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
											,	total_people=sum(pwgtp)
											)
						tbl2b<-as.data.frame(tbl2b)
						tbl2b_w<-reshape(tbl2b, idvar=c("occp_descr"), timevar=c("sex"), direction="wide", sep="_")
									
					#Combine
						#Merge Together
						tbl2<-merge(tbl2a, tbl2b_w, by="occp_descr", all=TRUE)
						
						#Create Percentages
						tbl2$pct_male<-tbl2$total_people_Male/tbl2$total_people
						tbl2$pct_female<-tbl2$total_people_Female/tbl2$total_people
						
						tbl2$pct_total_people<-tbl2$total_people/sum(tbl2$total_people,na.rm=TRUE)
						tbl2$pct_total_people_Male<-tbl2$total_people_Male/sum(tbl2$total_people_Male,na.rm=TRUE)
						tbl2$pct_total_people_Female<-tbl2$total_people_Female/sum(tbl2$total_people_Female,na.rm=TRUE)
						tbl2$pct_wage<-tbl2$avg_wage_Female/tbl2$avg_wage_Male
			
						#Finalize Table
						tbl2<-tbl2[c("occp_descr", "avg_wage", "total_people", "pct_total_people"
								, 	"avg_wage_Male", "total_people_Male", "pct_male", "pct_total_people_Male"
								, 	 "avg_wage_Female", "total_people_Female", "pct_female", "pct_total_people_Female", "pct_wage")]
					
				#iii. Full Time / Part Time / Average Hours Worked 
					#Build components of the table
						tbl3a<-acs_anl %>% group_by(ftpt) %>% summarize(
												avg_wage=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
											,	total_people=sum(pwgtp)
											)
						tbl3a<-as.data.frame(tbl3a)
					
						tbl3b<-acs_anl %>% group_by(ftpt, sex) %>% summarize(
												avg_wage=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
											,	total_people=sum(pwgtp)
											)
						tbl3b<-as.data.frame(tbl3b)
						tbl3b_w<-reshape(tbl3b, idvar=c("ftpt"), timevar=c("sex"), direction="wide", sep="_")
									
					#Combine
						#Merge Together
						tbl3<-merge(tbl3a, tbl3b_w, by="ftpt", all=TRUE)
						
						#Create Percentages
						tbl3$pct_male<-tbl3$total_people_Male/tbl3$total_people
						tbl3$pct_female<-tbl3$total_people_Female/tbl3$total_people
						
						tbl3$pct_total_people<-tbl3$total_people/sum(tbl3$total_people,na.rm=TRUE)
						tbl3$pct_total_people_Male<-tbl3$total_people_Male/sum(tbl3$total_people_Male,na.rm=TRUE)
						tbl3$pct_total_people_Female<-tbl3$total_people_Female/sum(tbl3$total_people_Female,na.rm=TRUE)
						tbl3$pct_wage<-tbl3$avg_wage_Female/tbl3$avg_wage_Male
			
						#Finalize Table
						tbl3<-tbl3[c("ftpt", "avg_wage", "total_people", "pct_total_people"
								, 	"avg_wage_Male", "total_people_Male", "pct_male", "pct_total_people_Male"
								, 	 "avg_wage_Female", "total_people_Female", "pct_female", "pct_total_people_Female", "pct_wage")]
								
				#iv. Education 
					#Create component tables
						tbl4a<-acs_anl %>% group_by(educ) %>% summarize(
												avg_wage=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
											,	total_people=sum(pwgtp)
											)
						tbl4a<-as.data.frame(tbl4a)
					
						tbl4b<-acs_anl %>% group_by(educ, sex) %>% summarize(
												avg_wage=weighted.mean(wagp_infl, w=pwgtp, na.rm=TRUE)
											,	total_people=sum(pwgtp)
											)
						tbl4b<-as.data.frame(tbl4b)
						tbl4b_w<-reshape(tbl4b, idvar=c("educ"), timevar=c("sex"), direction="wide", sep="_")
									
					#Combine
						#Merge Together
						tbl4<-merge(tbl4a, tbl4b_w, by="educ", all=TRUE)
						
						#Create Percentages
						tbl4$pct_male<-tbl4$total_people_Male/tbl4$total_people
						tbl4$pct_female<-tbl4$total_people_Female/tbl4$total_people
						
						tbl4$pct_total_people<-tbl4$total_people/sum(tbl4$total_people,na.rm=TRUE)
						tbl4$pct_total_people_Male<-tbl4$total_people_Male/sum(tbl4$total_people_Male,na.rm=TRUE)
						tbl4$pct_total_people_Female<-tbl4$total_people_Female/sum(tbl4$total_people_Female,na.rm=TRUE)
						tbl4$pct_wage<-tbl4$avg_wage_Female/tbl4$avg_wage_Male
			
						#Finalize Table
						tbl4<-tbl4[c("educ", "avg_wage", "total_people", "pct_total_people"
								, 	"avg_wage_Male", "total_people_Male", "pct_male", "pct_total_people_Male"
								, 	 "avg_wage_Female", "total_people_Female", "pct_female", "pct_total_people_Female", "pct_wage")]

	#C. Summary Cross Tables
		tbl5<-acs_anl %>% group_by(occp_descr, yr, sex) %>% summarize(total_pop=sum(pwgtp, na.rm=TRUE))
		tbl5<-as.data.frame(tbl5)
		tbl5<-reshape(tbl5, idvar=c("occp_descr", "sex"), timevar=c("yr"), direction="wide")
		tbl5<-reshape(tbl5, idvar=c("occp_descr"), timevar=c("sex"), direction="wide")
							
#IV. Data Output
	#Graphs
	ggsave(file="../output/003_graph1.pdf", height=8, width=11,graph1)
	ggsave(file="../output/003_graph2.pdf", height=8, width=11,graph2)
	ggsave(file="../output/003_graph3.pdf", height=8, width=11,graph3)
	ggsave(file="../output/003_graph4.pdf", height=8, width=11,graph4)
	ggsave(file="../output/003_graph5.pdf", height=8, width=11,graph5)
	ggsave(file="../output/003_graph6.pdf", height=8, width=11,graph6)
	ggsave(file="../output/003_graph7.pdf", height=8, width=11,graph7)
	ggsave(file="../output/003_graph8.pdf", height=8, width=11,graph8)
	ggsave(file="../output/003_graph9.pdf", height=8, width=11,graph9)

	ggsave(file="../output/003_hist1.pdf", height=8, width=11,hist1a)
	ggsave(file="../output/003_hist2.pdf", height=8, width=11,hist1b)
	ggsave(file="../output/003_hist3.pdf", height=8, width=11,hist1d)
	ggsave(file="../output/003_hist4.pdf", height=8, width=11,hist1e)
	ggsave(file="../output/003_hist5.pdf", height=8, width=11,hist1f)

	ggsave(file="../output/003_hist6.pdf", height=8, width=11,hist2a)
	ggsave(file="../output/003_hist7.pdf", height=8, width=11,hist2b)
	ggsave(file="../output/003_hist8.pdf", height=8, width=11,hist2d)
	ggsave(file="../output/003_hist9.pdf", height=8, width=11,hist2e)
	ggsave(file="../output/003_hist10.pdf", height=8, width=11,hist2f)	
	
	write.csv(file="../output/003_tbl1.csv", tbl1)
	write.csv(file="../output/003_tbl2.csv", tbl2)
	write.csv(file="../output/003_tbl3.csv", tbl3)
	write.csv(file="../output/003_tbl4.csv", tbl4)
	write.csv(file="../output/003_tbl5.csv", tbl5)
