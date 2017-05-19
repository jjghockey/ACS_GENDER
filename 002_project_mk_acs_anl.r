#####################################################################################################
#Engagement		-	UCLA MAS - STAT 404 - Project													#
#FileName		-	002_project_mk_acs_anl.r				  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	2/9/2017									  									#
#																	  								#
#Purpose:		-	Variable Selection and Tranformation 											#
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
	
	#Custom Functions
	source("CustomFunctions/clean/clean.r")
	source("CustomFunctions/merge2/merge2.r")
	source("CustomFunctions/dataprofile/dataprofile.r")
	source("CustomFunctions/distribution/distribution.r")
	
#II. Data Loading 
	#A. ACS
		load("../data/acs.rda")
		
	#B. CA CPI
		#1. Load data
			cpi<-read.csv("../raw/EntireCCPI.csv")  #Use to standardize income to constant dollars (original file is 1982=1984).  
		
		#2. Clean data
			cpi<-clean(dta=c("cpi"))
		
		#3. Filter to annual records
			cpi<-cpi[which(cpi$month=="annual"),c("year", "allurbanconsumers")]
			names(cpi)<-c("yr", "cpi")
			cpi$cpi<-as.numeric(cpi$cpi)
		
		#4. Convert cpi to constant 2015 dollars
			cpi2015<-cpi[which(cpi$yr==2015), c("cpi")]
			cpi$cpi2015<-cpi$cpi/cpi2015
			
		#5. Filter to 2010 to 2015
			cpi<-cpi[which(cpi$yr %in% c(2010, 2011, 2012, 2013, 2014, 2015)),]
			
	#C. OCCP Codes from PUMS data dictionary
		occ<-read.table("../raw/PUMSDataDict15_OCCP.csv", sep="|", header=TRUE) #This file has no delimiter.  Using PIPE to trick the file read in.
		names(occ)<-"occp_full"
		occ$occp_code<-substr(occ$occp_full,1,regexpr(" \\.", occ$occp_full))
		occ$occp_descr<-tolower(substr(occ$occp_full,regexpr(" \\.", occ$occp_full)+2, 255))
		occ$occp_ind<-tolower(substr(occ$occp_descr, 1,regexpr("\\-", occ$occp_descr)-1))
		
	#D. PUMA Codes
		geo<-read.csv("../raw/geocorr12.csv")
		
		#1. Divide into PUMA 2012 
		geo12<-as.data.frame(unique(geo[c("puma12", "cntyname", "intptlon", "intptlat")]))
		names(geo12)<-c("puma", "cntyname", "lon", "lat")
		geo12 <- geo12 %>% group_by(puma,cntyname) %>% summarize(lat=min(lat), lon=min(lon))
		geo12$cntyname <- as.character(geo12$cntyname)
		geo12<-as.data.frame(geo12)
		geo12 <- geo12 %>% distinct() %>% group_by(puma) %>% 
						 summarize(cntyname = paste(cntyname, collapse = '/'), lat=min(lat), lon=min(lon))
		geo12<-as.data.frame(geo12)
		
		#2. Divide into PUMA 2010
		geo10<-as.data.frame(unique(geo[c("puma2k", "cntyname", "intptlon", "intptlat")]))
		names(geo10)<-c("puma", "cntyname", "lon", "lat")	
		geo10 <- geo10 %>% group_by(puma,cntyname) %>% summarize(lat=min(lat), lon=min(lon))		
		geo10$cntyname <- as.character(geo10$cntyname)
		geo10 <-as.data.frame(geo10)			
		geo10 <- geo10 %>% distinct() %>% group_by(puma) %>% 
						 summarize(cntyname = paste(cntyname, collapse = '/'), lat=min(lat), lon=min(lon))
		geo10<-as.data.frame(geo10)		

#III. Data Processing 
				
	#A. External Data
		#1. CPI 
			acs$yr<-as.numeric(paste("20", gsub("acs", "", acs$.src), sep=""))
			acs$.src<-NULL
			acs<-merge2(acs, cpi, by.x=c("yr"), by.y=c("yr"))
			acs$.m<-NULL
			acs$.expl<-NULL
			
		#2. OCCP Codes
			acs$occp<-as.numeric(as.character(acs$occp))
			occ$occp_code<-as.numeric(occ$occp_code)
			acs<-merge2(acs,occ, by.x=c("occp"), by.y=c("occp_code"))
			
			acs$occp_descr[is.na(acs$occp_descr)==TRUE & is.na(acs$occp)==FALSE]<-"unknown"
			acs$occp_ind[is.na(acs$occp_ind)==TRUE & is.na(acs$occp)==FALSE]<-"unk"
			
		#3. Add PUMA Geo Codes 2012 and forward
			acs12<-acs[which(acs$yr %in% c(2012, 2013, 2014, 2015)),]
			acs12<-merge(acs12, geo12, by.x=c("puma"), by.y=c("puma"), all.x=TRUE)	
			
			acs10<-acs[which(acs$yr %in% c(2010, 2011)),]		
			acs10<-merge(acs10, geo10, by.x=c("puma"), by.y=c("puma"), all.x=TRUE)	
					
			nrow(acs)
			acs<-rbind(acs10, acs12)
			nrow(acs)
			table(is.na(acs$cntyname))
			  # FALSE 
			# 2216591 
			acs$.m<-NULL
			acs$.expl<-NULL	
		
			acs$cntyname<-as.factor(acs$cntyname)			
							
	#B. Data Selection
		#1. Variable Selection 
			acs<-acs[c("rt", "serialno", "sporder", "puma", "st", "adjinc", "pwgtp", "agep", "cit", "cow", "fer", "mar", "marhd", "marhm", "marht", "marhw"
					,  "mig", "esr", "sch", "schg", "schl", "sex", "wagp", "wkhp", "wkl", "wkw", "wrk", "indp", "migpuma", "msp", "naicsp", "pernp"
					,  "pincp", "socp", "yr", "cpi2015", "occp", "occp_descr", "occp_ind", "cntyname")]
			
			rm(acs10, acs12)
			gc(reset=TRUE)
		
		#2. Filter Selection (Fully Employeed Workers, who worked the entire past year)
			nrow(acs) #2216591
			
			acs<-acs[which(acs$wkw %in% c(1,2)),] #wkw == 1 or 2 is a person who worked from 48 to 52 weeks in the prior year
			nrow(acs) #809660
			
			acs<-acs[which(is.na(acs$wagp)==FALSE),] #Actual earnings from salary / wages is reported.
			nrow(acs) #809660
		
			acs<-acs[which(acs$wagp>0),] #Positive earnings
			nrow(acs) #750614
			
			acs<-acs[which(acs$cow %in% c(1,2)),] #cow indicates employment type.  Only taking private (non-government employees)
			nrow(acs) #586832
			
			acs<-acs[which(acs$cit %in% c(1,2,3,4)),] #cit is citizenship.  Only taking citizens
			nrow(acs) #483222
			
			acs<-acs[which(acs$esr==1),] #esr is employment status.  Only taking currently employed
			nrow(acs) #468475

	#C. Variable Creation 	
		#1. Inflation adjusted wages
			acs$wagp_infl<-acs$wagp*acs$adjinc*acs$cpi2015
			acs$pincp_infl<-acs$pincp*acs$adjinc*acs$cpi2015
			acs$pernp_infl<-acs$pernp*acs$adjinc*acs$cpi2015
			
		#2. Variable Recode - Creation
			#Education Level
				acs$schl<-as.numeric(as.character(acs$schl))
				acs$educ<-with(acs, {
						ifelse(schl <=15, "Less than Highschool",
						ifelse(schl >15 & schl<=17, "Highschool or Equivalent",
						ifelse(schl >17 & schl<=20, "Some College",
						ifelse(schl ==21, "Bachelors", 
						ifelse(schl ==22, "Masters", 
						ifelse(schl %in% c(23,24), "Professional or PhD", NA))))))
					}
				)
				acs$educ<-tolower(acs$educ)
				acs$educ<-as.factor(acs$educ)
				acs$schl<-as.factor(acs$schl)
			
		#3. Variable Recode Descriptions
			#a. Sex
				acs$sex<-as.character(acs$sex)
				
				acs$sex<-with(acs, {
						ifelse(acs$sex=="1", "Male", 
						ifelse(acs$sex=="2", "Female", NA))
					}
				)
				acs$sex<-as.factor(acs$sex)
				acs$sex<-factor(acs$sex, level=c("Male", "Female"))
				
			#b. Year
				acs$yr<-as.factor(acs$yr)
				
			#c. Baby born within the past 15 months
				acs$fer<-as.character(acs$fer)
				acs$fer<-with(acs, {
						ifelse(fer=="1", "Female - Baby Born within Past 15 Months",
						ifelse(fer=="2", "Female - No Baby Born within Past 15 Months",NA))
					}
				)
				acs$fer[is.na(acs$fer)==TRUE & acs$sex=="Male"]<-"Male"
				acs$fer[is.na(acs$fer)==TRUE & acs$sex=="Female"]<-"Female - No Baby Born within Past 15 Months"
				acs$fer<-as.factor(acs$fer)
				acs$fer<-factor(acs$fer, levels=c("Male", "Female - Baby Born within Past 15 Months", "Female - No Baby Born within Past 15 Months"))
				
			#d. Part / Full Time
				acs$ftpt<-with(acs, {
						ifelse(wkhp>=35 & wkhp<50, "Full Time (35 to 50 Hours)", 
						ifelse(wkhp>=50 & wkhp<=60, "Full Time (50 to 60 Hours)",
						ifelse(wkhp>60, "Full Time (Greater than 60 Hours)",
						ifelse(wkhp<35, "Part Time (Less than 35 Hours)", NA))))
					}
				)
				acs$ftpt<-tolower(acs$ftpt)
				acs$ftpt<-as.factor(acs$ftpt)
				
			#e. Tenure Proxy
				acs$age_cat<-with(acs, {
						ifelse(agep>=16 & agep<=22, "Less than 1 Year"
					,	ifelse(agep>22 & agep<=26, "1 to 4 Years"
					,	ifelse(agep>26 & agep<=29, "5 to 7 Years"
					,	ifelse(agep>29 & agep<=37, "8 to 15 Years"
					,	ifelse(agep>37 & agep<=42, "15 to 20 Years"
					, 	ifelse(agep>42 & agep<=52, "21 to 30 Years"
					,	ifelse(agep>52, "Greater than 30 Years",NA)))))))
					}
				)
				acs$age_cat<-tolower(acs$age_cat)
				acs$age_cat<-as.factor(acs$age_cat)
				
			#f. Industry / Job
				#1. Industry from NAICS
					acs$naicsp<-as.character(acs$naicsp)
					acs$ind<-with(acs, {
							ifelse(substr(naicsp,1,1)=="1", "Agriculture"
						,	ifelse(substr(naicsp,1,2)=="21", "Extraction"
						,	ifelse(substr(naicsp,1,2)=="22", "Utility"
						,	ifelse(substr(naicsp,1,2)=="23", "Construction"
						,	ifelse(substr(naicsp,1,1)=="3", "Manufacturing"
						,	ifelse(substr(naicsp,1,2)=="42", "Wholesale"
						,	ifelse(substr(naicsp,1,2) %in% c("44", "45"), "Retail"
						,	ifelse(substr(naicsp,1,2) %in% c("4m"), "Retail"
						,	ifelse(substr(naicsp,1,2) %in% c("48", "49"), "Transportation"
						,	ifelse(substr(naicsp,1,2)=="51", "Information"
						,	ifelse(substr(naicsp,1,2) %in% c("52", "53"), "Finance"	
						,	ifelse(substr(naicsp,1,2) %in% c("54", "55", "56"), "Professional"	
						,	ifelse(substr(naicsp,1,2)=="61", "Education"
						,	ifelse(substr(naicsp,1,3)=="624", "Community Services"
						,	ifelse(substr(naicsp,1,2)=="62", "Medical"
						,	ifelse(substr(naicsp,1,1)=="7", "Entertainment"						
						,	ifelse(substr(naicsp,1,1)=="8", "Personal Service"
						,	ifelse(substr(naicsp,1,3)=="928", "Military"
						,	ifelse(substr(naicsp,1,3)=="92m", "Government"						
						,	ifelse(substr(naicsp,1,2)=="92", "Government"
						,	ifelse(substr(naicsp,1,1)=="2", "Extraction", NA)))))))))))))))))))))						
						}
					)
					acs$ind<-as.factor(acs$ind)
					acs$naicsp<-as.factor(acs$naicsp)
					
				#2. Occupation Codes
					acs$occp_descr[acs$occp_descr=="mgr-miscellaneous managers, including funeral service managers and postmasters and mail superintendents"]<-"mgr-miscellaneous managers"
					acs$occp_descr<-gsub("mgr-", "", acs$occp_descr)	
					acs$occp_descr<-as.factor(acs$occp_descr)
								
					acs$occp_ind<-as.factor(acs$occp_ind)
					acs$occp<-as.factor(acs$occp)

			#g. Salaried Codes
				acs$tp<-"Not Exempt"
				acs$tp[grepl("actuaries", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("administrators", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("managers", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("supervisors", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("teacher", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("statistician", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("ists$", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("physicians", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("physical", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("engineers", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("officers", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("lawyers", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("judge", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("insurance", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("programmer", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("architects", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("veterinarians", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("sci\\-", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("fin\\-", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("inspectors", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("eng\\-", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("cmm\\-", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("prt\\-", acs$occp_descr)==TRUE]<-"Exempt"
				
				#2. Captured above, but likely Not Exempt
				acs$tp[grepl("first line", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("first-line", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("technicians", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("techs", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("hygienists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("typists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("technologists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("operators", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("specialists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("artists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("cosmetologists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("clerks", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("aides", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("assistants", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("transcriptionists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("therapists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("correctional", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("phlebotomists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("machinists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("police", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("pedicurists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("machine tool programmers", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("nurse", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("optometrists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("podiatrists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("therapists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("lifeguards", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("crossing guards", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("animal control workers", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("security guards", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("transportation", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("miscellaneous law enforcement workers", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("motion picture projectionists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("inspectors, testers, sorters, samplers", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("speech-language pathologists ", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("dietitians and nutritionists", acs$occp_descr)==TRUE]<-"Not Exempt"
				acs$tp[grepl("audiologists", acs$occp_descr)==TRUE]<-"Not Exempt"

				acs$tp[grepl("manager", acs$occp_descr)==TRUE]<-"Exempt"
				acs$tp[grepl("mgr\\-", acs$occp_descr)==TRUE]<-"Exempt"
				
				acs$tp[grepl("property, real estate, and community association managers", acs$occp_descr)==TRUE]<-"Not Exempt"
					
				acs$tp<-as.factor(acs$tp)	
					
	#D. Variable Cleanup
		#1. Variable Fix
		acs$.m<-NULL
		acs$.expl<-NULL
		acs$rt<-as.factor(acs$rt)
		
		#2. Below Minimum Wage
		acs$below_min<-0
		acs$below_min[(acs$wagp_infl/40/48)<=5]<-1 
		table(acs$below_min)
			# 0      1 
		# 437856  30619 
		
		#3. Relevel Education
		acs$educ<-factor(acs$educ, levels=c("less than highschool", "highschool or equivalent", "some college", "bachelors", "masters", "professional or phd"))
				
#IV. Data Output	
	#A. Save dataset
		acs_anl<-acs
		save(file="../data/acs_anl.rda", acs_anl)
