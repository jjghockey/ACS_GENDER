#####################################################################################################
#Engagement		-	UCLA MAS - STAT 404 - Project 													#
#FileName		-	001_project_mk_acs.r					  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	2/9/2017									  									#
#																	  								#
#Purpose:		-	Prepare all raw data from ACS													#
#																									#
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
	
	#Custom Functions
	source("CustomFunctions/clean/clean.r")
	source("CustomFunctions/merge2/merge2.r")
	source("CustomFunctions/dataprofile/dataprofile.r")					  
	
#II. Data Loading 
	#A. Load each dataset into an R object
		acs10<-as.data.frame(read_csv("../raw/ss10pca.csv", col_names=TRUE))
		acs11<-as.data.frame(read_csv("../raw/ss11pca.csv", col_names=TRUE))
		acs12<-as.data.frame(read_csv("../raw/ss12pca.csv", col_names=TRUE))
		acs13<-as.data.frame(read_csv("../raw/ss13pca.csv", col_names=TRUE))
		acs14<-as.data.frame(read_csv("../raw/ss14pca.csv", col_names=TRUE))
		acs15<-as.data.frame(read_csv("../raw/ss15pca.csv", col_names=TRUE))
		
		save(file="../data/acs_raw.rda", acs10, acs11, acs12, acs13, acs14, acs15)

	#B. Determine variables that are similar across each data
		#1. Make datasets of each name vector
			n10<-as.data.frame(names(acs10))
			names(n10)<-c("var10")
			
			n11<-as.data.frame(names(acs11))
			names(n11)<-c("var11")
			
			n12<-as.data.frame(names(acs12))
			names(n12)<-c("var12")
			
			n13<-as.data.frame(names(acs13))
			names(n13)<-c("var13")
			
			n14<-as.data.frame(names(acs14))
			names(n14)<-c("var14")
			
			n15<-as.data.frame(names(acs15))
			names(n15)<-c("var15")
			
		#2. Merge together and compare differences
			tot_n<-merge2(n10, n11, by.x=c("var10"), by.y=c("var11"))
			tot_n[which(tot_n$.m!=3),]
			tot_n$.m<-NULL
			tot_n$.expl<-NULL
			
			tot_n<-merge2(tot_n, n12, by.x=c("var10"), by.y=c("var12"))
			tot_n[which(tot_n$.m!=3),]
			tot_n$.m<-NULL
			tot_n$.expl<-NULL

			tot_n<-merge2(tot_n, n13, by.x=c("var10"), by.y=c("var13"))
			tot_n[which(tot_n$.m!=3),]
			tot_n$.m<-NULL
			tot_n$.expl<-NULL

			tot_n<-merge2(tot_n, n14, by.x=c("var10"), by.y=c("var14"))
			tot_n[which(tot_n$.m!=3),]
			tot_n$.m<-NULL
			tot_n$.expl<-NULL

			tot_n<-merge2(tot_n, n15, by.x=c("var10"), by.y=c("var15"))
			tot_n[which(tot_n$.m!=3),]
			tot_n$.m<-NULL
			tot_n$.expl<-NULL
								
#III. Data Processing 
	#A. Clean and Combine
		#1. Determine which variables to drop 
			for (i in c("FDISP", "FPERNP", "FPINCP", "FPRIVCOVP", "FPUBCOVP", "RACNHPI", "RACNH", "RACPI", "SSPA", "MLPC", "MLPD", "MLPF", "MLPG", "MLPCD", "MLPFG", "FHICOVP")) {
				acs10[,c(i)]<-NULL
				acs11[,c(i)]<-NULL
				acs12[,c(i)]<-NULL
				acs13[,c(i)]<-NULL
				acs14[,c(i)]<-NULL
				acs15[,c(i)]<-NULL					
			}
			
		#2. Reorder all Variables
			var_ord<-names(acs10)
			acs11<-acs11[c(var_ord)]
			acs12<-acs12[c(var_ord)]
			acs13<-acs13[c(var_ord)]
			acs14<-acs14[c(var_ord)]
			acs15<-acs15[c(var_ord)]
					
		#3. Clean and combine
			acs<-clean(dta=c("acs10", "acs11", "acs12", "acs13", "acs14", "acs15"))
			rm(acs10, acs11, acs12, acs13, acs14, acs15)
			gc(reset=TRUE)
			
		#4. Setup Decimals for each variable
			#a. Weighted Variables do not have a decimal. Convert to number
				for (i in names(acs)[grepl("pwgtp", names(acs))] ) {
					print(i)
					acs[,c(i)]<-as.numeric(acs[,c(i)])
				}
			
			#b. Other Variables that do not have decimals, but coded as character
				fac<-as.data.frame.matrix(summary.default(acs))
				fac<-rownames(fac[which(fac$Mode=="character"),])
				fac<-fac[which(grepl("\\.src", fac)==FALSE)]
				fac<-fac[which(grepl("rt", fac)==FALSE)]
				fac<-fac[which(grepl("naicsp", fac)==FALSE)]
				fac<-fac[which(grepl("socp", fac)==FALSE)]
				fac<-fac[which(grepl("occp", fac)==FALSE)]
				
				for (i in c(fac) ) {
					print(i)
					acs[,c(i)]<-as.numeric(acs[,c(i)])
				}
			
			#c. Other Variables coded as numeric, but are categorical and should be factors
				varlist<-c(	"st","cit","cow","ddrs","dear","deye","dout","dphy","drat","dratx","drem","eng","fer"
							,"gcl","gcm","gcr","hins1","hins2","hins3","hins4","hins5","hins6","hins7","jwrip","jwtr","lanx"
							,"mar","marhd","marht","marhw","mig","mil","mlpa","mlpb","mlpe","mlph","mlpi","mlpj","mlpk","nwab"
							,"nwav","nwla","nwlk","nwre","relp","sch","schg","schl","sex","wkl","wkw","wrk","anc","anc1p","anc2p","decade"
							,"dis","drivesp","esp","esr","fod1p","fod2p","hicov","hisp","indp","jwap","lanp","migsp","msp","naicsp","nativity"
							,"nop","oc","occp","paoc","pobp","powsp","privcov","pubcov","qtrbir","rac1p","rac2p","rac3p","racaian","racasn"
							,"racblk","racsor","racwht","rc","sciengp","sciengrlp","sfn","sfr","socp","vps","waob","fagep","fancp"
							,"fcitp","fcitwp","fcowp","fddrsp","fdearp","fdeyep","fdoutp","fdphyp","fdratp","fdratxp","fdremp"
							,"fengp","fesrp","fferp","ffodp","fgclp","fgcmp","fgcrp","fhins1p","fhins2p","fhins3c","fhins3p","fhins4c","fhins4p"
							,"fhins5c","fhins5p","fhins6p","fhins7p","fhisp","findp","fintp","fjwdp","fjwmnp","fjwrip","fjwtrp","flanp"
							,"flanxp","fmarp","fmarhdp","fmarhmp","fmarhtp","fmarhwp","fmarhyp","fmigp","fmigsp","fmilpp","fmilsp"
							,"foccp","foip","fpap","fpobp","fpowsp","fracp","frelp","fretp"
							,"fschgp","fschlp","fschp","fsemp","fsexp","fssip","fssp","fwagp","fwkhp","fwklp","fwkwp","fwrkp","fyoep")
				
				for (i in varlist) {
					print(i)
					acs[,c(i)]<-as.factor(as.character(acs[,c(i)]))
				}
				
			#d. Adjust Income (6 implied decimal places)	
			acs$adjinc<-as.numeric(acs$adjinc)/1000000
				
#IV. Data Output
	#A. Profiles	
		acs_prof<-dataprofile(acs)
		write.csv(file="../output/001_acs_profile.csv", acs_prof)
		
	#B. Save dataset
		
		save(file="../data/acs.rda", acs)