#Version		: 	1.0
#Author			: 	jjg 
#Last Update	: 	1/19/2017
#Purpose		: 	Set cleaning steps for all data
#Argument(s)	:	the function requires one or more data frame objects
#Input 			:	list of dataframes
#Output			:	combined dataframe
#External		:	gdata
#Example Syntax	: 	df<-clean(df1=c(), datefmt=c(), datecol=c(), datetimefmt=c(), datetimecol=c() ) 

#Other			: 	

clean <- function(dta=c(), datefmt=c(), datecol=c(), datetimefmt=c(), datetimecol=c(), na.strings=c("") ) {
	# input: 	dta is a list of dataframes 
	# output: 	out is a single combined dataframe
	# external: gdata
	
	require(gdata)

	#1. Remove browser debug
	browser <- list
		
	#2. Setup loop for each dataframe
	#	Parse data frame objects
				
	#3. Prepare cleaning steps		
		n<-1
		dta1<-length(dta)
		if (dta1==0) stop("Unspecified dta")
		
		for (o in dta) {
		
			print(paste(n, ". Processing ", o, "...", sep=""))
	
			tmp_dta<-get(o)
			
			#A. Standardize Fields
				print("     --Standardizing fields...")
				#1. Numeric - Nothing to do
							
				#2. Factor - Set to Character
					tp<-as.data.frame.matrix(summary.default(tmp_dta))
					fac<-rownames(tp[which(tp$Class=="factor"),])
					for (i in fac ) {
						tmp_dta[,c(i)]<-as.character(tmp_dta[,c(i)])
					}
				
				#3. Character
					tp<-as.data.frame.matrix(summary.default(tmp_dta))
					chr<-rownames(tp[which(tp$Mode=="character"),])
					for (i in chr ) {
						tmp_dta[,c(i)]<-trim(tolower(tmp_dta[,c(i)]))
					}
						
			#B. Standardize names
				print("     --Standardizing field names...")	
				#1. Lowercase 
					names(tmp_dta)<-tolower(names(tmp_dta))
				#2. Remove Punctuation
					names(tmp_dta)<-gsub("[^a-z0-9]", "", names(tmp_dta))
				#3. Trim 
					names(tmp_dta)<-trim(names(tmp_dta))
									
			#C. Remove Blanks
				print("     --Removing blanks and replacing with NA...")
				for (i in na.strings ) {
					i<-tolower(trim(i))
					tmp_dta[tmp_dta==i]<-NA
				}
					
			#D. Convert Dates				
				d1<-length(datefmt)
				d2<-length(datecol)
				
				colpos<-names(tmp_dta)
				
				if(d1!=d2) stop("ERROR. Unbalanced date column and date format.  Please specify one date format for each date column")
				
				if(d1>0) print("     --Converting date values...")
								
				j<-1
				for (i in c(datecol) ) {
					i<-tolower(i)
					i<-gsub("[^a-z0-9]", "", i)
					i<-trim(i)
					tmp_dta$tmpdt <- as.Date(tmp_dta[,c(i)], format=datefmt[j] )
					chkna<-nrow(tmp_dta[which(is.na(tmp_dta$tmpdt)==TRUE),])
					chkna2<-nrow(tmp_dta[which(is.na(tmp_dta[,c(i)])==TRUE),])
					if (chkna!=chkna2) stop(paste("ERROR. NAs generated when converting", i, sep=" "))
					tmp_dta[,c(i)]<-tmp_dta$tmpdt
					tmp_dta$tmpdt<-NULL
					j<-j+1
				}
				
				tmp_dta<-tmp_dta[c(colpos)]
		
			#E. Convert Datetimes
				d1<-length(datetimefmt)
				d2<-length(datetimecol)
				
				colpos<-names(tmp_dta)
				
				if(d1!=d2) stop("ERROR. Unbalanced date column and date format.  Please specify one date format for each date column")
				
				if(d1>0) print("     --Converting datetime values")
				
				j<-1
				for (i in c(datetimecol) ) {
					i<-tolower(i)
					i<-gsub("[^a-z0-9]", "", i)
					i<-trim(i)
					tmp_dta$tmpdt <- as.POSIXct(tmp_dta[,c(i)], format=datetimefmt[j], tz="GMT" )
					chkna<-nrow(tmp_dta[which(is.na(tmp_dta$tmpdt)==TRUE),])
					chkna2<-nrow(tmp_dta[which(is.na(tmp_dta[,c(i)])==TRUE),])
					if (chkna!=chkna2) stop(paste("ERROR. NAs generated when converting", i, sep=" "))
					tmp_dta[,c(i)]<-tmp_dta$tmpdt
					tmp_dta$tmpdt<-NULL
					j<-j+1
				}
				tmp_dta<-tmp_dta[c(colpos)]	
				
			#F. Add Source Variable, Row Order and Combine			
				tmp_dta$.src<-o
				
				tmp_dta$.ord<-1:nrow(tmp_dta)
				
				if (dta1>1 & n>1) print("     --Combining data...")
				
				if (n==1) {
					out<-tmp_dta
				}
				else {
					out<-rbind(out,tmp_dta)
				}
				n<-n+1
		}

	#5. Summary of Output
		chk_src<-nrow(out[which(is.na(tmp_dta$.src)==TRUE),])
		if (chk_src>0) stop("ERROR. NAs created when generating source variable")
		
		print(paste(n, ". Summary of Output...", sep=""))
		tbl<-as.data.frame(table(out[c(".src")], useNA="ifany"))
		names(tbl)<-c("File", "Observation Count")
		tbl_t<-cbind(as.data.frame(c("Total")), as.data.frame(nrow(out)))
		names(tbl_t)<-c("File", "Observation Count")
		tbl<-rbind(tbl,tbl_t)
		print(tbl)
		print(str(out))
		
	#6. Return browser to normal
		rm(browser)	
	#7. Output
		return(out)		
}	