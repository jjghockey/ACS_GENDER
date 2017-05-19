#Version		: 	1.0
#Author			: 	jjg 
#Last Update	: 	1/19/2017
#Purpose		: 	Better merging of data frames
#Argument(s)	:	the function requires two dataframe objects
#Input 			:	dataframe
#Output			:	dataframe
#External		:	dplyr
#Example Syntax	: 	df<-merge2(df1, df2, by.x(), by.y(), explosion=FALSE)

#Other			: 	nci_merge only performs a full outer join

merge2 <- function(df1,df2, by.x=c(), by.y=c(), explosion=FALSE) {
	# input: 	df1 is a dataframe 
	# input: 	df2 is a dataframe
	# output: 	a master data frame with all components of df1 and df2 with a merge variable indicating how the join worked
	# external: dplyr
	
	require(dplyr)
	
	#1. Remove browser debug
	browser <- list
	
	#2. Test that df1 and df2 were supplied (this will trigger an error if df1 or df2 were given)
	df1<-df1
	df2<-df2
		
	#3. Parse Joining Fields
		#a. Determine length of each join field
		l1<-length(by.x)
		l2<-length(by.y)

		#b. Test to length of each l1 and l2
		if(l1==0) stop("Unspecified by.x")
		if(l2==0) stop("Unspecified by.y")
	
		#c.Test to compare l1 and l2
		if(l1!=l2) stop("Unbalanced by.x and by.x.  Please check that you are merging on the same number variables")
	
	
	#4. Prepare to merge 
		#a. The merge summary variable 
		df1$.m<-1
		df2$.m<-1
	
		#b. Build join 
		df3<-merge(df1,df2, by.x=c(by.x), by.y=c(by.y), all=TRUE)
 		
		#c. Create Merge Summary Variable
		df3$.m[df3$.m.x==1 & df3$.m.y==1]<-3			#Inner Join 
		df3$.m[is.na(df3$.m.x)==TRUE & df3$.m.y==1]<-2	#Right Join
		df3$.m[df3$.m.x==1 & is.na(df3$.m.y)==TRUE]<-1	#Left Join

		df3$.m.x<-NULL
		df3$.m.y<-NULL

		#d. Create merge summary table
		tbl<-as.data.frame(table(df3$.m))
	
		tbl$desc[tbl$Var1==3]<-"Match"
		tbl$desc[tbl$Var1==2]<-"Righthand Side"
		tbl$desc[tbl$Var1==1]<-"Lefthand Side"
		names(tbl)<-c(".m", "cnt", "descr")
		
		#e. Test for join explosion
			#i. Original Row Counts
			cnt1<-nrow(df1)
			cnt2<-nrow(df2)
			
			#ii. New Row Counts
			cnt1_new<-nrow(df3[which(df3$.m==3 | df3$.m==1),])
			cnt2_new<-nrow(df3[which(df3$.m==3 | df3$.m==2),])
				
			#iii. Final Row Counts
			cnt3<-nrow(df3)
				
			#iv. Check and stop
			if (  (cnt1 != cnt1_new | cnt2 != cnt2_new ) & explosion==TRUE ) stop("Join explosion.  Please ensure that your joining fields are unique")
			
			#v. Flag and identify rows that explode	
			if (  (cnt1 != cnt1_new | cnt2 != cnt2_new ) & explosion==FALSE ) { 
				lst<-c(by.x)
				df1_ <- df1 %>% group_by_(.dots=lst) %>% summarize(cnt=n()) %>% filter(cnt>1) %>% select_(.dots=lst)
				lst<-c(by.y)
				df2_ <- df2 %>% group_by_(.dots=lst) %>% summarize(cnt=n()) %>% filter(cnt>1) %>% select_(.dots=lst)
				names(df2_)<-names(df1_)
				df3_<-unique(rbind(df1_, df2_))
				df3_<-as.data.frame(df3_)
				df3_$.expl<-1
				df3<-merge(df3,df3_, by=c(by.x), all.x=TRUE)
				df3$.expl[is.na(df3$.expl)==TRUE]<-0
			}
			#vi. Prepare summary of exploded records
			tbl_expl1<-data.frame(cnt1, cnt1_new, "Lefthand Side")
			names(tbl_expl1)<-c("orig_cnt", "matched_cnt", "descr")
			tbl_expl2<-data.frame(cnt2, cnt2_new, "Righthand Side")
			names(tbl_expl2)<-c("orig_cnt", "matched_cnt", "descr")	
			tbl_expl3<-data.frame(cnt3, cnt3, "Total")
			names(tbl_expl3)<-c("orig_cnt", "matched_cnt", "descr")	
			
			tbl_expl<-rbind(tbl_expl1, tbl_expl2, tbl_expl3)
			
		#f. Print results
		print("Summary of Match")
		print(tbl)
		print("Summary of Explosion")
		print(tbl_expl)
	
	#5. Return browser to normal
	rm(browser)

	#6. Return data
	return(df3)
}	