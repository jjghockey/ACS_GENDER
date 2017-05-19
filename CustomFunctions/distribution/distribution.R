### Custom Functions
### Purpose: Automate the creation of value count tables and distribution charts.
### Author: jjg
### Last Updated: 1/19/2017


# Overview: There are three functions in this script:
#   distribution() : create value counts table and distribution chart for a single field
#   distribution_all(): given a table, run distribution() on each column
#   vba_combine_and_format(): Automatically generate the VBA necessary to combine
#                             the distribution table CSVs.

# distribution(): create value counts table and distribution chart for a single field
distribution <- function(df, col, prefix="", tbl_out_path="output/tables/",
                             chart_out_path="output/charts/", max_unique=10000){
  # INPUT: 
  #   df - data.frame or tbl_df object. This should be a relatively clean and converted
  #         full dataset (in the sense that it is unsummarized - no value counts), unless 
  #         the col argument is set to the special value of "__counts__", in which case 
  #         it should be a table with two columns: the first has the unique values of a 
  #         field and the second has the counts.
  #   col - character vector of a single column name (multiple currently not supported).
  #         set to the special value of "__counts__" if the table you're passing in 
  #         is already a summary of a field with unique values in the first column
  #         and counts in the second.
  #   prefix - character string to be prepended to the filenames on output. For 
  #            example, an abbreviation for the table name
  #   tbl_out_path - relative or full folder path where the tables should be written
  #   chart_out_path - same, but for charts
  # OUTPUT:
  #   to console: name of field
  #   to tbl_out_path: CSV with unique value counts (if not a continuous variable)
  #   to chart_out_path: Single page PDF of distribution (if not a binary categorical)
  
  require(readr)
  require(ggplot2)
  require(scales)
  require(stringr)
  require(dplyr)
  
  # Make sure paths end in a slash
  if(str_extract(tbl_out_path, ".$") != "/") tbl_out_path <- paste0(tbl_out_path, "/")
  if(str_extract(chart_out_path, ".$") != "/") chart_out_path <- paste0(chart_out_path, "/")
  
  # If __counts__ was specified, set col equal to the first field in the tbl
  if(col == "__counts__"){
    col <- names(df)[1]
    already_counts <- TRUE
  } else {
    already_counts <- FALSE
  }
  
      
  # Store field class and number of unique values
  class <- class(df[[col]])[1]
  n_dist <- n_distinct(df[[col]])
  
  ### Summarize unique values and counts
  if(class %in% c("character", "factor", "Date", "POSIXct", "numeric", "integer", "integer64")){
    # If numeric and more than 100 values, don't output summary table
    if(class %in% c("numeric", "integer", "integer64") & n_dist > 100){
      cat(" - Numeric and more than 100 unique values; skipping table")
    } else {
      
      full_smry <- df
      samp_indicator <- "" #Only flag if a sample if taken, below
      
      # Create summary table, if it's not already a counts table
      if(!already_counts){
        # If there are more than 10x max_unique values, just take the first 5 values,
        #   the last 5 values, and a sample of 9,900 others
        if(n_dist > max_unique & nrow(full_smry) > max_unique * 10){
          full_smry <- bind_rows(full_smry %>% select_(col) %>% head(5), 
                                 full_smry %>% select_(col) %>% tail(5),
                                 full_smry %>% select_(col) %>% sample_n(max_unique - 10))
          samp_indicator <- "(sample)"
        }
        
        # Summarize table
        full_smry <- full_smry %>%   
          group_by_(col) %>% 
          summarize(count=n())
      } else { 
        # else if already a counts table, just rename it to be consistent
        names(full_smry) <- c(col, "count")
      }
      # If categorical, sort by counts, descending
      if(class %in% c("character", "factor")) full_smry <- full_smry %>% arrange(-count)
      # If date, sort by date
      if(class %in% c("date", "POSIXct")) full_smry <- full_smry %>% arrange_(col)
      
      # Output
      write_csv(full_smry, paste0(tbl_out_path, prefix, col, samp_indicator, ".csv"), na = "[[NA]]")
    }
  }
  
  if(n_dist <= 2){
      cat(" - Only 2 levels; skipping chart")
      return(NULL)
  }
  
  ### Plot: Categorical
  if(class %in% c("character", "factor")){
    # Add rank to summary table
    smry <- full_smry %>% 
      mutate(rank=rank(-count, na.last=FALSE, ties.method="first"))
    
    # Set NAs to "[[NA]]"
    stopifnot(!("[[NA]]" %in% smry[[col]]))
    smry[[col]][is.na(smry[[col]])] <- "[[NA]]"
    
    # If more than 20 unique values, combine the 21+ least frequent ones
    if(any(smry$rank > 20)){
      above20 <- smry %>% 
        filter(rank > 20) %>% 
        summarize(z=paste0("(", sum(smry$rank > 20), " others)"), 
                  count=sum(count), rank=21)
      names(above20)[1] <- col
      smry <- bind_rows(smry %>% filter(rank <= 20), above20)
    
      # If too many unique values, don't plot at all
      if(smry$count[nrow(smry)] > 40 * smry$count[1] | smry$count[2] == 1){
        cat(" - Distinct values too infrequent; skipping chart.")
        return(NULL)
      }
    } 
      
    # Set factor levels so bar chart is ordered
    smry[[col]] <- factor(smry[[col]], levels=rev(smry[[col]]))
      
    # Plot bar chart for categorical variable
    plt <- ggplot(smry, aes_string(x=col, y="count")) + 
      geom_bar(stat="identity", fill="steelblue3") + 
      scale_y_continuous(labels=comma) +
      labs(title=paste0("\nBar Chart: ", col, "\n"), x="", y="\nCount") +
      coord_flip() + theme_bw()
  } else if(class %in% c("Date", "POSIXct")){
  ### Plot: Date/POSIXct
    # Convert POSIXct to date ... not going to plot times for now.
    if(class == "POSIXct") df[[col]] <- as_date(df[[col]])
    
    na_message <- ifelse(any(is.na(df[[col]])), 
                         paste0("Removed ", comma(sum(is.na(df[[col]])))," missing values.\n"), "")
    
    # Plot date/POSIXct histogram with one bin per day regardless of time period.
    weight <- if(already_counts) names(df)[2] else NULL
    plt <- ggplot(df %>% filter_(paste0("!is.na(", col, ")")), 
                  aes_string(x=col, weight=weight)) + 
      geom_histogram(binwidth=1, fill="steelblue3") +
      scale_y_continuous(labels=comma) +
      labs(title=paste0("\nHistogram: ", col, "\n", na_message), x="", y="\nCount") + 
      theme_bw()
  } else if(class %in% c("numeric", "integer", "integer64")){
  ### Plot: Numeric
    # First, plot all data
    # If range is under 1000, use clean binwidths of 1/5/10. Otherwise, just use 50 bins.
    binw <- ifelse(diff(range(df[[col]], na.rm=T)) <= 100, 1,
            ifelse(diff(range(df[[col]], na.rm=T)) <= 500, 5,
            ifelse(diff(range(df[[col]], na.rm=T)) <= 1000, 10,
                   NA)))
    nbins <- ifelse(is.na(binw), 50, NA)
    if(is.na(binw)) binw <- NULL else nbins <- NULL
    
    # Histogram
    na_message <- ifelse(any(is.na(df[[col]])), 
                         paste0("Removed ", comma(sum(is.na(df[[col]])))," missing values.\n"), "")
    title <- paste0("\nHistogram: ", col, "\n", na_message)
    weight <- if(already_counts) names(df)[2] else NULL
    plt <- ggplot(df %>% filter_(paste0("!is.na(", col, ")")), aes_string(x=col, weight=weight)) + 
      geom_histogram(bins=nbins, binwidth=binw, fill="steelblue3") +
      scale_x_continuous(labels=comma) +
      scale_y_continuous(labels=comma) +
      labs(title=title, x="", y="\nCount") + 
      theme_bw()
    
    # Then, check for outliers. If the range of the chart shrinks outside 3
    # standard deviations, and there are more than 100 unique values
    sd <- sd(df[[col]], na.rm=T)
    mean <- mean(df[[col]], na.rm=T)
    df_trimmed <- df %>% filter_(paste0(col, " < ", round(mean + sd * 3, 6), " & ", 
                                        col, " > ", round(mean - sd * 3,6)))
    n_removed <- nrow(df) - nrow(df_trimmed)
    
    # If there are outliers, clip them and plot another histogram
    if(n_removed > 0 & n_dist > 100){
      # Redefine bin parameters
      binw <- ifelse(diff(range(df_trimmed[[col]], na.rm=T)) <= 100, 1,
              ifelse(diff(range(df_trimmed[[col]], na.rm=T)) <= 500, 5,
              ifelse(diff(range(df_trimmed[[col]], na.rm=T)) <= 1000, 10,
                     NA)))
      nbins <- ifelse(is.na(binw), 50, NA)
      if(is.na(binw)) binw <- NULL else nbins <- NULL
        
      # Trimmed histogram
      plt_trimmed <- ggplot(df_trimmed %>% filter_(paste0("!is.na(", col, ")")), aes_string(x=col)) + 
        geom_histogram(bins=nbins, binwidth=binw, fill="steelblue3") +
        scale_x_continuous(labels=comma) +
        scale_y_continuous(labels=comma) +
        labs(title=paste0("\nHistogram: ", col, "\n", comma(nrow(df) - nrow(df_trimmed)), 
                           " observations outside 3 standard deviations have been removed", ""),
             x="", y="\nCount") + 
        theme_bw()
      # Save trimmed histogram separately
      ggsave(filename = paste0(chart_out_path, prefix, col, "_trimmed.pdf"),
         plot = plt_trimmed, width = 10, height=10)
    }
  } else{
  ### None of the above
    cat(paste0("No chart code written yet for column ", col, " of type ", class, "."))
    return(NULL)
  }
  
  # Save chart
  ggsave(filename = paste0(chart_out_path, prefix, col, ".pdf"),
         plot = plt, width = 10, height=10)
}

# distribution_all(): given a table, run distribution() on each column
distribution_all <- function(df, prefix="", tbl_out_path="output/tables/",
                                 chart_out_path="output/charts/"){
  # run distribution() on each column
  for(col in names(df)){
    cat(paste0("\n", col))
    distribution(df, col, prefix = prefix, tbl_out_path = tbl_out_path,
                     chart_out_path = chart_out_path)
  }
  
  # Print instructions and code to run vba_combine_and_format
  if(col == names(df)[ncol(df)]){
    cat(paste0("\nRun \"writeClipboard(vba_combine_and_format(\"",
             tbl_out_path, "\", \"", prefix, "\"))\"\nfor the VBA code to combine the tables."))
  }
}


# vba_combine_and_format(): Automatically generate the VBA necessary to combine
#                             the distribution table CSVs.
vba_combine_and_format <- function(path, prefix=""){
  require(stringr)
  
  # If the path isn't a full path, add the working directory to it
  if(substring(str_replace(path, "\\\\", "/"), 1, 2) != "//") path <- paste0(getwd(), "/", path)
  
  # Retrieve code to combine CSVs
  vba_combine <- readLines("CustomFunctions/excel_vba/combine_csvs_into_one_xlsx.txt")
  
  # Modify paths and prefixes for code to combine CSVs
  path <- str_replace(path, "/$", "")
  vba_combine[which(str_detect(vba_combine, "^MyFolder"))] <- 
    paste0("MyFolder = \"", str_replace_all(path, "/", "\\\\"), "\"")
  vba_combine[which(str_detect(vba_combine, "^MyFile"))] <- 
    paste0("MyFile = Dir(MyFolder & \"\\", prefix, "*.csv\")")
  
  # Retrieve code to format a tab
  vba_format <- readLines("CustomFunctions/excel_vba/simple_value_counts_format.txt")
  
  # Write code to format each tab
  vba_format_each <- c("For i = 1 To Sheets.Count", "    Sheets(i).Activate",
                       "    Call simple_value_counts_format", "Next i")
  
  # Combine all the code together
  full_code <- c(vba_format, vba_combine[1:(which(vba_combine == "End Sub")-1)],
                 vba_format_each, "End Sub")
  
  cat("1. Wrap this function call in writeClipboard(), if you haven't already.\n")
  cat("2. Open a new Excel Workbook.\n")
  cat("3. Press Alt + F11\n")
  cat("4. Press Alt + I + M\n")
  cat("5. Ctrl + V to paste\n")
  cat("6. Press F5\n")
  return(full_code)
}