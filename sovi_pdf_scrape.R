# scrape pdf of sovi data

# ---- 1. set up ----

# clear ws
rm(list = ls())

# load libraries
library(pdftools)
library(tidyverse)

# load pdf
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/sovi_data/usc_sovi_data")
sovi_pdf=pdf_text("SoVI_10_14_Website.pdf")

# ---- 2. reformat pdf to data frame ----

# split lines
sovi_txt=unlist(strsplit(sovi_pdf,"\n")) # \n for new line: http://faculty.nps.edu/sebuttre/home/R/text.html

# save header
sovi_header=unlist(strsplit(trimws(sovi_txt[1]),"\\s+"))
sovi_num_cols=length(sovi_header)

# number of entries
#num_pgs=67
#num_lines_per_pg=47
#num_lines_per_pg_last_pg=42
#total_num_lines=(num_pgs-1)*num_lines_per_pg+num_lines_per_pg_last_pg

# alt. way (simpler) to determine number of entries
total_num_lines=length(sovi_txt)

# initialize lists
temp_FIP_Code=as.numeric()
temp_State_FIP=as.numeric()
temp_County_FIP=as.numeric()
temp_County_Name=as.character()
temp_CNTY_SoVI=as.numeric()
temp_Percentile=as.numeric()

for (i in 2:total_num_lines) {
  
  # look at one row
  temp_row_str=unlist(strsplit(sovi_txt[i],"\\s+"))
  
  # fix temp_row_str if county name gets broken in two
  if (length(temp_row_str)==(sovi_num_cols+1)) { # count name with two words
    if (grep(".",paste0(temp_row_str[4],"_",temp_row_str[5]))==1) { # if there's a period in the name
      temp_county_name=gsub("._","_",paste0(temp_row_str[4],"_",temp_row_str[5]))
      temp_row_str_fix=c(temp_row_str[1],temp_row_str[2],temp_row_str[3],temp_county_name,temp_row_str[6],temp_row_str[7])
    }
    else {
      temp_county_name=gsub(" ","_",paste(temp_row_str[4],temp_row_str[5]))
      temp_row_str_fix=c(temp_row_str[1],temp_row_str[2],temp_row_str[3],temp_county_name,temp_row_str[6],temp_row_str[7])
    }
  }
  else if (length(temp_row_str)==(sovi_num_cols+2)) { # county name with three words
    if (grep(".",paste0(temp_row_str[4],"_",temp_row_str[5],"_",temp_row_str[6]))==1) { # if there's a period in the name
      temp_county_name=gsub("._","_",paste0(temp_row_str[4],"_",temp_row_str[5],"_",temp_row_str[6]))
      temp_row_str_fix=c(temp_row_str[1],temp_row_str[2],temp_row_str[3],temp_county_name,temp_row_str[7],temp_row_str[8])
    }
    else {
      temp_county_name=gsub(" ","_",paste(temp_row_str[4],temp_row_str[5],"_",temp_row_str[6]))
      temp_row_str_fix=c(temp_row_str[1],temp_row_str[2],temp_row_str[3],temp_county_name,temp_row_str[7],temp_row_str[8])
    }
  }
  else if (length(temp_row_str)==(sovi_num_cols+3)) { # county name with four words
    if (grep(".",paste0(temp_row_str[4],"_",temp_row_str[5],"_",temp_row_str[6],"_",temp_row_str[7]))==1) { # if there's a period in the name
      temp_county_name=gsub("._","_",paste0(temp_row_str[4],"_",temp_row_str[5],"_",temp_row_str[6],"_",temp_row_str[7]))
      temp_row_str_fix=c(temp_row_str[1],temp_row_str[2],temp_row_str[3],temp_county_name,temp_row_str[8],temp_row_str[9])
    }
    else {
      temp_county_name=gsub(" ","_",paste(temp_row_str[4],temp_row_str[5],"_",temp_row_str[6],"_",temp_row_str[7]))
      temp_row_str_fix=c(temp_row_str[1],temp_row_str[2],temp_row_str[3],temp_county_name,temp_row_str[8],temp_row_str[9])
    }
  }
  else {
    temp_row_str_fix=temp_row_str
  }
  
  # save entry to temp column
  temp_FIP_Code[i-1]=as.numeric(temp_row_str_fix[1])
  temp_State_FIP[i-1]=as.numeric(temp_row_str_fix[2])
  temp_County_FIP[i-1]=as.numeric(temp_row_str_fix[3])
  temp_County_Name[i-1]=as.character(temp_row_str_fix[4])
  temp_CNTY_SoVI[i-1]=as.numeric(temp_row_str_fix[5])
  temp_Percentile[i-1]=as.numeric(temp_row_str_fix[6])
  
}

# save all to data frame
sovi_data=data.frame(FIP_Code=temp_FIP_Code,
                     State_FIP=temp_State_FIP,
                     County_FIP=temp_County_FIP,
                     County_Name=temp_County_Name,
                     CNTY_SoVI=temp_CNTY_SoVI,
                     Percentile=temp_Percentile)


# ---- 3. export data ----

# export reformatted data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
write_csv(sovi_data,"sovi_data.csv")
