 # r script for woods & pool data formatting

# ---- 1. set up ----

# load libraries
library(tidyverse)


# ---- 2. reformat WPCOMP data for nc ----

# set directory for WPCOMP data
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/WPCOMP")

# load in data
comp_data=read_csv("WPCOMP.csv",skip=3,col_names=TRUE,n_max=4246)
# these data are aggregated by county

# rename column names
comp_cols_original=colnames(comp_data)
comp_cols_new=gsub('\\s+', '', comp_cols_original) %>%
  gsub(',','_',.) %>%
  gsub('\\+','',.)
colnames(comp_data)=comp_cols_new

# save general rows
comp_usa_data=comp_data %>% filter(NAME=="UNITED STATES") # national
comp_se_data=comp_data %>% filter(REGION==5) # all of south east
comp_nc_data=comp_data %>% filter(NAME=="NORTH CAROLINA") # north carolina (statewide)
comp_nc_county_data=comp_data %>% filter(COUNTY>37000&COUNTY<37200) # north carolina (by county)

# fix name column of comp_nc_data to remove comma
comp_nc_county_data$NAME=gsub(', NC','',comp_nc_county_data$NAME)

# 

# set working directory to save data to
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/reformatted_data")

# export data
write_csv(comp_usa_data,"comp_usa_data.csv")
write_csv(comp_se_data,"comp_se_data.csv")
write_csv(comp_nc_data,"comp_nc_data.csv")
write_csv(comp_nc_county_data,"comp_nc_county_data.csv")


# ---- 3. reformatting WPGEO data for nc ----

# bring in info on yadkin

# set working directory to save data to
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")

# read in yadkin county names/numbers
yadkin_counties_sel=read_csv("yadkin_counties_sel.csv",col_names=TRUE)
yadkin_counties_sel=arrange(yadkin_counties_sel,COUNTY)

# go back to WPGEO data
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/WPGEO")

# load in row descriptions to be updated
my_vars=read_csv("WPGEO_row_rename.csv",col_names=TRUE,col_types=cols(.default = "c")) %>% select(var_name)

# set working directory for nc files
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/WPGEO/NC")

# import and edit only yadkin files from WPGEO nc folder
my_file_paths=paste("WP5",yadkin_counties_sel$COUNTY,".CSV",sep="")
my_col_names=c("var_name",paste("yr_",seq(1970,2000,5),sep=""),paste("yr_",seq(2001,2020,1),sep=""),paste("yr_",seq(2025,2040,5),sep=""))
yadkin_geo_data=data.frame()
for (i in 1:length(yadkin_counties_sel$NAME))
  {
    if (i==1) {
      my_table=read_csv(my_file_paths[i],skip=2,col_names=TRUE,
                      n_max=122,col_types=cols(.default = "c"))
      colnames(my_table)=my_col_names
      my_table_sel=as.data.frame(select(my_table,yr_2000:yr_2009))
      my_table_final=my_table_sel %>% 
        mutate(var_name=my_vars$var_name,
              NAME=rep(yadkin_counties_sel$NAME[i],length(my_vars)),
              COUNTY=rep(yadkin_counties_sel$COUNTY[i],length(my_vars))) %>%
        select(var_name,NAME,COUNTY,yr_2000:yr_2009)
      assign(yadkin_counties_sel$NAME[i],my_table_final)
      yadkin_geo_data=my_table_final
    }
    
    else {
      my_table=read_csv(my_file_paths[i],skip=2,col_names=TRUE,
                        n_max=122,col_types=cols(.default = "c"))
      colnames(my_table)=my_col_names
      my_table_sel=as.data.frame(select(my_table,yr_2000:yr_2009))
      my_table_final=my_table_sel %>% 
        mutate(var_name=my_vars$var_name,
               NAME=rep(yadkin_counties_sel$NAME[i],length(my_vars)),
               COUNTY=rep(yadkin_counties_sel$COUNTY[i],length(my_vars))) %>%
        select(var_name,NAME,COUNTY,yr_2000:yr_2009)
      assign(yadkin_counties_sel$NAME[i],my_table_final)
      yadkin_geo_data=bind_rows(yadkin_geo_data,eval(as.name(yadkin_counties_sel$NAME[i])))
    }
  }

# set working directory to save data to
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/reformatted_data")

# export data
write_csv(yadkin_geo_data,"yadkin_geo_data.csv")
