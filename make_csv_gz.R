library(tidyverse)
library(read.cwa)
library(boxr)
box_auth()

## this function takes 1 box file id
## temporarily downloads the file (sorry, cant get around it yet... working on it...)
## converts it to gzipped csv (and saves an R object that can be read directly using box_load)
## and cleans up. (actually, the cleaning up currenly fails, and I had to clean it up)
convert_cwa_to_csv <- function (file_id){

  message(" === reading data from box ")  
  tmpfile=box_dl(file_id, local_dir = tempdir() )
  fname = sub("^.*/","",tmpfile)
  outfile=paste0(fname,".csv.gz")
  desc = paste0("CSV file for ",  fname)

  message(" === working on ",tmpfile,"\n\t\t===> ",outfile)
  dta <- read_cwa(tmpfile,verbose = TRUE)
  
  
  tmpout = paste0(tempdir(),"/",outfile)
  message("\t=== saving as gzipped file...")
  write_csv(dta$data,gzfile(tmpout) )
  message("\t=== writing file to box ")
  box_ul(file = tmpout,description = desc)  
  box_save(dta,file_name = paste0(fname,".RData"))
  
  message("\t=== removing tmp files")
  unlink(tmpfile)
  unlink(tmpout)
  invisible(0)
}

# this is the directory that contains my accelerometer data
#  you will need set this yourself....
box_setwd(127327591772)

# get a listing of all the files (should only look at .cwa* files)
# only keep files that are .cwa, but doesn't have a .cwa.csv.gz file or .cwa.RData file
as_tibble(box_ls()) %>% select(name,id)  %>% filter(grepl("\\.cwa.*",name)) %>% mutate(run=gsub("\\.cwa.*","",name)) %>% group_by(run) %>% 
  mutate(keep=n()==1) %>% filter(keep) %>% pull(id) %>% map_chr(convert_cwa_to_csv)
