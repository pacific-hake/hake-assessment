#!/bin/bash

# Copy all model input files and RDS files to Google drive, preserving
# directory structure, using parallelism

# Need to make sure this directory exists first, by running:
# ll /run/user/1000/gvfs
# If the Google Drive directory is not listed, open Nautilus and click on the
# Gmail account link on the left to establish the mount point, then run this
# script once the mount point exists. It will disappear after non-use,
# sometimes very quickly
#gd="/run/user/1000/gvfs/google-drive:host=gmail.com,user=chrisgrandin/0AD8L8r-HoOiUUk9PVA/hake-temp/"
gd="/home/grandin/googledrive/hake-temp/"
# Make sure this covers all the models you want to be backed up
ld="/srv/hake/models/2023/"

files=(*.rds \
       starter.ss
       wtatage.ss \
       forecast.ss \
       hake_control.ss \
       hake_data.ss)

for file in ${files[@]}; do
  (trap 'kill 0' SIGINT; \
   echo "Copying $file files..."; \
   fns=$(find "$ld" -type f -name "$file" | sort -n); \
   # Filter out some of the files, we only want the ones from the outer model
   # directories
   fns=$(sed -E 's/.*(forecasts|retrospectives|mcmc|catch-levels).*//g' <<<$fns); \
   #echo $fns; \
   cp --parents -r $fns $gd 2>/dev/null; \
   echo; \
   echo "All $file files copied"; \
   echo) &
done
