#!/bin/bash

file_list_fn="file_list_for_google_sync.txt"
# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh


# Creates a list of files to synchronize. The list includes only the file
# matches below in the $files variable
gd="googledrive:/hake-data/"

# Make sure this covers all the models you want to be backed up
# ld = local directory
ld="/srv/hake/"
remove_path_regex="\\/srv\\/hake\\/"

files=(*.rds \
       starter.ss \
       wtatage.ss \
       forecast.ss \
       hake_control.ss \
       hake_data.ss)

rm -f $file_list_fn
touch $file_list_fn

for file in ${files[@]}; do
  fns=$(find "$ld" -type f -name "$file" | sort -n)
  echo "$fns" >> $file_list_fn
  echo "/srv/hake/models/$assess_year/01-version/01-base-models/01-base/mcmc/hake.Rdata" \
   >> $file_list_fn
done

sed -i "s/$remove_path_regex//g" $file_list_fn
