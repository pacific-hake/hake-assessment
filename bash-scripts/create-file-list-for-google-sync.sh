#!/bin/bash

file_list_fn="file_list_for_google_sync.txt"
# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

# Version used for the final base model. Usually 01-version but some years
# may be 02-version
version="01-version"

base_dir="/srv/hake/models/$assess_year/$version/01-base-models/01-base"
base_mcmc_dir="$base_dir/mcmc"
base_sso_dir="$base_mcmc_dir/sso"

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
  echo "$base_mcmc_dir/hake.Rdata" >> $file_list_fn
  echo "$base_sso_dir/CompReport.sso" >> $file_list_fn
  echo "$base_sso_dir/derived_posteriors.sso" >> $file_list_fn
  echo "$base_sso_dir/posteriors.sso" >> $file_list_fn
  echo "$base_sso_dir/posterior_vectors.sso" >> $file_list_fn
  echo "$base_sso_dir/Report.sso" >> $file_list_fn
  echo "$base_sso_dir/Report.sso" >> $file_list_fn
done

sed -i "s/$remove_path_regex//g" $file_list_fn
