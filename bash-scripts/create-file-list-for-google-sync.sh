#!/bin/bash

[[ -z $file_list_fn ]] && { printf "\nVariable 'file_list_fn' has not been \
set, bailing out.\n"; exit 1; }

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
done

sed -i "s/$remove_path_regex//g" $file_list_fn
