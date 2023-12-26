#!/bin/bash

# Synchronize all model input files and RDS files to Google drive,
# preserving directory structure for given year, which is passed as an
# argument. If no argument is supplied, the current year (as calculated by
# `get-assess-year.sh`) will be synced.
#
# rclone config must have been run before this, with a new remote called
# `googledrive`. To see if it is set up properly, run the following:
# `rclone listremotes`
# Which should return `googledrive:`
#
# `get-assess-year.sh` creates the variable $assess_year containing the
# current year unless it is currently December, in which case it will be
# the current year + 1. Enter a year as an argument to this script to force
# it to be that year, even if December.
if [ "$#" -eq 1 ]
then
  . ./get-assess-year.sh $1
else
  . ./get-assess-year.sh
fi

printf "Syncing files for model year $assess_year\n\n"

# Remote directory on Google drive to sync
# For some reason cannot use this variable in the rclone command below,
# must use direct string in the command or it will continue to sync to the
# last known sync drive
# gd="googledrive:/hake-data/mod/$assess_year/"

# Local directory to sync with Google drive directory above
ld="/srv/hake/models/$assess_year/"

# `file_list_fn` is needed by create-file-list.sh as well as being used
# in this script
file_list_fn="file_list_for_google_sync.txt"
. ./create-file-list-for-google-sync.sh

rclone sync $ld "googledrive:/hake-data/srv/hake/models/$assess_year/" \
  --files-from $file_list_fn

rm -f $file_list_fn
