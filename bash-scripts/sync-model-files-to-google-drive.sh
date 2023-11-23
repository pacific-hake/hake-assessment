#!/bin/bash

# Synchronize all model input files and RDS files to Google drive,
# preserving directory structure
#
# rclone config must have been run before this, with a new remote called
# `googledrive`. To see if it is set up properly, run the following:
# `rclone listremotes
# Which should return `googledrive:`
#
# `get-assess-year.sh` creates the variable $assess_year containing the
# current year unless it is currently December, in which case it will be
# the current year + 1. Enter a year as an argument here to force it to be
# that year, even if December.
. ./get-assess-year.sh
printf "Syncing files for model year $assess_year\n\n"

# Remote directory on Google drive to sync
gd="googledrive:/hake-data/models/$assess_year/"
# Local directory to sync with Google drive directory above
ld="/srv/hake/models/$assess_year/"

# `file_list_fn` is needed by create-file-list.sh as well as being used
# in this script
file_list_fn="file_list_for_google_sync.txt"
. ./create-file-list-for-google-sync.sh

rclone sync $ld $gd --files-from $file_list_fn

rm -f $file_list_fn
