#!/bin/bash

# Synchronize all model input files and RDS files to the following Google
#  drive:
# https://drive.google.com/drive/folders/19xcCDlYYE9Uj94Ns9S9aXZKPtPIz0O0O?tid=0B5lSVg5Eq86uQllqNmdmczFVU2c
# preserving directory structure
#
# rclone config must have been run one time before this, with a new remote
# called `googledrive`. To see if it is set up properly, run the following:
#
# `rclone listremotes`
#
# Which should return:
# `googledrive:`

# `file_list_fn` is needed by create-file-list.sh as well as being used
# in this script
file_list_fn="file_list_for_google_sync.txt"

. ~/github/pacific-hake/hake-assessment/bash-scripts/create-file-list-for-google-sync.sh

# Local directory to sync with Google drive directory above
ld="/srv/hake"

rclone sync --transfers=20 --progress --delete-excluded \
  --track-renames $ld "googledrive:/hake-data/srv/hake" \
  --files-from $file_list_fn

rm -f $file_list_fn
