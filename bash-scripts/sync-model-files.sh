#!/bin/bash

# Synchronize all model input files and RDS files to the following Google
#  drive and to the /srv2 drive on the server (as a local backup):
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
#
# The backup of /srv/hake to /srv2/hake will be a 100% backup meaning ALL
# files including intermediary SS3 files will be backed up there
. ~/github/pacific-hake/hake-assessment/bash-scripts/create-file-list-for-google-sync.sh

# Local directory to sync with Google drive directory above
ld="/srv/hake"

rclone sync \
  --transfers=20 \
  --progress \
  --delete-excluded \
  --track-renames \
  --files-from $file_list_fn \
  $ld \
  "googledrive:/hake-data/srv/hake"

rm -f $file_list_fn

# Sync the /srv2 backup, note no --files-from argument so ALL files
# are backed up:
rclone sync \
  --transfers=20 \
  --progress \
  --delete-excluded \
  --track-renames \
  /srv/hake \
  /srv2/hake

