#!/bin/bash

# Miscellaneaous commands needed from time to time. Not intended to be run
# as one big script, but to be copied and pasted from to terminal.

# Verify that the CRON scheduling job ran to sync /srv/hake with backups
grep "CRON.*grandin" /var/log/syslog

# Edit CRON jobs
crontab -e

# View CPU core temperatures (if 'crit' temp is reached, CPU will be switched
# off and machine will lock up due to failsafe)
sensors | grep "ac"
# View fan speeds
sensors | grep "fan"

# Turn fan speed up for fan #2
fan hi
# Retun fan speed to original
fan lo

# Change ownership recursively, starting in current directory
sudo chown -R grandin:hake .

