#!/bin/bash

# Miscellaneaous commands needed from time to time. Not intended to be run
# as one big script, but to be copied and pasted from to terminal.

# Verify that the CRON scheduling job ran to sync /srv/hake with backups
grep "CRON.*grandin" /var/log/syslog

# Edit CRON jobs
crontab -e
# If above command says "no crontab available for grandin"
sudo crontab -u grandin -e
# View cron jobs completed - check to see last time sync occurred
grep "CRON.*bash" /var/log/syslog

# View CPU core temperatures (if 'crit' temp is reached, CPU will be switched
# off and machine will lock up due to failsafe)
sensors | grep "ac"
# View fan speeds
sensors | grep "fan"

# Graphical display of all cores temperatures. Runs in background after you
# close the window so there is a graph that builds up over time once you
# open it again
psensor

# Turn fan speed up for fan #2
fan hi
# Retun fan speed to original
fan lo

# Change ownership recursively, starting in current directory
sudo chown -R grandin:hake .

# Find all RDS files in a folder recursibely and sort by number. Run in
# briding model directory of sensitiviy directory to see which models
# have RDS files built for them
find -name "*.rds" | sort
