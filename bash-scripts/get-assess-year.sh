#!/bin/bash

# Creates the $assess_year variable for insertion into year_path variable
# for other run and create scripts. If an argument is supplied, `assess_yr`
# will be set to that, otherwise it will be the current year, unless the
# current month is December, then it will be the current year plus 1

if [ "$#" -eq 1 ]
then
  echo "Using year supplied as argument for assess_yr: $1"
  assess_year=$1
  mnth=1
else
  assess_year=$(date +'%Y')
  mnth=$(date +'%m')
fi

if [ $mnth -eq 12 ]
then
  assess_year=$(($assess_year+1))
fi


