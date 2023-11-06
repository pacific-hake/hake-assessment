#!/bin/bash

# Creates the $assess_year variable for insertion into year_path variable
# for other run and create scripts. If an argument is supplied, that will be
# used as the current year and `assess_year` will be set to it

if [ "$#" -eq 1 ]
then
  echo "Supplied argument! $1"
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
