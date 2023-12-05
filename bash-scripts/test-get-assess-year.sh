#!/bin/bash

echo "Output years below should be 1900, 2024, 2023, 2024, 2024"

# Note the dot and space that preceed the ./get-assess-year.sh call. This is
# required to export the $assess_year variable from the script
. ./get-assess-year.sh 1900
echo $assess_year

. ./get-assess-year.sh
echo $assess_year

. ./get-assess-year.sh 2023
echo $assess_year

. ./get-assess-year.sh 2024
echo $assess_year

. ./get-assess-year.sh
echo $assess_year
