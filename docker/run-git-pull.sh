#!/bin/bash

# Pull a repository. Used for a starting script for Docker container
# so that you are starting with the most current commit in the repo
# each time you start the container

git stash # There seem to be changes somehow. Ignore them.
git stash clear

git pull
