#!/bin/bash

#Informs you if the local repo is up to date with the remote repo
#rev-parse returns the ID of hte branches
$(git fetch origin)
localRepo=(git rev-parse master)
remoteRepo=()
if [ $upToDate ]

#puts all uncommited changes in changes.log


#puts each line from every file with the tag TODO into todo.log


#checks all Haskell files for syntax errors and puts the results into error.log
