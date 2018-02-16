#!/bin/bash

#informs you if the local repo is up to date with the remote repo
$(git fetch origin)
localRepo=$(git rev-parse master)
remoteRepo=$(git ls-remote)
if [ $localRepo -eq $remoteRepo ]
then
    echo "Local Repo is up to date with Remote Repo"
else
    echo "Local Repo is not up to date with Remote Repo"
fi

#puts all uncommitted changes in changes.log
diffs=$(git diff)
$diffs > changes.log

#puts each line from every file with the tag TODO into todo.log
todo=$(grep -r "TODO")
$todo > todo.log

#checks all haskell files for syntax errors and puts the results into erro.log
