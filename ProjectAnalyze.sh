#!/bin/bash

#informs you if the local repo is up to date with the remote repo
$(git fetch origin)
localRepo=(git rev-parse master)
remoteRepo=(git ls-remote)
if [ $localRepo == $remoteRepo ]

then
    echo "Local Repo is up to date with Remote Repo"
else
    echo "Local Repo is not up to date with Remote Repo"
fi

#puts all uncommitted changes in changes.log
git diff $1 > changes.log

#puts each line from every file with the tag TODO into todo.log
todo=(grep -r "#TODO" --exclude todo.log)
$todo > todo.log 

#checks all haskell files for syntax errors and puts the results into error.log
file=$1
for file in "*.hs"
do
    ghc -fno-code $file > error.log
done

#moves up n number of dirtories
movingUpDirectories () {
    echo "Would you like to move up directories? (Y/N)"
    read ans
    if [ $ans = "Y" ]
    then 
        echo "How many directories would you like to move up?"
	read num
        i=0
        d=$PWD
        while [ i -lt num ]
        do
	    d=$d/..
            i=i+1
        done
    fi
}
