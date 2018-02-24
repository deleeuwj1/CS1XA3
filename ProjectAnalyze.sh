#!/bin/bash

#building a menu for user experience
echo "Here are your options:" > menu.txt
echo "1. isUpToDate will tell you if your Local and Remote Repos are up to date." >> menu.txt
echo "2. uncommittedChanges will tell you if what changes have been made since your last commit." >> menu.txt
echo "3. findTODO will show you all lines containing the tag #TODO." >> menu.txt
echo "4. haskellErrors will show you syntax errors in all Haskell files." >> menu.txt
echo "5. reminderOfDiffs will show you the differences between your Local and Remote Repos." >> menu.txt
cat menu.txt

#inorms you if the local repo is up to date with the remote repo
function isUpToDate () {
    $(git fetch origin)
    localRepo=$(git rev-parse master)
    remoteRepo=$(git rev-parse origin/master)
    if [ $localRepo == $remoteRepo ]
    then
        echo "Local Repo is up to date with Remote Repo"
    else
        echo "Local Repo is not up to date with Remote Repo"
    fi    
}

function uncommittedChanges () {
    git diff > changes.log
    read -p "Would you like to view the contents of changes.log? (Y/N) " ans
    if [ $ans == "Y" ]
    then
        cat changes.log
    fi
}

function findTODO () {
    grep -r -e "#TODO" --exclude="todo.log" --exclude="ProjectAnalyze.sh" > todo.log 
    read -p "Would you like to view the contents of todo.log? (Y/N) " ans
    if [ $ans == "Y" ]
    then
        cat todo.log
    fi
}

#checks all haskell files for syntax errors and puts the results into error.log
function haskellErrors () {
    find -name "*.hs" | xargs -I {} ghc -fno-code {} &> error.log
    read -p "Would you like to view the contents of error.log? (Y/N) " ans
    if [ $ans == "Y" ]
    then 
        cat error.log
    fi  
}

function findAndMove () {
    read -p "Enter the name of the file you wish to find: " fileName
    if [ $(find . -name "$fileName" -type f | wc -l) -gt 0 ]
    then
        fileLocation=$(find `pwd` -name $fileName)
        cp -v $fileLocation .
        echo "Your file in now in $PWD"
    else
        echo "$fileName does not exist"
    fi

}

function reminderOfDiffs () {
    read -p "Would you like to see the differences between your Local and Remote Repos? (Y/N) " ans
    if [ $ans == "Y" ]
    then
        d=$"git diff origin/master master"
        $d &> diffs.log
        cat diffs.log
    fi
}

read -p "What would you like to do? " ans
if [ $ans == "isUpToDate" ]
then
    isUpToDate
elif [ $ans == "uncommittedChanges" ]
then
    uncommittedChanges
elif [ $ans == "findTODO" ] 
then
    findTODO
elif [ $ans == "haskellErrors" ] 
then
    haskellErrors
elif [ $ans == "findAndMove" ]
then
    findAndMove
elif [ $ans == "reminderOfDiffs" ] 
then
    reminderOfDiffs
else:
    echo "$ans is an invalid input"
fi
