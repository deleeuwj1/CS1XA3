#!/bin/bash

#building a menu for user experience
echo "Here are your options:" > menu.txt
echo "1. same will tell you if your local and remote repos are up to date (if any commits have not been pushed)." >> menu.txt
echo "2. changes will tell you if what changes have been made since your last commit." >> menu.txt
echo "3. todo will show you all lines containing the tag #TODO." >> menu.txt
echo "4. haskellErrors will show you syntax errors in all Haskell files." >> menu.txt
echo "5. move will move a file from anywhere into your current directory." >> menu.txt
echo "6. diffs will show you the differences between your Local and Remote Repos." >> menu.txt
cat menu.txt

#inorms you if the local repo is up to date with the remote repo
function same () {
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

function changes () {
    git diff > changes.log
    read -p "Would you like to view the contents of changes.log? (Y/N) " ans
    if [ $ans == "Y" ]
    then
        cat changes.log
    fi
}

function todo () {
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

function move () {
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

function diffs () {
    read -p "Would you like to see the differences between your Local and Remote Repos? (Y/N) " ans
    if [ $ans == "Y" ]
    then
        d=$"git diff origin/master master"
        $d &> diffs.log
        cat diffs.log
    fi
}

read -p "What would you like to do? " ans
if [ $ans == "same" ]
then
    same
elif [ $ans == "changes" ]
then
    changes
elif [ $ans == "todo" ] 
then
    todo
elif [ $ans == "haskellErrors" ] 
then
    haskellErrors
elif [ $ans == "move" ]
then
    move
elif [ $ans == "diffs" ] 
then
    diffs
else:
    echo "$ans is an invalid input"
fi
