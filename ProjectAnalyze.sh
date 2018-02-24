#!/bin/bash

#informs you if the local repo is up to date with the remote repo
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
    read -p "Would you like to view the contents of changes.log? (Y/N)" ans
    if [ $ans == "Y" ]
    then
        cat changes.log
    fi
}

function findTODO () {
    grep -r -e "#TODO" --exclude="todo.log" --exclude="ProjectAnalyze.sh" > todo.log 
    read -p "Would you like to view the contents of todo.log? (Y/N)" ans
    if [ $ans == "Y" ]
    then
        cat todo.log
    fi
}

#checks all haskell files for syntax errors and puts the results into error.log
function haskellErrors () {
    find -name "*.hs" | xargs -I {} ghc -fno-code {} &> error.log
    read -p "Would you like to view the contents of error.log? (Y/N)" ans
    if [ $ans == "Y" ]
    then 
        cat error.log
    fi  
}

function findAndGo () {
    read -p "Enter the name of the file you wish to find: " fileName
    if [ $(find . -name "$fileName" -type f | wc -l) -gt 0 ]
    then
        #fileLocation=(readlink -f ${fileName})
        $(cd (readlink -f ${fileName}))
    else
        echo "$fileName does not exist"
    fi

}

function reminderOfDiffs () {
    read -p "Would you like to see the differences between your Local and Remote Repos? (Y/N)" ans
    if [ $ans == "Y" ]
    then
        d=$"git diff origin/master master"
        $d &> diffs.log
        cat diffs.log
    fi
}
"$@"
