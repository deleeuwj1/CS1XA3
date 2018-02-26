#!/bin/bash

#colour variables
GREEN='\033[0;32m'
WHITE='\033[1;37m'

#building a menu for user experience
echo -e "${GREEN}Here are your options:" > menu.txt
echo -e "${WHITE}1. same will tell you if your local and remote repos are up to date (if any commits have not been pushed)." >> menu.txt
echo -e "${WHITE}2. changes will tell you if what changes have been made since your last commit." >> menu.txt
echo -e "${WHITE}3. todo will show you all lines containing the tag #TODO." >> menu.txt
echo -e "${WHITE}4. haskellErrors will show you syntax errors in all Haskell files." >> menu.txt
echo -e "${WHITE}5. move will move a file from anywhere into your current directory." >> menu.txt
echo -e "${WHITE}6. diffs will show you the differences between your local and remote repos." >> menu.txt
echo -e "${WHITE}7. replace will replace a pattern in a file with a new chosen pattern." >> menu.txt
echo -e "${WHITE}8. newDir will create a directory containing a README.md file and push the directory to GitHub." >> menu.txt
echo -e "${WHITE}9. newFile will create a file and let the user add text to it without opening an editor." >> menu.txt

#inorms you if the local repo is up to date with the remote repo
function same () {
    $(git fetch origin)
    localRepo=$(git rev-parse master)
    remoteRepo=$(git rev-parse origin/master)
    if [ $localRepo == $remoteRepo ]
    then
        echo -e "${WHITE}Local Repo is up to date with remote repo"
    else
        echo -e "${WHITE}Local Repo is not up to date with remote repo"
    fi
    read -p "Would you like to see the differences in commits between repos? (Y/N) " ans
    if [ $ans == "Y" ] 
    then 
        echo "Here is your status: "
        git status master
    fi    
}

#shows the user any changes that have been made since the last commit
function changes () {
    git diff > changes.log #outputs results of git diff to changes.log
    read -p "Would you like to view the contents of changes.log? (Y/N) " ans #allows user to view changes.log if they wish to
    if [ $ans == "Y" ]
    then
        echo -e "${GREEN}Here is changes.log: "
        cat changes.log
    fi
}

#find every line containing the tag #TODO and outputs the results to todo.log
function todo () {
    grep -r -e "#TODO" --exclude="*.log" --exclude="ProjectAnalyze.sh" --exclude="menu.txt" > todo.log #excludes some files that won't contain actual TODO messages
    read -p "Would you like to view the contents of todo.log? (Y/N) " ans #allows the user to view todo.log if they wish to
    if [ $ans == "Y" ]
    then
        echo -e "${GREEN}Here is todo.log: "
        cat todo.log
    fi
}

#checks all haskell files for syntax errors and puts the results into error.log
function haskellErrors () {
    find -name "*.hs" | xargs -I {} ghc -fno-code {} &> error.log #checks haskell files for errors and outputs results into error.log
    read -p "Would you like to view the contents of error.log? (Y/N) " ans #allows the user to view error.log if they wish to
    if [ $ans == "Y" ]
    then 
        echo -e "${GREEN}Here is error.log: "
        cat error.log
    fi  
}

#finds a file that the user wished to see and moves it to the user's current directory
function move () {
    read -p "Enter the name of the file you wish to find: " fileName
    if [ $(find . -name "$fileName" -type f | wc -l) -gt 0 ] #looks through all sub-directories and moves the file to the current directory
    then
        fileLocation=$(find `pwd` -name $fileName) 
        cp -v $fileLocation .  #moves the file from it's location into the current directory
        echo "Your file in now in $PWD"
    else
        echo "$fileName does not exist"
    fi

}

#shows the user the differences between their local and remote repositories
function diffs () {
    read -p "Would you like to see the differences between your Local and Remote Repos? (Y/N) " ans
    if [ $ans == "Y" ]
    then
        d=$"git diff origin/master master" #shows differences between repos, even if those differences have not been committed
        $d &> diffs.log
        cat diffs.log
    fi
}

#takes a file and pattern and replaces the pattern with a new one
function replace () {
    read -p "File you wish to modify (must be in current directory): " file
    read -p "Word you wish to replace: "old
    read -p "Word you wish to replace with: "new
    sed "s/${old}/${new}/g" "$file" > "M.$file" #replaces old with new in a file of the user's choosing
}

#adds, commits and pushes a new directory to GitHub with a README.md file that contains the date the directory was created
function newDir () {
    read -p "Would you like to create a directory? (Y/N) " ans
    if [ $ans == "Y" ] 
    then 
        read -p "What would you like to name your directory? " name
        if [ ! -d $name ] #this only searches through the current directory to see if it exists
        then
            mkdir $name
    	    touch $name/README.md 
            echo "# This is README for $name" >> $name/README.md
            read -p "Would you like to add anything to the README file? (Y/N) " a
            while [ $a != "N" ]
            do
                read -p "Please enter text: " txt
                echo -e "$txt\n" >> $name/README.md #adds each entry onto it's own line
                read -p "Would you like to add anything more? (Y/N) " a
            done
    	    today=`date +%Y-%m-%d`
    	    echo "This file was created on $today" >> $name/README.md
   	    git add $name/README.md
    	    git add $name
    	    git commit "$name" -m "Created $name"
            if [[ "$(git push --porcelain)" == *"Done"*  ]] #determines if the push to GitHub was successful or not
            then
    	        echo -e "${GREEN}$name has now been pushed."
            else
                echo -e "${GREEN} Push failed."
            fi
        else
            echo -e "${GREEN}A directory of that name already exists."
        fi
    else
        echo -e "${GREEN}A directory will not be created."
    fi
}

#creates a file and lets the user add text to it all in one go!
function newFile () {
    read -p "Would you like to create a new file? (Y/N) " ans
    if [ $ans == "Y" ]
    then 
        read -p "What will the file be called? (Please include the extension) " name
        if ! [[ ./${name} ]] #this only looks through the current directory to see if the file name exists 
        then
            touch $name
            read -p "Would you like to add anything to $name? (Y/N) " a
            while [ $a != "N" ]
            do
                read -p "Please enter text: " txt
                echo -e "$txt\n" >> $name #adds each entry onto it's own line
                read -p "Would you like to add another line? (Y/N) " a
            done
            read -p "Would you like to view $name? (Y/N) " n
            if [ $n == "Y" ]
            then
                cat $name
            fi
        else
            echo -e "${GREEN}$name already exists."
        fi
    else
        echo -e "${GREEN}No file has been created."
    fi
}

cont="C"
while [ $cont != 'Q' ]
do
    cat menu.txt
    read -p "What would you like to do? (Enter a number) " ans
    if [ $ans == "1" ]
    then
        same
    elif [ $ans == "2" ]
    then
        changes
    elif [ $ans == "3" ] 
    then
        todo
    elif [ $ans == "4" ] 
    then
        haskellErrors
    elif [ $ans == "5" ]
    then
        move
    elif [ $ans == "6" ] 
    then
        diffs
    elif [ $ans == "7" ]
    then
        replace
    elif [ $ans == "8" ]
    then
        newDir
    elif [ $ans == "9" ]
    then
        newFile
    else
        echo "Invalid entry."
    fi
    echo -e "${WHITE}Would you like to quit (type Q) or continue (press any key)? "
    read cont
done
