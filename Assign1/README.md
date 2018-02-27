# README for Assignment 1
Due on February 26th, 2018.

## Usage Information
   - To run the script, enter `./ProjectAnalyze.sh`. A menu will appear with prompts for each function.
      - To quit the program, type in **Q** when prompted with: "Would you like to quit or continue?"
   - This file must be kept at the root of your git repository. 

## Basic Requirements:

   - same: informs the user if their local repo is up to date with their remote repo.
      - **Used by typing in *1* after menu appears.**
   - changes: puts any changes since the user's last commit in changes.log.
      - **Used by typing in *2* when menu appears.**
   - todo: puts any line in this project with the tag **#TODO** in todo.log.
      - **Used by typing in *3* when menu appears.**
   - haskellErrors: puts all syntax errors from haskell files in error.log.
      - **Used by typing in *4* when menu appears.**

## Custom Functions:

   - diffs: when run, creates and displays a file **diffs.log** that shows the differences between the user's local and remote repositories.
      - **Used by typing in *5* when menu appears.**
   - move: modified version of **findFile** from [Jeffrey Gibson's Project](https://github.com/gibsoj12/CS1XA3/blob/master/ProjectAnalyze.sh/). 
      - **Used by typing in *6* when menu appears.**
      - move finds the user's file and moves it to the current directory. This is useful when an existing file is needed in the current dictory, but is currently in          a subdirectory. This saves the user some time, as they don't need to figure out where the file is, or copy it manually.
      - `find` `` `pwd` `` `-name` command was found [here](https://stackoverflow.com/questions/246215/how-can-i-list-files-with-their-absolute-path-in-linux).
      - Information on copying from [here](https://askubuntu.com/questions/835657/copy-file-to-current-directory).
   - replace: as seen in **Replace.sh** in [Akram Elwazani's Project](https://github.com/elwazana/CS1XA3/blob/master/Assign1/Replace.sh).
      - **Used by typing in *7* when menu appears.**
      - This function is one that I found used when writing my script. The ability to replace a re-occuring pattern with another makes trying to change a large               portion of code a lot less arduous.  
   - newDir: modified version of **Bonus Feature 2** from [Ali Kariapper's Project](https://github.com/Kariappa/CS1XA3/blob/master/Assign1/ProjectAnalyze.sh)
      - **Used by typing in *8* when menu appears.**
      - This function is one that is useful for people that want to create a new directory with a README.md file, and add it to GitHub, but don't want to do each of          the steps individually. With one command and a couple lines depicting what they want their directory to be called (and if they want to put anything in their          README file) the user is able to do the aforementioned.
      - The function will also tell the user if their push to GitHub was successful. This was done with help from [here](https://stackoverflow.com/questions/40177013/check-response-of-git-push-from-shell-script). 
      - `! -d $name`command, to check if the directory already existed found [here](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script).
   - newFile: when run, creates a new file and allows the user to input lines without using an editor.
      - **Used by typing in *9* when menu appears.**
      - This function is useful for users that want to quickly create simple files. 
      - This could be used when looking to create files for testing a function, creating simple README files, or writing a to-do list. This is also useful for people         who do not have a good text editor installed (such as Emacs).
      - `[[ ./file ]]`command, to check if the file already existed found [here](https://unix.stackexchange.com/questions/280614/how-to-use-file-test-to-check-if-a-file-already-exists-in-a-directory).

## Other Additions:

   - menu: a simple menu was added for ease of use.
   - colours: colours were added for *slightly* improved readability of outputs.
     - Information on changing the colour of output of echo from [here](https://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux). 
   - Features included in functions:
      - Option for the user to see differences in commits between local and remote repos added to **same** function.
      - Option for user to view **changes.log**, **todo.log** or **error.log** after running the respective function.

**Information on formatting .md files was found [here](https://help.github.com/articles/basic-writing-and-formatting-syntax/#headings).**
