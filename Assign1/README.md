# README for Assignment 1
Due on February 26th, 2018.
## Basic Requirements:

- same: informs the user if their local repo is up to date with their remote repo

- changes: puts any changes since the user's last commit in changes.log

- todo: puts any line in this project with the tag **#TODO** in todo.log

- haskellErrors: puts all syntax errors from haskell files in error.log

## Custom Functions:

   - diffs: when run, creates and displays a file **diffs.log** that shows the differences between the user's local and remote repositories.
   - move: modified version of **findFile** from [Jeffery Gibson's Project](https://github.com/gibsoj12/CS1XA3/blob/master/ProjectAnalyze.sh/). 
      - move finds the user's file and moves it to the current directory. This is useful when an existing file is needed in the current dictory, but is currently in          a subdirectory. This saves the user some time, as they don't need to figure out where the file is, or copy it manually.
      - `find` `` `pwd` `` `-name` command was found [here](https://stackoverflow.com/questions/246215/how-can-i-list-files-with-their-absolute-path-in-linux).
      - Information on copying from [here](https://askubuntu.com/questions/835657/copy-file-to-current-directory).
   - replace: as seen in **Replace.sh** in [Akram Elwazani's Project](https://github.com/elwazana/CS1XA3/blob/master/Assign1/Replace.sh).
      - This function is one that I found used when writing my script. The ability to replace a re-occuring pattern with another is very useful when trying to change         a large portion of code makes the process a lot less arduous.  
   - newDir : modified version of **Bonus Feature 2** from [Ali Kariapper's Project](https://github.com/Kariappa/CS1XA3/blob/master/Assign1/ProjectAnalyze.sh)
      - This function is one that is useful for people that want to create a new directory with a README.md file, and add it to GitHub, but don't want to do all of  
        the steps individually. With one command and a couple lines depicting what they want their directory to be called, and if they want to put anything in their          README file, the user is able to do all of this.
      - The function will also tell the user if their push to GitHub was successful. This was done with help from [here](https://stackoverflow.com/questions/40177013/check-response-of-git-push-from-shell-script).
## Other Additions:
   - menu: a simple menu was added for ease of use.
   - colours: colours were added for *slightly* improved readability of outputs.
     - Information on changing the colour of output of echo from [here](https://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux). 
   - Features included in functions:
      - Option for the user to see differences in commits between local and remote repos.
      - Option for user to view **changes.log**, **todo.log** or **error.log** after running the respective function.

**Information on formatting .md files was found [here](https://help.github.com/articles/basic-writing-and-formatting-syntax/#headings).**
