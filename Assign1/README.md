# README for Assignment 1

## Basic Requirements:

- same: informs user if the local repo is up to date with the remote repo

- changes: puts any changes since last commit in changes.log

- todo: puts any line in this project with tag **#TODO** in todo.log

- haskellErrors: puts all syntax errors from haskell files in to error.log

## Custom Functions:

   - diffs: when run, creates and displays a file **diffs.log** that shows the differences between the user's local and remote repositories.

   - move: modified version of **findFile** from [Jeff Gibson's Project](https://github.com/gibsoj12/CS1XA3/blob/master/ProjectAnalyze.sh/). 
    - move finds the user's file and moves it to the current directory. This is useful when an existing file is needed in the current dictory, but is currently in a
     subdirectory.
    - "find `pwd` -name" command was found [here](https://stackoverflow.com/questions/246215/how-can-i-list-files-with-their-absolute-path-in-linux)
    - information on copying from [here](https://askubuntu.com/questions/835657/copy-file-to-current-directory)

## Other Additions:
   - menu: a simple menu was added for ease of use.
   - colours: colours were added for *slightly* improved readability of outputs.
   - features included in functionsL
    - option for the user to see differences in commits between local and remote repos
    - option for user to view **changes.log**, **todo.log**, **error.log** or **diffs.log** after running the respective function

