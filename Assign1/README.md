# README for Assignment 1

## Basic Requirements:

- isUpToDate: informs user if the local repo is up to date with the remote repo

- uncommittedChanges: puts any changes since last commit in changes.log

- findTODO: puts any line in this project with tag **#TODO** in todo.log

- haskellErrors: puts all syntax errors from haskell files in to error.log

## Custom Functions:

- reminderOfDiffs: when run, creates and displays a file **diffs.log** that shows the differences between the user's local and remote repositories.

- findAndMove: modified version of **findFile** from [Jeff Gibson's Project](https://github.com/gibsoj12/CS1XA3/blob/master/ProjectAnalyze.sh/).
   - findAndMove finds the user's file and moves it to the current directory. This is useful when an existing file is needed in the current dictory, but is currently      in a subdirectroy.
