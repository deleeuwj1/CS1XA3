# Study Sheet for CS 1XA3 Final

## Bash
Commands:
- `ssh` (Secure SHell) is used for remote login
- `scp` (Secure Copy) is used for remote transfer of files between machines and servers
- `man` (Manual) command is used to remember how to use a command
- File Browsing
   - `cd` (Change Directory)
   - `ls` (List) lists the current directory's contents
   - `pwd` (Parent Working Directory) shows where you currently are
- File Manipulation
   - `cp` (Copy) copies a file from one location to another
     - `cp -r` copies a directory and all of its contents 
   - `mv` (Move) moves a file from one location to another
   - `mkdir` (Make Directory) creates a new directory
   - `rm` (Remove) removes a file
     - `rm -r` removes a directory and all of its contents
- File Paths
   - `\` is the **root** directory
   - `~` is the **home** directory 
   - `.` specifies the current directory
   - `..` specifies the previous directory
   - `cd /path/to` changes the directory to `/path/to`
   - `cd path/to` changes the directory to `PWD/path/to`
- Vim
   - Press `a` to change into **Insert Mode**
   - Press `Esc` to change into **Command Mode**
   - `:q` to quit, add an exclamation mark (`:q!`) to quit without saving
   - `:w` (Write) saves
   - `dd` deletes the line that the user is currently on
   - `u` (Undo)
   - `Ctrl-r` for redo 

## Git
Terminology: 
- **Working Directory**: the local directory you downloaded your repo to
- **Index** and **Local Repository**: files providing version control on your system
- **Remote Repository**: the GitHub server where your repo is kept

![Connection between repositories][link]
[link]: https://greenido.files.wordpress.com/2013/07/git-local-remote.png?w=696&h=570 

Commands:
- `git clone` downloads the code from a Remote Repo into the current directory
- `git pull` merges code from the remote repo to the current directory
- `git add` and `git rm` add or remove files/directories
- `git reset` undoes local changes (opposite of `git add`)
   - `git reset -hard` changes everything back to the last commit
      - the last commit is known as **HEAD**
   - It can also be specified which commit to revert back to
- `git commit -m` commits changes to the local repo with a message describing thechanges
- `git push` pushes changes in the local repo to the remote repo
- `git status` shows the current status of staged files
- `git log` displays the log of commits
- `git -help` lists git commands with descriptions


## ELM


## Haskell


Created by Jessica de Leeuw 
