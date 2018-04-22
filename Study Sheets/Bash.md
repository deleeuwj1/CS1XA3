# Study Sheet on Bash and Git 

- Commands
   - `ssh` (Secure SHell) is used for remote login
   - `scp` (Secure Copy) is used for remote transfer of files between machines and servers
   - `man` (Manual) command is used to remember how to use a command

- File Browsing
   - `cd` (Change Directory)
   - `ls` (List) lists the current directory's contents
     - Hidden files begin with `.` and can't be seen using `ls`
        - `ls -la` (List All) is used instead
   - `pwd` (Parent Working Directory) shows where you currently are

- File Manipulation
   - `cp` (Copy) copies a file from one location to another
     - `cp -r` copies a directory and all of its contents 
   - `mv` (Move) moves a file from one location to another
   - `mkdir` (Make Directory) creates a new directory
   - `rm` (Remove) removes a file
     - `rm -r` removes a directory and all of its contents  
   - `sed` deals with patterns in files
      - `sed /pattern/a line to insert` inserts a new line after a pattern
      - `sed /pattern/i line to insert` inserts a new line before a pattern
      - `sed /pattern/c line to insert` replaces a line with a pattern
      - `sed s/old/new` substitutes one word for another 
      - `sed s/old//g` deletes a word
      - `sed /pattern/d` deletes any line with the specified pattern

- File Paths
   - `\` is the **root** directory
   - `~` is the **home** directory 
   - `.` specifies the current directory
   - `..` specifies the previous directory
   - `cd /path/to` changes the directory to `/path/to`
   - `cd path/to` changes the directory to `PWD/path/to`

- Finding Files
   - `find dir -name pattern -type f` limits to file or directory with the specified type
   - `find dir -name pattern -exec cm {} \;` executes a command without prompt
   - `find fir -name pattern -ok cm {} \;` executes a command with prompt
   - `find dir -name pattern -print` prints files starting at current directory
      - `{}` specifies the input
      - `\;` specifies to execute each command separately
      - Adding a `+` at the end specifies to add all arguaments at once
   - Because of spaces, `find -exec` commands often go wrong
      - `-print0` is used to output the result of a find separated by a special character 
      - They can then be parsed by `xargs` with the `-0` flag

- Vim
   - Press `a` to change into **Insert Mode** and press `Esc` to change into **Command Mode**
   - `:q` to quit; add an exclamation mark (`:q!`) to quit without saving
   - `:w` (Write) saves
   - `dd` deletes the line that the user is currently on
   - `u` (Undo)
   - `Ctrl-r` for redo 

- Bash Profile
   - A hidden script `.bash_profile` that loads every time a terminal session is started
   - This can be used to created custom commands
     - Ex. `alias ll="ls -la" lets you use `ll` instead of `ls -la`

- Environment Variable
   - Environment variables are values that can affect the way running processes will behave on a computer
   - The **PATH** variable is the most important 
   - Everything command/program accessible is either in the current working directory 
      - To see what's in the path, use `exho $PATH`
      - To see where a command is located, use `which` command

- Glob Patterns
   - `*` matches any quantity of character
   - `?` matches the occurence of a single character
   - `\` escapes a special character
   - `[...]` matches one occurence of a character within brackets  

- Syntax of Commands
   - Spaces are used to separate arguments in commands
   - Quotations aren't used to specify strings, and are instead used to stop spaces in names from creating separate arguments
      - It's a good idea to use quotations marks when using glob patterns

- IO Redirection
   - `>` is used to overwrite or create a file from **stdout**
   - `>>` appends to or creates a file from **stdout**
   - `2>` or `2>>` write or append to a file from **stderr**
   - `&>` or `&>>` write or append from both **stdout** and **stderr**

- Piping and Chaining Commands
   - `|` is used to pipe output from one command into another
      - Ex. `ls -la | grep bash`
      - Ex. `ls -la | grep bash | grep profile`
   - `xargs` is also used to do piping 
      - It splits the input into manageable chunks  
   - Commands can be chained by using a semicolon
      - Ex. `comm1 ; comm2`
      - For it only to execute if `comm1` doesn't produce and error
          - `comm1 && comm2`
      - For it to execute only if `comm1` produces and error
          - `comm1 || comm2`

- Important Flags and Such
   - `-r` is used make a command recursive
   - `-R` is used to search only **files** in the specified directory
   - `-v` is used to reverse grep, and shows lines excluding the specified pattern
   - `-i` is used to ignore cases when matching a pattern
   - `-I` is used to list only te filenames
   - Word Count flags
      - `wc -w` is used to count words
      - `wc -l` is used to count lines
      - `wc -m` is used to count characters

- Bash Scripts
   - Have the extension `.sh`
   - `#/bin/bash` is put at the top of the file
   - Executed using the `sh` command
   - The file is made executable with `chmod a+x`
       - It can then be used by typing `./` and then the file name
   - Variables are used by putting `$` in front of them
       - They should be declared with no space between the `=` operator
           - Ex. `var="This is bash"`
           - Ex. `mv "$var"`
       - The number of variables is obtained by using `$#`, and `$@` will let you access all of the arguments at once
       - Variables can also be executed using `${}`
           - Ex. echo `${Hello}Goodbye`
   - String equalities: =, !=
   - Integer equalities: `-eq`, `-gt`, `-lt` 
   - Double brackets have to be used for arithmetic expansions
       - Ex. `for ((i=1;i<=5;i+=1))`
       - Operators such as `%`, `+` and `-` etc. are only available in the double parenthesis
   - Putting `shopt -s nullglob` before for loops handles situations where it is not possible to iterate through
       - Iterations of a loop can be skipped with the `continue` command
       - They can be exited using the `break` command
   - For if statements, the conditions should be put in square brackets (note that the spaces are important
        - Ex. `if [ $i -lt 5 ]`
   - Commands executed in a subshell are done in a different environment than the original one, therefore they **do not** change the current environment
   - The `read` command can be used to take in user input
   - We can iterate through items in a directory, and not in it's subdirectories
        - Ex. `for file in *.tmp`
   - We can also iterate through all the subdirectories as well
        - Ex. `find . -name "*.tmp" -print0 |
                   while IFS='' read -r -d $'\0' file`
        - `IFS` stops leading or trailing spaces from being trimmed
        - `read -d` specifies the argument separator
        - `'\0'` (NULL character) coresponds to the `-print0` 

- Miscellaneous Commands 
   - `du -h` gives the sive of a directory or file in a human readable format
   - `wget url` downloads the file at the given URL
   - `whoami` outputs your username
   - `which inp` shows the full path of `inp`
   - `sort file` sorts a file

## Git
Terminology: 
- **Working Directory**: the local directory you downloaded your repo to
- **Index** and **Local Repository**: files providing version control on your system
- **Remote Repository**: the GitHub server where your repo is kept

Connection Between Repositories
![alt text][link]

[link]: https://greenido.files.wordpress.com/2013/07/git-local-remote.png?w=696&h=570 "Repo diagram"

Commands:
- `git clone` downloads the code from a Remote Repo into the current directory
- `git pull` merges code from the remote repo to the current directory
- `git add` and `git rm` add or remove files/directories
- `git reset` undoes local changes (opposite of `git add`)
   - `git reset -hard` changes everything back to the last commit
      - the last commit is known as **HEAD**
   - It can also be specified which commit to revert back to
- `git commit -m` commits changes to the local repo with a message describing the changes
- `git push` pushes changes in the local repo to the remote repo
- `git status` shows the current status of staged files
- `git log` displays the log of commits
- `git -help` lists git commands with description

Created by Jessica de Leeuw 
