# Install Development Environment

1. Register at github.
https://github.com/

2. Install Git for Windows.
https://git-scm.com/install/windows

3. Install VS Code. 
At the AutoCAD command prompt:

a. Type LISPSYS 1 to be sure you are in VS Code mode. If it was 0, you must restart AutoCAD.
b. Type ```VLISP``` (Visual LISP) or ```VLIDE``` (Visual Lisp Integrated Design Environment); these are equivalent. This command is your gateway to our project.
c. Follow the instructions until you are looking at the VS Code editor with the AutoLISP Extension installed.

# Clone Git
1. To your github account.

2. To your local (PC) environment.

# Submit a Git Pull Request
1. Create a branch for your work.
```
git status # This is your git "Where/who/what am I?" command. When all else fails and your mind is mush, type this command.
git checkout -b test-[your initials]-1 # Checkout a new branch.
git branch # List all branches
git status # See where we are now. What branch are we on?
```
2. Create, add, and commit a new file.

a. Create a file called devtools\[your initials]-test.md
b. Enter some text.
c. Commit your work.
```
git status # Where are we? Note the untracked file. Note the current branch.
git add . # Add all untracked files.
git branch # List all branches
git status # See where we are now. Are there untracked files? Are there uncommitted changes?
git commit -am"[Your initials] added a test file." # "Stage" and commit all uncommitted files with this message.
git log # Enter the git log viewer. Hit h for help and q to quit.
git status # Is there anything uncommitted? What branch are we on?
```

3. Check out master and observe.

a. Change branches. Check out master.
```
git checkout master # Edits all the files on your PC to be in the "master" state.
git status # What branch are we on?
```
b. Observe that devtools\[your initials]-test.md has disappeared from your PC.
c. Change back to your development branch.
```
git branch # List all branches
git checkout test-[your initials]-1 # Checkout a new branch. Note that the tab key is auto-complete.
git status 
```
d. Observe that devtools\[your initials]-test.md has returned to your PC.

4. Push your branch to git hub.

```
git push # This is short for git push origin test-[your initials]-1
```
5. Submit a pull request to hawstom.

a. I have never done this. Fingers crossed.

# Git cheat sheet
Search logs
```
git log --grep='underscore'
```