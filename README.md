![](https://constructionnotesmanager.com/cnm-demo-animated.gif)
# WELCOME TO CONSTRUCTION NOTES MANAGER
Welcome to the Construction Notes Manager suite of AutoCAD productivity tools started by Thomas Gail Haws in 1994 and released as Free Libre Open Source Software on July 26, 2020. These tools are AutoCAD commands ready for AutoCAD and Civil 3D users who have no knowledge of AutoLISP. Please contact me if you need any help using or modifying these tools.

# NEWS
With the help of the Claude Sonnet 4.5 AI model of Copilot AI agent in VS Code, this week I added 

- reactive auto text for quantities and Civil 3D information to CNM bubble notes
- a quick "pipe" labeler for utility research that reads text and style based on layer from a file you edit

Thanks to my AI assistant, I can now implement your ideas fast. Try me!

# INSTALLATION
- Windows installer: Choose, download and run a Windows installer from the [download page](https://constructionnotesmanager.com/download.htm).
- From github: Put all the files from devsource into a folder of your choice and follow the Installation Help instructions at the [download page](https://constructionnotesmanager.com/download.htm).

# INTRODUCTION
- Videos: Watch [CNM From The Beginning](https://youtu.be/49cvLI28sb8?t=268) or browse [this playlist](https://www.youtube.com/playlist?list=PLb4Ow6sAWuXMENceM_eZaqMjshpefO0uN).
- Command list: Open the very nice spreadsheet list [CNM-Command-Reference.ods](https://github.com/hawstom/cnm/blob/master/devsource/CNM-Command-Reference.ods) that has dozens of productivity enhancing commands and a column ranking the "coolness" of each command.

# HOW I DEVELOP AND TEST CNM
1. Run Git Bash Here in the Windows File Explorer context (right-click) menu under hawsedc\develop.
2. Check 'git status'
3. 'git commit -am"..."' as needed.
4. Open any drawing in hawsedc\develop\sandbox or anywhere.
5. Put hawsedc\develop\devsource at top of AutoCAD Support Files Search Path and in Trusted Locations
6. If developing menu, unload cnm.cuix and load from source folder.
7. In AutoCAD command VLISP IDE, open hawsedc\develop\devsource\cnm.prj
8. Develop and test in VLISP.
9. Final test using CNM-Demo.scr with a copy (outside source for safety) of CNM-Demo.*
10. Save, then compile FAS and see hawsedc\compile\README.TXT for compile and distribe instructions.