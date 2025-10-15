# HOW TO DEVELOP AND TEST CNM
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