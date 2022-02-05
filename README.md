# zip-diff
zip-diff with tree viewer. It works also for all ZIP-like files (XLSX, DOCX, etc.)

Features:
- It shows the differences between 2 ZIP-like files. It's based on Column Tree control. `z_zip_diff_demo2` shows the old ZIP-like file (white page) compared to the new ZIP-like file (envelope), the old file contains "deleted.txt", "changed.txt" and "unchanged.txt", the new file contains "added.txt", "changed.txt" and "unchanged.txt", the contents of "changed.txt" is different between the two ZIP-like files, but not the one of "unchanged.txt":
  
  ![image](https://user-images.githubusercontent.com/34005250/152645850-4a11f027-4e91-4f18-ab29-039b1563caea.png)
  - unchanged/No color: file exists in both ZIP-like files and its content is identical
  - deleted/Red color: file exists in the old ZIP-like file only
  - changed/Yellow color: file exists in both ZIP-like files but its content is different
  - added/Green color: file exists in the new ZIP-like file only
- To see the differences in a changed file, it's possible to connect zip-diff to Microsoft Visual Studio Code. Then, by clicking the file name, the 2 versions of the file are compared and the differences are displayed by VS Code.
- Demo program allows to work with new or existing ZIP-like files; here with Excel, to see what has changed after typing "TEXT" in cell A1, and VSCode used as file comparator:
  
  ![image](https://user-images.githubusercontent.com/34005250/152639842-578ddf70-3099-4985-8fbe-88013796ccf4.png)
  ![image](https://user-images.githubusercontent.com/34005250/152642674-5f464d07-4dd0-477e-9832-35207833574d.png)
  ![image](https://user-images.githubusercontent.com/34005250/152642806-0146367f-c702-4e74-ae1d-0f1e48ef3014.png)
