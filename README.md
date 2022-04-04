#  What the project does
To recap information is a powerful teaching tool. 
One way to implement recap is to ask students to invent question and answer sets and to turn them into a quiz. 
Question and answer sets may for example be a question with two true and two wrong answers, like "Monkey are ...? 1 mammals, 2 related to humans, 3 insects, 4 birds". 
In the recap context, question and answer sets may be collected at the end of a lecture, and the quiz may be taken at the beginning of the next lecture. 
After the quiz, the quiz may be evaluated immediately.

#  Why the project is useful
This project aims to facilitate the recap process described above by a re-combination of available shiny based tools (shiny, shinyjs, shinysurveys, ...  ) and software. 
We aim to standardize and automate the recap process as much as possible. 

# How users can get started with the project
A full implementation requires teachers to have RStudio and the possibility to launch a shiny app (e.g. on shinyapps.io with a free plan). 
Anyway, as a starter, functionality can be tested also locally within RStudio.  
Teacher without RStudio experience may find someone else to set up the shiny apps on shinyapps.io and thereafter even use the tools without RStudio. 

## Local testing in RStudio
Download the Project from github.com/mspngnbrg/shinymwe and open it with RStudio.

Open app.R in the 01_collect_questions folder. 
Click "Run app" above the script. 
In the shiny app, enter questions and answers and mark correct answers by clicking the checkbox below them. 
If the collection of the question and answer sets is done, go to the download page and download the .csv file. 
This file is in the correct format to create a shinysurvey from it. 

Open app.R in the 002_run_quiz folder. 
Click "Run app" above the script. 
Upload the .csv file with the quiz questions. 
Answer the quiz on the "Quiz" panel. 

On the "Solutions" panel, chose a question ID and see how many of the answer option votes were correct ("green") or incorrect("black").  

## Full implementation
As of now, the password to reset collected question and answer sets is displayed on the shiny page. 
You "may" wish to change it and to un-comment / delete the text above the text field before you upload the apps to your own shinyapps.io account or something similar. 
Setting up a free shinyapps.io account is easy, and uploading your app via the rsconnect package there is described here:
https://shiny.rstudio.com/articles/shinyapps.html


# Getting help / get involved
If you find an error, please file an issue here: https://github.com/mspngnbrg/shinymwe/issues . 

If you have suggestions for improvement, please contact us.

# Contributors
This project is based mainly on ideas of members of the ecomod group (ecosystem modelling) at the Uni Göttingen. 
