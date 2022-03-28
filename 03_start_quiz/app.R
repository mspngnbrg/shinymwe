# start quiz
# ==========

library(shinysurveys)
library(dplyr)
library(rdrop2)

run_locally <- TRUE # FALSE uses Dropbox

inputDir <- "questions_1_teacher" # teachers file!
outputDir <- "answers_1"


loadTeachersFile <- function(){
    # load and aggregate teachers file

    if(run_locally){
        localDir <- paste0("../local", inputDir, "/")
        data <- list.files(path =localDir, pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>%
            purrr::map(readr::read_csv) %>%
            bind_rows()

    } else {
        # Dropbox
        filesInfo <- drop_dir(inputDir)
        filePaths <- filesInfo$path_display
        data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
        data <- do.call(rbind, data)
    }
    data
}

df <- loadTeachersFile() # file similar to teachers question file

ui <- shiny::fluidPage(
    shinysurveys::surveyOutput(df,
                               survey_title = "Quiz time! KT 1",
                               survey_description = "made with {shinysurveys}")
)

server <- function(input, output, session) {
    renderSurvey()

    observeEvent(input$submit, {

        response_data <- getSurveyData()

        # save results as .csv
        if(run_locally){
            localDir <- paste0("../local", outputDir, "/")
            if(!dir.exists(localDir)) dir.create(localDir)
            fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
            write.csv(response_data,
                      file = paste0(localDir, fileName, ".csv"),
                      row.names = FALSE, quote = TRUE)

        } else {
            # DROPBOX
            saveData(response_data)
        }

        updateActionButton(session, "submit",
                           label = "Thanks! Done! ")

    })
}

shiny::shinyApp(ui = ui, server = server)
