# start quiz
# ==========

library(shinysurveys)
library(dplyr)
library(rdrop2)


# load folder names for inputs and outputs
d <- readRDS("./data/folder_names.rds")
inputDir <- d$questions_teacher
outputDir <- d$answers_raw
(DROPBOX <- d$dropbox) # TRUE uses Dropbox, FALSE saves files locally

loadTeachersFile <- function(){
    # load and aggregate teachers file

    if(DROPBOX){

        filesInfo <- drop_dir(inputDir)
        filePaths <- filesInfo$path_display
        data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
        data <- do.call(rbind, data)

    } else {
        localDir <- paste0("../", inputDir, "/")
        data <- list.files(path =localDir, pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>%
            purrr::map(readr::read_csv) %>%
            bind_rows()
    }
    data
}

saveData <- function(data) {

    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))

    if(DROPBOX){
        # Dropbox
        # Write the data to a temporary file locally & upload to dropbox
        filePath <- file.path(tempdir(), fileName)

        write.csv(x = data,
                  file = filePath,
                  row.names = FALSE, quote = TRUE)

        drop_upload(filePath, path = outputDir)

    } else {

        # save locally
        localDir <- paste0("../", outputDir, "/")
        if(!dir.exists( localDir)) dir.create(localDir)

        write.csv(
            x = data,
            file = paste0(localDir, fileName),
            row.names = FALSE, quote = TRUE)
    }
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
        if(DROPBOX){
            saveData(response_data)
        } else {
            localDir <- paste0("../", outputDir, "/")
            if(!dir.exists(localDir)) dir.create(localDir)
            fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
            write.csv(response_data,
                      file = paste0(localDir, fileName, ".csv"),
                      row.names = FALSE, quote = TRUE)

        }

        updateActionButton(session, "submit",
                           label = "Thanks! Done! ")

    })
}

shiny::shinyApp(ui = ui, server = server)
