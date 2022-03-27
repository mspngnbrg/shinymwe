# start quiz

library(shinysurveys)
library(dplyr)
library(rdrop2)

inputDir <- "questions_1"
outputDirTeacher <- "questions_1_teacher" # save all questions in one file
outputDir <- "answers_1"

token <- readRDS("../Rmd/droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)

# TODO: data curating (checking questions for quality, appropriateness, doublettes, ... ) will be done in an external script in the future.

loadData <- function() {
    # Read all the files into a list
    filesInfo <- drop_dir(inputDir)
    filePaths <- filesInfo$path_display
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    # assign input ids
    n_questions <- unique(data$question)
    for(i in 1:length(data$input_id)){
        data$input_id[i] <- paste0("id_", which(n_questions == data$question[i]) )
    }
    saveData(data, outputDirTeacher)
    return(data)
}

saveData <- function(data, saveDir) {
    outputDir <- saveDir
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))

    # Write the data to a temporary file locally & upload to dropbox
    filePath <- file.path(tempdir(), fileName)
    write.csv(
        x = data,
        file = filePath,
        row.names = FALSE, quote = TRUE)

    drop_upload(filePath, path = outputDir)
}

df <- loadData() # file similar to teachers question file

ui <- shiny::fluidPage(
    shinysurveys::surveyOutput(df,
                               survey_title = "Quiz time! KT 1",
                               survey_description = "made with {shinysurveys}")
)

server <- function(input, output, session) {
    renderSurvey()

    observeEvent(input$submit, {
        response_data <- getSurveyData()
        saveData(response_data, outputDir)
        updateActionButton(session, "submit",
                           label = "Thanks!")

    })
}

shiny::shinyApp(ui = ui, server = server)
