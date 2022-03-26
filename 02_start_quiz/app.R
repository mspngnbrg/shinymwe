# start quiz

library(shinysurveys)
library(dplyr)

# in_dir <- "./questions_2022-03-26"
in_dir <- "../questions_2022-03-26"
getwd()
loadData <- function() {
    # Read all the files into a list
    files <- list.files(in_dir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)

    # assign input ids
    n_questions <- unique(data$question)
    for(i in 1:length(data$input_id)){
        data$input_id[i] <- paste0(which(n_questions == data$question[i]), "id")
    }
    return(data)
}

df <- loadData()

ui <- shiny::fluidPage(
    shinysurveys::surveyOutput(df,
                               survey_title = "A minimal title",
                               survey_description = "A minimal description")
)

server <- function(input, output, session) {
    renderSurvey()

    observeEvent(input$submit, {
        response_data <- getSurveyData()
        print(response_data) # save results
    })
}

shiny::shinyApp(ui = ui, server = server)
