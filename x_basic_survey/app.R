library(shiny)
library(shinysurveys)

outputDir <- paste0("../answers_", Sys.Date())
if(!file.exists(outputDir)) dir.create(outputDir)

saveData <- function(data) {

    data <- t(data)

    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
        x = data,
        file = file.path(outputDir, fileName),
        row.names = FALSE, quote = TRUE
    )
}

q_df <- tibble::tibble(question = "How much",
                       option = c("1", "Kartoffelsalat", "42"),
                       input_type = "mc",
                       input_id = "gender",
                       dependence = NA,
                       dependence_value = NA,
                       required = TRUE)

ui <- shiny::fluidPage(
    shinysurveys::surveyOutput(q_df,
                               survey_title = "A minimal title",
                               survey_description = "A minimal description")
)

server <- function(input, output, session) {

    shinysurveys::renderSurvey()

    observeEvent(input$submit, {
        response_data <- getSurveyData()
        # print(response_data)
        saveData(response_data)
    })

}

shiny::shinyApp(ui = ui, server = server)
