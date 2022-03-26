# collect questions
# students invent 1 question and 3 potential answers

library(shiny)
outputDir <- paste0("../questions_", Sys.Date())
if(!file.exists(outputDir)) dir.create(outputDir)

saveData <- function(data) {

    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
        x = data,
        file = file.path(outputDir, fileName),
        row.names = FALSE, quote = TRUE
    )
}

# Define the fields we want to save from the form
fields <- c("question", paste0("a", 1:3)) # modify if 4 answers are needed

# Shiny app with 3 fields that the user can submit data for
shinyApp(
    ui = fluidPage(
        DT::dataTableOutput("responses", width = 300), tags$hr(),
        textInput("question", "Question", ""),
        textInput("a1", "Answer 1", ""),
        textInput("a2", "Answer 2", ""),
        textInput("a3", "Answer 3", ""), # modify if 4 answers are needed
        actionButton("submit", "Submit")
    ),
    server = function(input, output, session) {

        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            tibble <- tibble::tibble(question = data[1],
                                     option = data[2:4],
                                     input_type = "mc",
                                     input_id = "42dada",
                                     dependence = NA,
                                     dependence_value = NA,
                                     required = TRUE) # modify if 4 answers are needed
            data <- tibble
        })

        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
        })
    }
)

