# collect questions
# =================
# students submit 1 question and 3 potential answers (a "set")
# "sets" are stored in dropbox folder or in a local file (for testing)

library(shiny)
library(rdrop2)

`%>%` <- magrittr::`%>%`

run_locally <- TRUE # FALSE uses Dropbox

outputDir <- "questions_1"

# DANGER AREA! DELETES OLD DATA IF TRUE!
delete_old_data <- TRUE
if(delete_old_data) unlink(paste0("../local", outputDir, "/"))

saveData <- function(data) {

    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))

    if(run_locally){
        # save locally
        localDir <- paste0("../local", outputDir, "/")
        if(!dir.exists( localDir)) dir.create(localDir)

        write.csv(
            x = data,
            file = paste0(localDir, fileName),
            row.names = FALSE, quote = TRUE)
    } else {
        # Dropbox
        # Write the data to a temporary file locally & upload to dropbox
        filePath <- file.path(tempdir(), fileName)

        write.csv(
            x = data,
            file = filePath,
            row.names = FALSE, quote = TRUE)

        drop_upload(filePath, path = outputDir)
    }
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
        actionButton("submit", "Submit, all answers MUST differ!"),
        textOutput("Please click")
    ),
    server = function(input, output, session) {

        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            # TODO: check that no answers are doublettes
            tibble <- tibble::tibble(question = data[1],
                                     option = data[2:4],
                                     input_type = "mc",
                                     input_id = "id",
                                     dependence = NA,
                                     dependence_value = NA,
                                     required = TRUE) # modify if 4 answers are needed
            data <- tibble %>% mutate(across(everything(), as.character)) # numbers in text fields will crash otherwise
        })

        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
            updateActionButton(session, "submit",
                               label = "Thanks! Submit another one? ")
        })
    }
)

