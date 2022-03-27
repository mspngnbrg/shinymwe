# token test with dropbox

library(rdrop2)
outputDir <- "responses"

saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
}

loadData <- function() {
    # Read all the files into a list
    filesInfo <- drop_dir(outputDir)
    filePaths <- filesInfo$path_display
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
}



# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
    ui = fluidPage(
        DT::dataTableOutput("responses", width = 300), tags$hr(),
        textInput("name", "Name", ""),
        checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
        sliderInput("r_num_years", "Number of years using R",
                    0, 25, 2, ticks = FALSE),
        actionButton("submit", "Submit")
    ),
    server = function(input, output, session) {

        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
        })

        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
        })

        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$responses <- DT::renderDataTable({
            input$submit
            loadData()
        })
    }
)
