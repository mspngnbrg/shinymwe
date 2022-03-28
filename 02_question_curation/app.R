# Aggregate all Questions (Q) and Answers (A) in one table (teacher file)
# for a quick check & storing
# =======================================================================
# Formate as required by shinysurveys R package
# Shuffle answers? (TODO)

library(shiny)
library(dplyr)
library(rdrop2)

run_locally <- TRUE # FALSE uses Dropbox

inputDir <- "questions_1"
outputDir <- "questions_1_teacher"

load_questions <- function(){
    # load and aggregate all Q&A sets

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


}

data <- load_questions() # gives warning, but works well


# assign unique input ID for each Q&A set (required by shinysurveys package)
n_questions <- unique(data$question)
for(i in 1:length(data$input_id)){
    data$input_id[i] <- paste0("id_", which(n_questions == data$question[i]) )
}

# save teacher file
if(run_locally) {
    localDir <- paste0("../local", outputDir, "/")
    if(!dir.exists(localDir)) dir.create(localDir)

    write.csv(
        x = data,
        file = paste0(localDir, outputDir, ".csv"),
        row.names = FALSE, quote = TRUE)

} else {
    # Dropbox
    filePath <- file.path(tempdir(), paste0(outputDir, ".csv"))

    write.csv(
        x = data,
        file = filePath,
        row.names = FALSE, quote = TRUE)

    drop_upload(filePath, path = outputDir)
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Q&A overview, teacher file constructed... "),

    # Sidebar with a slider input for number of bins

                # Show data as table
        mainPanel(
            tableOutput('table')
        )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable(data[, 1:2])
}

# Run the application
shinyApp(ui = ui, server = server)
