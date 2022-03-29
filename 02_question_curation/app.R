# Aggregate all Questions (Q) and Answers (A) in one table (teacher file)
# for a quick check & storing
# =======================================================================
# Formate as required by shinysurveys R package
# Shuffle answers? (TODO)

library(shiny)
library(dplyr)
library(rdrop2)

DROPBOX <- FALSE # TRUE uses Dropbox, FALSE saves files locally

# load folder names for inputs and outputs
d <- readRDS("./data/folder_names.rds")
inputDir <- d$questions_raw
outputDir <- d$questions_teacher

load_questions <- function(){
    # load and aggregate all Q&A sets

    if(DROPBOX){
        # Dropbox
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
}

data <- load_questions() # gives warning, but works well

# assign unique input ID for each Q&A set (required by shinysurveys package)
n_questions <- unique(data$question)
for(i in 1:length(data$input_id)){
    data$input_id[i] <- paste0("id_", which(n_questions == data$question[i]) )
}

# save teacher file
if(DROPBOX) {
    filePath <- file.path(tempdir(), paste0(outputDir, ".csv"))

    write.csv(
        x = data,
        file = filePath,
        row.names = FALSE, quote = TRUE)

    drop_upload(filePath, path = outputDir)
} else {
    localDir <- paste0("../", outputDir, "/")
    if(!dir.exists(localDir)) dir.create(localDir)

    write.csv(
        x = data,
        file = paste0(localDir, outputDir, ".csv"),
        row.names = FALSE, quote = TRUE)
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

    output$table <- renderTable(data[, c(1, 4, 2)]) # Q, ID, A
}

# Run the application
shinyApp(ui = ui, server = server)
