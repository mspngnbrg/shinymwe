# show results
# ============

# merge questions and new answer from next class

library(dplyr)
library(rdrop2)
library(ggplot2)

run_locally <- TRUE # FALSE uses Dropbox

# load questions (teachers file)
inputQ <- "questions_1_teacher"
inputA <- "answers_1"

loadData <- function(inDir){

    # load and aggregate all answers
    inputDir <- inDir

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

(questions <- loadData(inputQ))
(answers <- loadData(inputA))
names(answers)[names(answers) == "question_id"] <- "input_id"
# vote count per answer option
answer_summary <- answers %>% group_by(input_id, response) %>%
    summarize(counts = n())

names(answer_summary)[2] <- "option"

(teacher_results <- merge(questions, answer_summary, all.x = TRUE))

(teacher_results$counts[is.na(teacher_results$counts)] <- 0)
teacher_results
teacher_results <- teacher_results %>% select(input_id, question, option, counts)

print(teacher_results)

# TODO `%>%` <- magrittr::`%>%`

# give answers number 1:n for plotting
x <- teacher_results %>% group_by(input_id) %>%
    mutate(percent = counts / sum (counts) * 100,
           answer_id = 1:length(input_id) )

# sort by answer IDs, make them a factor
x$answer_id <- factor(x$answer_id)
levels(x$answer_id)

ui <- fluidPage(

    # Application title
    titlePanel("Result presentation"),

    sidebarLayout(

        sidebarPanel(
            helpText("Select question ID, please."),
            # Sidebar for user choices

            selectInput("response", "Questions",
                        unique(x$input_id)
            ),
        ),

        # main panel for plotting
        mainPanel(
            h2(textOutput("question_text")),
            h4(textOutput("a1_text")),
            h4(textOutput("a2_text")),
            h4(textOutput("a3_text")),
            plotOutput("resPlot")
        )
    )
)

## SERVER STARTS HERE
server <- function(input, output) {

    output$question_text <-  renderText({
        x <- x %>%
            filter(input_id == input$response)

        paste0(x$question[1])
    })

    output$a1_text <-  renderText({
        x <- x %>%
            filter(input_id == input$response)

        paste0("1: ", with(x,
                         option[answer_id == "1"]))
    })

    output$a2_text <-  renderText({
        x <- x %>%
            filter(input_id == input$response)

        paste0("2: ", with(x,
                         option[answer_id == "2"]))
    })

    output$a3_text <-  renderText({
        x <- x %>%
            filter(input_id == input$response)

        paste0("3: ", with(x,
                        option[answer_id == "3"]))
    })




    #output$a1 <- res$option[answer_id == "1"]
    #output$a2 <- res$option[answer_id == "2"]
    #output$a3 <- res$option[answer_id == "3"]

    # performance plot
    output$resPlot <- renderPlot({

        x <- x %>%
            filter(input_id == input$response)

        plot(ggplot(x, aes(x = answer_id, y = percent))+
                 geom_bar(stat = "identity")+
                 scale_x_discrete(drop = FALSE)+
                 theme_classic(base_size = 16)+
                 ylim(0, 100)+
                 ylab("%"))
    })
}
## SERVER ENDS HERE
shinyApp(ui = ui, server = server)
