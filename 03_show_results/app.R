# show results

# merge questions and new answer from next class

library(dplyr)
library(rdrop2)
library(ggplot2)

token <- readRDS("../Rmd/droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)

# load questions (teachers file)
inputQ <- "questions_1_teacher"
inputA <- "answers_1"
loadData <- function(inDir) {
    inputDir <- inDir
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
    return(data)
}

# load questions (teacher version) and answers from start of day 2
questions <- loadData(inputQ)
answers <- loadData(inputA)

# vote count per answer option
answer_summary <- answers %>% group_by(question_id, response) %>%
    summarize(counts = n())

names(answer_summary)[2] <- "option"

teacher_results <- merge(questions, answer_summary, all.x = TRUE)

teacher_results$counts[is.na(teacher_results$counts)] <- 0

teacher_results <- teacher_results %>% select(question_id, question, option, counts)

# TODO `%>%` <- magrittr::`%>%`

# give answers number 1:n for plotting
x <- teacher_results %>% group_by(question_id) %>%
    mutate(percent = counts / sum (counts) * 100,
           answer_id = 1:length(question_id) )

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
                        unique(x$question_id)
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
            filter(question_id == input$response)

        paste0(x$question[1])
    })

    output$a1_text <-  renderText({
        x <- x %>%
            filter(question_id == input$response)

        paste0(with(x,
                    option[answer_id == "1"]))
    })

    output$a2_text <-  renderText({
        x <- x %>%
            filter(question_id == input$response)

        paste0(with(x,
                    option[answer_id == "2"]))
    })

    output$a3_text <-  renderText({
        x <- x %>%
            filter(question_id == input$response)

        paste0(with(x,
                    option[answer_id == "3"]))
    })




    #output$a1 <- res$option[answer_id == "1"]
    #output$a2 <- res$option[answer_id == "2"]
    #output$a3 <- res$option[answer_id == "3"]

    # performance plot
    output$resPlot <- renderPlot({

        x <- x %>%
            filter(question_id == input$response)

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
