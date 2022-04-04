# create quiz & display student answers
# =====================================

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

shinysurveys::extendInputType(input_type = "checkboxgroup", {
    shiny::checkboxGroupInput(
        inputId = surveyID(),
        label = surveyLabel(),
        choices = surveyOptions()
    )
})

# how many answers per answer option?
votes_per_option <- function(quiz_answers, question_df, question_ID){
    quest <- question_df
    # get answer options from questions
    qa <- quiz_answers[quiz_answers$question_id == question_ID, ]
    q <- quest[quest$input_id == question_ID, ]
    q$counts <- NA

    for(i in 1:length(q$option)){
        q$counts[i] <- stringr::str_count(list(qa$response), q$option[i])
        # q$counts[i] <- sum(q$option[i] %in% qa$response)
    }

    return(q)
}

ui <- navbarPage("Navbar",
                 tabPanel("Quiz",
                          mainPanel(
                              textOutput("no_quiz_file"),
                              uiOutput("quiztime"),
                              tableOutput("quiz_answers")
                          )
                 ),
                 tabPanel("Solution",
                          mainPanel(
                              uiOutput("select_question_id"),
                              #verbatimTextOutput("text"),
                              tableOutput("table_test"),
                              plotOutput("answer_plot")
                          ),

                 ),
                 tabPanel("Upload questions",
                          mainPanel(
                              tabPanel("Solution",
                                       tabPanel("Upload",
                                                mainPanel(
                                                    fileInput("questions_csv", "Upload questions as .csv")

                                                )
                                       )
                              )
                          )
                 )
)

server <- function(input, output, session) {

    questions <- reactiveValues(df = NULL)

    output$no_quiz_file <- renderText({
            if(is.null(input$questions_csv))"Please upload questions."
    })

    output$quiztime <- renderUI({
        req(input$questions_csv)
        inFile <- input$questions_csv
        if (is.null(inFile))
            return(NULL)
        questions$df <- read.csv(inFile$datapath)
       # questions$df$aid <- rep(1:4, length(unique(questions$df$input_id)))
        shinysurveys::surveyOutput(questions$df,
                                   survey_title = "Quiz time!",
                                   survey_description = "made with {shinysurveys}")

    })


    # collect answers
    rv <- reactiveValues()
    rv$answers <- data.frame(NULL)

    # connect questions and answers from the quiz
    qrv <- reactiveValues()

    observeEvent(input$submit, {
        rv$answers <- dplyr::bind_rows(rv$answers, shinysurveys::getSurveyData())
        updateActionButton(session, "submit",
                           label = "Thanks! Done! ")
        # connect Q and As from quiz here
    })

    output$quiz_answers <- renderTable({
        req(rv$answers)
        rv$answers
    })

    output$select_question_id <- renderUI({
        var <- rv$answers[["question_id"]]
        lvl <- as.factor(var)
        selectInput("var", "Choose answer:", choices= lvl)
    })

    # display Q&A set above barplot
     output$table_test <- renderTable({
            req(input$var)
            txt <- votes_per_option(rv$answers, questions$df, input$var)
            data.frame("Question" = c(paste0(txt$question[1]), txt$option[1:4]))
        })

    # plot solutions (barplot)
    output$answer_plot <- renderPlot({

        req(input$var)

        df <- votes_per_option(rv$answers, questions$df, input$var)

        plot(ggplot(df, aes(x = substr(option, 1, 2), y = counts, fill = option_is_true))+
                 geom_bar(stat = "identity")+
                 scale_x_discrete(drop = FALSE)+
                 theme_classic(base_size = 16)+
                 scale_fill_manual(values=c("FALSE"="black","TRUE"="green"), name = "Correctness")+
                 ylab("Votes")+
                 xlab("Answer"))
    })

}

# Run the application
shinyApp(ui = ui, server = server)
