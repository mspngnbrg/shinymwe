# collect questions from students

library(shiny)
library(shinyjs)

fields <- c("question", paste0("a", 1:4), paste0("a", 1:4, "_true")) # modify if 4 answers are needed

ui <- fluidPage(
    useShinyjs(),
    navbarPage("Navbar",
               tabPanel("Questions",
                        helpText("Please enter one question and four answers. Mark correct answer(s) by clicking the check box below. Question is mandatory, not all answer fields must be filled."),

                        mainPanel(
                            div(id = "form",
                                textInput("question", "Question", ""),
                                textInput("a1", "Answer 1", ""),
                                checkboxInput("a1_true", "A1 true?"),
                                textInput("a2", "Answer 2", ""),
                                checkboxInput("a2_true", "A2 true?"),
                                textInput("a3", "Answer 3", ""),
                                checkboxInput("a3_true", "A3 true?"),# modify if 4 answers are needed
                                textInput("a4", "Answer 4", ""),
                                checkboxInput("a4_true", "A4 true?")# modify if 4 answers are needed
                            ),

                            actionButton("submit", "Submit"),

                        )
               ),
               tabPanel("Download questions",
                        mainPanel(
                            helpText("Here you can download all questions and answers in a suitable format for the quiz."),
                            downloadButton("downloadData", "Download"),
                            helpText("To delete all old Q&A sets, type the pw into the text field. This will be a teacher-only function in the future"),
                            helpText("pw is -I know it!- without he minus signs, only here for testing..."),
                            textInput("passphrase", "Pass phrase", ""),
                            actionButton("deleteOldAnswers", "Delete old answers")
                        )
               )
    )
)

server <- function(input, output, session) {

    # collect form data
    formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])

        tibble <- tibble::tibble(question = paste0("Q: ", data[1]),
                                 option = paste0("A", 1:4, ": ", data[2:5]),
                                 input_type = "checkboxgroup",
                                 input_id = "id",
                                 dependence = NA,
                                 dependence_value = NA,
                                 required = TRUE,
                                 "option_is_true" = data[6:9],
                                 counts = 0)



    })

    # reactive form data
    rv <- reactiveValues(df_data = NULL)

    observeEvent(input$submit, {
        if(!input$question == "") rv$df_data <- rbind(rv$df_data , formData())
        shinyjs::reset("form")
        updateActionButton(session, "submit", label = "Thanks! Submit another question?")
        updateActionButton(session, "deleteOldAnswers", label = "Delete old answers")
        d <- rv$df_data
        n_questions <- unique(d$question)
        for(i in 1:length(d$input_id)){
            d$input_id[i] <- paste0("id_", which(n_questions == d$question[i]) )
        }
        rv$df_data <- d

    })

    # csv download function
    output$downloadData <- downloadHandler(
        filename = "questions.csv",
        content = function(file) {
            write.csv(rv$df_data, file, row.names = FALSE)
        }
    )

    #  Delete old questions
    observeEvent(input$deleteOldAnswers, {
        if(input$passphrase == "I know it!"){
            rv$df_data <- NULL
            updateActionButton(session, "deleteOldAnswers", label = "Deleted")
        }
    })

}

# Run the application
shinyApp(ui = ui, server = server)

