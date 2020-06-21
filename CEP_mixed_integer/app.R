library(shiny)
library(Rglpk)
library(tidyverse)
library(janitor)



set.seed(12345)
df<-read_csv("LAUSD_raw.csv") %>% select(school_name=school_name.x,enrollment,isp,cep) %>%
    mutate(objective=floor(enrollment*runif(n = n(),min = 100,max=200)))
# Define UI for data upload app ----
ui <- fluidPage(
    p("This workbook can calculate Optimal CEP participation in a few distinct scenarios based on your district's needs.\n Currently, it only calculate the maximum coverage for universal meal service. We have built a mixed-integer solver and a fairness constrained optimization process."),
    p("Click the link below to learn more about how optimization could improve meal and food insecurity in California."),
    p("https://rpubs.com/dnlang86/625269"),
    p("In order to use this toolkit, upload your data as a csv with the following data the number of students enrolled as enrollment, the ISP percentage for each school(0-1), and a name."),
    p("Click Demonstration see to an example dataset"),
    p("Specify the lowest ISP percentage that you are willing to accept "),
    p("If you wish to specify an alternative objective than maximize student coverage, create a column called objective with the associated school weights."),



    # App title ----
    titlePanel("Uploading Files"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Horizontal line ----
            tags$hr(),
            sliderInput("isp_cutoff", "ISP Cutoff:", 40, min = 40, max = 62.5),
            verbatimTextOutput("value"),

            # Input: Checkbox if file has header ----
            #checkboxInput("header", "Header", TRUE),

            # Input: Select separator ----
            # radioButtons("sep", "Separator",
            #              choices = c(Comma = ",",
            #                          Semicolon = ";",
            #                          Tab = "\t"),
            #              selected = ","),

            # Input: Select quotes ----
            # radioButtons("quote", "Quote",
            #              choices = c(None = "",
            #                          "Double Quote" = '"',
            #                          "Single Quote" = "'"),
            #              selected = '"'),

            # Horizontal line ----
            tags$hr(),

            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),

            # Input: Select number of rows to display ----
            radioButtons("Policy", "Policy Objective",
                         choices = c(Demonstration = "Demo",
                                     `Fairness Constrained Maximization`="Fairness",
                                     `Maximize Coverage`="Coverage",
                                     `Alternative Weights` = "Objective"),
                         selected = "head"),
            tags$hr(),
            downloadButton("downloadData", "Download")



        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Data file ----
            tableOutput("contents"),
            plotOutput('plot1')

        )

    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    output$value <- renderText({ input$obs })
    output$contents<- renderTable(df)
    print("hello")



    output$contents <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$Policy)
        print(input$Policy)
        if(input$Policy=="Demo"){
            df<-read_csv("LAUSD.csv") %>% select(schoolname=school_name.x,enrollment,isp) %>%
                mutate(identified=floor(isp*enrollment),slack=identified-enrollment*input$isp_cutoff/100)

            obj <- df %>% pull(enrollment)
            constr1<- df %>% pull(slack)
            mat <- matrix(constr1, nrow = 1)
            dir <- c(">=")
            rhs <- c(0)
            types <- c("B")
            Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = TRUE)->sol
            df$optimal_cep<-sol$solution





        }
        else if (input$Policy=="Coverage"){
        df <- read_csv(input$file1$datapath) %>% select(contains("school"),contains("enrollment"),contains("isp"))  %>%
            mutate(identified=floor(isp*enrollment),slack=identified-enrollment*input$isp_cutoff/100)

        obj <- df %>% pull(enrollment)
        constr1<- df %>% pull(slack)
        mat <- matrix(constr1, nrow = 1)
        dir <- c(">=")
        rhs <- c(0)
        types <- c("B")
        Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = TRUE)->sol
        df$optimal_cep<-sol$solution
        optimal_df<<-df

         #              header = input$header,
        #               sep = input$sep,
        #               quote = input$quote)
        }
        else if (input$Policy=="Objective"){
            df <- read_csv(input$file1$datapath) %>% select(contains("school"),contains("enrollment"),contains("isp"))  %>%
                mutate(identified=floor(isp*enrollment),slack=identified-enrollment*input$isp_cutoff/100)

            obj <- df %>% pull(objective)
            constr1<- df %>% pull(slack)
            mat <- matrix(constr1, nrow = 1)
            dir <- c(">=")
            rhs <- c(0)
            types <- c("B")
            Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = TRUE)->sol
            df$optimal_cep<-sol$solution
            optimal_df<<-df

            #              header = input$header,
            #               sep = input$sep,
            #               quote = input$quote)
        }
        else if (input$Policy=="Fairness"){
            print("Fairness")
            df_clean <- read_csv(input$file1$datapath) %>%
                select(contains("school"),contains("enrollment"),contains("isp"))  %>%
                mutate(identified=floor(isp*enrollment),slack=identified-enrollment*input$isp_cutoff/100,lea_id=1,rn=row_number())

                cutpoint_all <- tibble()
                min_eligibility <- input$isp_cutoff/100
                for (i in (0:250)) {

                    cutoff_value <- i / 400

                    cutpoint_df <-
                        df_clean %>%
                        ungroup() %>%
                        mutate(isp = replace_na(isp, 0),cutpt=cutoff_value) %>%
                        mutate(identified_student_num = floor(enrollment * isp), above_cutoff=isp>cutoff_value) %>%
                        filter(above_cutoff) %>%
                        summarise(
                            total_identified = sum(identified_student_num, na.rm = TRUE),
                            total_enrolled = sum(enrollment, na.rm = TRUE)
                        ) %>%
                        mutate( percent_identified = total_identified / total_enrolled,cutpt=cutoff_value)



                    cutpoint_all <- cutpoint_all %>% bind_rows(cutpoint_df)


                }

                cutpoint_all
                cutpoint_opt<-cutpoint_all %>%
                    filter(percent_identified>min_eligibility)  %>%
                    filter(total_enrolled==max(total_enrolled)) %>%
                    filter(cutpt==min(cutpt))  %>%
                    pull(cutpt)

                df<-
                    df_clean %>%
                    mutate(isp = replace_na(isp, 0),cutpt=cutpoint_opt) %>%
                    group_by(lea_id) %>%
                    mutate(identified_student_num = floor(enrollment * isp), optimal_cep=isp>cutpoint_opt)  %>%
                    select(isp,enrollment, (everything()))

                optimal_df<<-df

        }

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        optimal_df<-df %>% mutate(test="here")



    })
        datasetInput <- reactive({optimal_df
    })



    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }




    )
    garbage<-optimal_df

    output$plot1 <- renderPlot({
        print(optimal_df)

        optimal_df %>% summarise(total_enrollment=sum(enrollment,na.rm = TRUE),
                                 optimal_coverage=sum(enrollment*optimal_cep),
                                 individual_coverage=sum(enrollment*(isp>input$isp_cutoff)))
        as_tibble(cbind(category = names(plot_data), t(plot_data))) %>%
            transmute(category,students=as.numeric(V2))  %>%
            ggplot(aes(category,students))+
            geom_col()+
            scale_y_continuous(label=scales::label_number())+
            labs(
                title= str_c("Optimal CEP under  ",input$Policy, " Policy", "under",input$isp_cutoff)
            )
    })

}
# Run the app ----
shinyApp(ui, server)
