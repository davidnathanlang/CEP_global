library(DT)
library(shiny)
library(rsconnect)
library(tidyverse)
#textone<-"This Shiny App is Designed to help identify which school districts are currently participating in the Community Eligibility Provision of the No-Hungry-Kids Act. The Community Eligibility Provision allows schools to provide universal meal service to schools that have at least 40% of students identified as eligible for free or reduced price lunch via programs such as SNAP, TANF and CalFresh. In exchange for providing universal meal service, the federal government will offer a substantial subsidy for all meals provided. An interesting aspect of this program is that school districts can satisfy this requirement for a district, individual schools, or a subset of schools. For instance if two similarly sized schools have 35% and 45% eligibility, then on average they satisfy this requirement.”
#?p()

df<-read_rds("optimal_df.rds") %>%
  mutate_at(c("current_coverage","optimal_coverage") , ~round(.*100,digits=1))
ui <- basicPage(
  h2("Optimal Eligibility Under the CEP"),
  p(
    str_c(
      "This Shiny App is Designed to help identify which school districts are currently participating in the Community Eligibility Provision of the No-Hungry-Kids Act.",
      "The Community Eligibility Provision allows schools to provide universal meal service to schools that have at least 40% of students identified as eligible for free or reduced price lunch.",
      "Students count towards the eligibility provision if they are currently receiving benefits through programs like SNAP, TANF, and CalFresh."
    )
  ),
  p(
    str_c(
      "In exchange for providing universal meal service, schools receive a substantial federal subsidy to their lunch programs",
      " An interesting aspect of this program is that school districts can satisfy this requirement for a district, individual schools, or a subset of schools. For instance if two similarly sized schools have 35% and 45% eligibility, then on average they satisfy this requirement."
    )
  ),
  p(
    "Intriguingly, relatively few districts take full advantage of this provision. I went through and tried to maximize the number of student who could receive the service using the data that is publicly available at the Food Research and Action Committee. These results suggest that nearly 13 million students could receive universal meal service through the Community Eligibility Provision."
  ),


  #       " The Community Eligibility Provision allows schools to provide universal meal service to schools that have at least 40% of students identified as eligible for free or reduced price lunch via programs such as SNAP, TANF and CalFresh. In exchange for providing universal meal service, the federal government will offer a substantial subsidy for all meals provided. An interesting aspect of this program is that school districts can satisfy this requirement for a district, individual schools, or a subset of schools. For instance if two similarly sized schools have 35% and 45% eligibility, then on average they satisfy this requirement.”


  DT::dataTableOutput("mytable"),
  h2("About the Data"),
  p("The data for this analysis came from FRAC  (frac.org/research/resource-library/community-eligibility-cep-database).I cannot speak to the full accuracy of this data but I believe it to be a reasonable approximation of eligibility at many districts"),
  h2("About Me"),

  p("My name is David Lang (dnlang86@stanford.edu) . I am graduate student at Stanford University's Graduate School of Education. If you are a decision-maker at a school district. I would love to help you optimize your district's coverage under this program.")
)

server <- function(input, output) {
  output$mytable = DT::renderDataTable({
    df
  })
}

shinyApp(ui, server)


