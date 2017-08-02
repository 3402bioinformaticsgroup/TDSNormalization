# ui.R

library(shiny)

shinyUI(fluidPage(
 titlePanel("NanoString Normalization Pt1"),
 sidebarLayout(
   sidebarPanel(

      fileInput("NanoStringData", label = h3("NanoString Expression Profile(.csv)")),
      fileInput("PanelReference", label = h3("Panel Reference(.csv)")),
      textInput("Condition1", "Index of condition1:", "1,2"),
      textInput("Condition2", "Index of condition2:", "3,4"),
      textInput("LogFC.cutoff", "LogFC cutoff value:", "1"),
      textInput("outputfile", "Output file name","Normalized Reads"),
      actionButton("outputreport", "Generate PDF report"),
      downloadButton('downloadData', 'Download Normalized Profile'),
      uiOutput("pdfview")

   ),

 mainPanel(
   tableOutput("input.view"),
   fluidRow(
       column(6, plotOutput("plotRawCV")),
       column(6, plotOutput("plotProcessedCV"))
),
   fluidRow(
       column(6, plotOutput("plotCV"))
#column(6, plotOutput("plotPower"))
)

              )
           )
        )

     )


