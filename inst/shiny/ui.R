library(DT)

# Database tab ---------
ui_Database <- fluidPage(

  fluidRow(
    column(3, style = "margin-top: 20px",
           wellPanel(
             h4(""),
             actionButton("load_btn1", "Load example database")
           ),
           wellPanel(
             h4(""),
             fileInput("database", paste("Upload a database file"),accept = c(".csv", ".tsv",".txt"))
           ),
    ),
    column(9,  style = "margin-bottom: 20px; height:430px; width:700px",
           mainPanel(
             h3("Database"),
             DT::DTOutput("preview_database")
             , width = 12)
    )
  ),
  fluidRow(
      column(3,style = "margin-right: 50px",
           wellPanel(
             h4("Database options"),
                   numericInput("theta","Theta",0, min=0, max=1, step=0.01),
                   numericInput("silent","Silent allele frequency",0, min=0, max=1, step=0.01),
                   numericInput("maf","Minimum allele frequency",0.001, min=0, max=1,step=0.001),
           ),
      ),
      column(3,
           wellPanel(
             h4("Mutations"),
                   selectInput("mutModel", "Mutation model", c("Equal", "Proportional", "Stepwise"), selected="Equal"),
                   numericInput("range","Range (for stepwise only)",value=0.5),
                   numericInput("mutFem","Female",0, min=0, max=1, step=0.01),
                   numericInput("mutMale","Male",0, min=0, max=1, step=0.01)
          )
        )
      )
)

# Import data tab ----------
ui_Data <- fluidPage(

  fluidRow(
    column(3, style = "margin-top: 20px",
           wellPanel(
             h4(""),
             actionButton("load_btn2", "Load example data")
           ),
           wellPanel(
             h4(""),
             fileInput("mixture", paste("Upload a mixture file"),accept = c(".csv", ".tsv",".txt"))
           ),
    ),
    column(9,  style = "margin-bottom: 20px; height:430px; width:700px",
           mainPanel(
             h3("Mixture profile"),
             DT::DTOutput("preview_mixture")
             , width = 12)
    ),

    column(3, style = "margin-top: 20px",
           wellPanel(
             h4(""),
             fileInput("mixture", paste("Upload a reference file"),accept = c(".csv", ".tsv",".txt"))
      ),
    ),
    column(9,  style = "margin-bottom: 20px; height:430px; width:700px",
           mainPanel(
             h3("Reference profile"),
             DT::DTOutput("preview_reference")
             , width = 12)
    )
  )

)


# Pedigrees tab ----------------
ui_pedigrees <-
  sidebarLayout(
    sidebarPanel(

      h4("Pedigree under H1"),
        selectInput("pedigree1","Built-in pedigree",
                    choices = c("","Paternity", "Non-paternity"),selected = NULL),
      h4("--- or ---"),
        fileInput("customPed1", "Load a ped file"),

      h4("Pedigree under H2"),
        selectInput("pedigree2","Built-in predigree",
                    choices = c("","Paternity", "Non-paternity"),selected = NULL),
        h4("--- or ---"),
        fileInput("customPed2", "Upload your file"),

      #These should be updated based on the pedigrees chosen above
      checkboxGroupInput(
        inputId = "cont1",
        label = "Contributors under H1:",
        choices = NULL), #Choices depend on names in pedigree
      checkboxGroupInput(
        inputId = "cont2",
        label = "Contributors under H2:",
        choices = NULL), #Choices depend on names in pedigree
      uiOutput("numericInputs"), #dropout per contributor
      style = "margin-top: 20px"),
    mainPanel(
      h3("Pedigree plots"),
      #plotOutput("plot1", height = "200px", width = "200px"),
      #plotOutput("plot2", height = "200px", width = "200px")
      plotOutput("plot_peds", height = "200px", width = "400px")
    )
  )

# LR calculations tab ------------

ui_LR <- fluidRow(
    column(3, style = "margin-top: 20px",
           wellPanel(
             h4("Database options"),
             DT::DTOutput("database")
           ),
           wellPanel(
             h4("Mutations"),
             DT::DTOutput("mutations")
           ),
           wellPanel(
             h4("Dropout and drop-in"),
             DT::DTOutput("drop")
           ),
    ),
    mainPanel(
      h4("Pedigrees"),
      plotOutput("plot3", height = "200px", width = "500px")
    ),
    column(9, style = "margin-top: 20px",
           wellPanel(
             style = "border: 1px solid #ccc; padding: 10px;",
             h3("Results"),
             actionButton("LRbut", "Calculate LR"),
             div(style = "margin-top: 20px;",
                 #h3("Results"),
                 DT::DTOutput("LR")
             )
           )
    )
)

# Define UI
fluidPage(

  titlePanel("relMix2"),

  tabsetPanel(
    tabPanel("Database",
    ui_Database
    ),
    tabPanel("Import data",
    ui_Data
    ),
    tabPanel("Pedigrees",
             ui_pedigrees
             ),
    tabPanel("LR calculations",
             ui_LR
             )
  )
)


