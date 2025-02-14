library(DT)

ui_upload_mixture <- sidebarLayout(
  sidebarPanel(
    fileInput("mixture", "Upload a mixture file",accept = c(".csv", ".tsv",".txt")),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Mixture profile"),
    tableOutput("preview1")
  )
)

ui_upload_reference <- sidebarLayout(
  sidebarPanel(
    fileInput("reference", "Upload a reference file",accept = c(".csv", ".tsv",".txt")),
    textInput("delim2", "Delimiter (leave blank to guess)", ""),
    numericInput("skip2", "Rows to skip", 0, min = 0),
    numericInput("rows2", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Reference profiles"),
    DT::DTOutput("preview2")
  )
)

ui_upload_database <- sidebarLayout(
  sidebarPanel(
    fileInput("database", "Upload a database file",accept = c(".csv", ".tsv",".txt")),
    textInput("delim3", "Delimiter (leave blank to guess)", ""),
    numericInput("skip3", "Rows to skip", 0, min = 0),
    numericInput("rows3", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Database"),
    tableOutput("preview3")
  )
)

ui_database <-
  # fluidRow(
  # column(4,
  #  numericInput("theta","Theta",0, min=0, max=1),
  #  numericInput("silent","Silent allele frequency",0, min=0, max=1),
  #  numericInput("maf","Minimum allele frequency",0.001, min=0, max=1)
  # ),
  # fluidRow(
  #   column(6,
  #          numericInput("theta","Theta",0, min=0, max=1),
  #          numericInput("silent","Silent allele frequency",0, min=0, max=1),
  #          numericInput("maf","Minimum allele frequency",0.001, min=0, max=1)
  #   )
  # )

    sidebarLayout(
    sidebarPanel(
      h3("Database options"),
      numericInput("theta","Theta",0, min=0, max=1, step=0.01),
      numericInput("silent","Silent allele frequency",0, min=0, max=1, step=0.01),
      numericInput("maf","Minimum allele frequency",0.001, min=0, max=1,step=0.001)
    ),
    mainPanel(
      h3("Mutations"),
      selectInput("mutModel", "Mutation model", c("Equal", "Proportional", "Stepwise")),
      numericInput("range","Range (for stepwise only)",value=NULL),
      numericInput("mutFem","Female",0, min=0, max=1, step=0.01),
      numericInput("mutFem","Male",0, min=0, max=1, step=0.01)
      )
  )



ui_pedigrees <-
  sidebarLayout(
    sidebarPanel(
      h3("Pedigrees"),
      selectInput("pedigree1","Pedigree under H1",c("Paternity", "Non-paternity")),
      selectInput("pedigree2","Pedigree under H2",c("Non-paternity","Paternity")),
      #These should be updated based on the pedigrees chosen above
      checkboxGroupInput(
        inputId = "cont1",
        label = "Contributors under H1:",
        choices = c("Mother", "Father", "Child")), #Choices should depend on names in pedigree
      checkboxGroupInput(
        inputId = "cont2",
        label = "Contributors under H2:",
        choices = c("Mother", "Father", "Child")) #Choices should depend on names in pedigree
    ),
    mainPanel(
      h3("Pedigree plots"),
      plotOutput("plot1", height = "200px", width = "200px"),
      plotOutput("plot2", height = "200px", width = "200px")
    )
  )

# Define UI
fluidPage(
 # Application title
  titlePanel("relMix2"),

  tabsetPanel(
    tabPanel("Import data",
    ui_upload_mixture,
    ui_upload_reference,
    ui_upload_database
    ),
    tabPanel("Set parameters",
             ui_database
             ),
    tabPanel("Pedigrees",
             ui_pedigrees
             ),
    tabPanel("Results")
  )
)


