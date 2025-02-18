library(DT)

# Example data
ui_exData <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      actionButton("load_btn", "Load Example Data")
    ),
    mainPanel(
      #tableOutput("mixEx")
    )
  )
)


#Load own data
ui_upload_mixture <- ui_upload("mixture")
ui_upload_reference <- ui_upload("reference")
ui_upload_database <- ui_upload("database")



ui_database <-

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
    ui_exData,
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
    tabPanel("LR calculations")
  )
)


