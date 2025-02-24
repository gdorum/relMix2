

# Define server logic
server <- function(input, output, session) {

  # Upload tab ---------------------------------------------------------

    datasets <- reactiveValues(mix = NULL, ref = NULL, db = NULL)

    #Example data
    observeEvent(input$load_btn, {
      # Load the specific data set
      datasets$mix <- read.table(here::here("inst","extdata","mixture.txt"), header = TRUE, sep = "\t")
      datasets$ref <- read.table(here::here("inst","extdata","references.txt"), header = TRUE, sep = "\t")
      datasets$db <- read.table(here::here("inst","extdata","frequencies22Markers.txt"), header = TRUE, sep = "\t")

      # Render the data table
      output$preview_database <- DT::renderDT({
        db = req(datasets$db)
        #db = unobserved_allele(db)
        datatable(db, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })
      output$preview_mixture <- DT::renderDT({
        mix = req(datasets$mix)
        datatable(mix, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })


      output$preview_reference <- DT::renderDT({
        ref = req(datasets$ref) |> prettyRef()
        datatable(ref, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })
  })



  #Read mixture file
  observe({print("input$mixture")
    req(input$mixture)
    datasets$mix <- read.table(input$mixture$datapath, sep = "\t", header=TRUE)
    output$preview_mixture<- DT::renderDT({
      mix = req(datasets$mix)
      datatable(mix, rownames = FALSE,
                options = list(scrollY = "300px", paging = FALSE, dom = "t"))
    })
  })

  #Read reference file
  observe({print("input$reference")
    req(input$reference)
    datasets$ref <- read.table(input$reference$datapath, sep = "\t", header=TRUE)
   if(ncol(datasets$ref) != 4)
      stop("Reference file must have 4 columns")
      names(datasets$ref) = c("SampleName", "Marker", "Allele1", "Allele2")

    output$preview_reference<- DT::renderDT({
      refs = req(datasets$ref) |> prettyRef()
      datatable(refs, rownames = FALSE,
                options = list(scrollY = "300px", paging = FALSE, dom = "t"))
    })
  })

  #Read database file
  observe({print("input$database")
    req(input$database)
    datasets$db <- read.table(input$database$datapath, sep = "\t", header=TRUE)
    output$preview_database<- DT::renderDT({
      db = req(datasets$db)
      datatable(db, rownames = FALSE,
                options = list(scrollY = "300px", paging = FALSE, dom = "t"))
    })
  })

  # raw <- reactive({ print("input$mixture")
  #   req(input$mixture)
  #   read.table(input$mixture$datapath, sep = "\t", header=TRUE)
  # })
  #
  #  output$preview_mixture <- DT::renderDT({
  #    mix = req(raw())
  #    datatable(mix, rownames = FALSE,
  #              options = list(scrollY = "300px", paging = FALSE, dom = "t"))
  #    }, label="readMix")
   # raw2 <- reactive({ print("input$reference")
   #   req(input$reference)
   #
   #   df = read.table(input$reference$datapath, sep = "\t", header=TRUE)
   #   if(ncol(df) != 4)
   #     stop("Reference file must have 4 columns")
   #   names(df) = c("SampleName", "Marker", "Allele1", "Allele2")
   #   df
   # }, label="readRef")

   # output$preview_reference <- DT::renderDT({
   #   refs = req(raw2()) |> prettyRef()
   #   datatable(refs, rownames = FALSE,
   #             options = list(scrollY = "300px", paging = FALSE, dom = "t"))
   # })


   # raw3 <- reactive({
   #   req(input$database)
   #   read.table(input$database$datapath, sep = "\t", header=TRUE)
   # }, label="readDB")
   # output$preview_database <- DT::renderDT({
   #   db = req(raw3())
   #   datatable(db, rownames = FALSE,
   #             options = list(scrollY = "300px", paging = FALSE, dom = "t"))
   # })

  #observeEvent(input$check,{
  observe({
    print("Checking marker names")
    req(datasets$mix, datasets$ref, datasets$db)

    missing_markers <- checkMissingMarkers(mix=datasets$mix, ref=datasets$ref, db=datasets$db)

    if (nrow(missing_markers) > 0) {
      shinyalert::shinyalert(
        title = "Missing markers",
        text = paste(missing_markers$Marker, collapse = "\n"),
        type = "info"
      )
    } else {
      shinyalert::shinyalert(
        title = "OK",
        text = "Data is valid",
        type = "info"
      )
    }
  })


    # Set parameters tab ----------------------------

   #If user chooses stepwise model, allow range to be set
  #How to let range be greyed out when model is not stepwise?
   observeEvent(input$mutModel, {
     if (input$mutModel == "Stepwise") {
       updateNumericInput(session, "range", value=0.5, min=0, max=1, step=0.01)
      }  else {
        updateNumericInput(session, "range", value=NULL)
      }
   })



  # Pedigrees tab -------------------

  pedigrees <- reactiveValues(ped1 = NULL, ped2 = NULL)

   #Create pedigree under H1, give choice of contributors to include in mixture, and plot pedigree
   observeEvent(input$pedigree1, {
    req(input$pedigree1)
     pedigrees$ped1 <- createPedigree(input$pedigree1)
     #Choices should be updated according to the names of the individuals in the pedigree
     updateCheckboxGroupInput(session, "cont1", choices=pedigrees$ped1$id)
      #The pedtools arguments create warnings
     output$plot1 <- renderPlot(plot(pedigrees$ped1))#, hatched=ped1$id, title="Pedigree under H1"))

     })
  #Create pedigree under H2, give choice of contributors to include in mixture, and plot pedigree
   observeEvent(input$pedigree2, {
   req(input$pedigree2)
     pedigrees$ped2 <- createPedigree(input$pedigree2)
     #Choices should be updated according to the names of the individuals in the pedigree
     updateCheckboxGroupInput(session, "cont2", choices=pedigrees$ped2$id)
     #The pedtools arguments create warnings
     output$plot2 <- renderPlot(plot(pedigrees$ped2))#, hatched=ped2$id,  title="Pedigree under H2"))
   })

  # LR calculations tab --------------------
   observeEvent(input$LRbut, {

     req(datasets$mix,datasets$ref,datasets$db)
    req(pedigrees$ped1,pedigrees$ped2)

    LRvalue <- calculateLR(datasets$mix,datasets$ref,datasets$db, list(pedigrees$ped1,pedigrees$ped2), idxC1=c("Mother","Child"), idxC2=c("Mother","Child"), drop=list("Mother"=0,"Child"=0,dropin=0.05))
    output$LR <- DT::renderDT({
      datatable(cbind(Marker=c(datasets$mix$Marker,"Total"),LR=round(c(LRvalue,prod(LRvalue)),2)), rownames = FALSE,
                options = list(scrollY = "300px", paging = FALSE, dom = "t"))
    })
   })

}
