

# Define server logic
server <- function(input, output, session) {

  # Database tab ---------------------------------------------------------

    datasets <- reactiveValues(mix = NULL, ref = NULL, db = NULL)


    ## Example database ----
    observeEvent(input$load_btn1, {
      # Load the database
      datasets$db <- read.table(here::here("inst","extdata","frequencies22Markers.txt"), header = TRUE, sep = "\t")

      # Render the data table
      output$preview_database <- DT::renderDT({
        db = req(datasets$db)
        #db = unobserved_allele(db)
        datatable(db, rownames = FALSE,
                  options = list(scrollY = "300px", paging = TRUE, dom = "t"))

      })
    })


    ## Read database file from user ----
    observe({print("input$database")
      req(input$database)
      datasets$db <- read.table(input$database$datapath, sep = "\t", header=TRUE)

      #Display database
      output$preview_database<- DT::renderDT({
        db = req(datasets$db)
        datatable(db, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })
    })


      # --- Changes will be displayed in database ---

      ## Silent allele --------
      #Add silent allele if silent freq > 0
      observeEvent(input$silent, {
        if(input$silent>0){
          showModal(modalDialog(
            title = paste("Note: silent allele is added with frequency",input$silent),
            "Frequencies will be scaled to sum to 1",
            easyClose = TRUE,
            footer = NULL
          ))
          datasets$db <- silent_allele(datasets$db, input$silent)
        }
      })
      ## MAF -------
      #Check that input frequencies are above MAF
      observeEvent(input$maf, {
        if(any(datasets$db[,-1]<input$maf, na.rm=TRUE)){
          print("Text to print")

            showModal(modalDialog(
              title = "Note: some frequencies are below the minimum allele frequency",
                 "The frequencies will be updated and all frequencies will be scaled to sum to 1",
              easyClose = TRUE,
              footer = NULL
            ))
          datasets$db <- MAF_allele(datasets$db,input$maf)
        }
      })

      ## Scaling or rest allele ----
      observeEvent(datasets$db,{
        req(datasets$db)
        print("Scaling or  rest allele")

      # Define a tolerance level
      tolerance <- 1e-8

      freqs <- datasets$db[,-1]
      freqs <- apply(freqs, 2, function(x) as.numeric(as.character(x)))
      markerNames <- colnames(freqs)
      # Check if all elements are approximately equal to 1
      ix1 <- which((colSums(freqs,na.rm=TRUE) - 1) > tolerance) #Scale
      ix2 <- which((1 - colSums(freqs,na.rm=TRUE)) >  tolerance) #Scale or rest allele
      if(length(ix1)>0) { #Scaling is enforced
        showModal(modalDialog(
          title = paste0("The sum of the frequencies for the following markers is larger than 1: ", paste(markerNames[ix1],collapse=", ")),
          "The frequencies will be updated and all frequencies will be scaled to sum to 1",
          easyClose = TRUE,
          footer = NULL
        ))
        #Apply scaling
        datasets$db[,ix1+1] <- apply(datasets$db[,ix1+1,drop=FALSE], 2, function(x) x/sum(x, na.rm=TRUE))
      }
      if(length(ix2)>0)  { #Scaling or rest allele

        showModal( modalDialog(
          paste0("The sum of the frequencies for the following markers is less than 1: ", paste(markerNames[ix2],collapse=", "),
                 ". Do you want to scale or add a rest allele?"),
          title = "Scale or add rest allele",
          #footer = tagList(
            #actionButton("scale", "Scale"),
            #actionButton("rest", "Add rest allele")
          #)
        ))

        # observeEvent(input$scale, {
        #   showNotification("Scaling applied")
        #   datasets$db[,ix2+1] <- apply(freqs[,ix2,drop=FALSE], 2, function(x) x/sum(x, na.rm=TRUE))
        #   removeModal()
        # })
        # observeEvent(input$rest, {
        #   showNotification("Adding rest allele")
        #   freqr <- rep(NA,ncol(freqs))
        #   print(freqs)
        #   freqr[ix2] <- 1 - colSums(as.matrix(freqs[,ix2,drop=FALSE]),na.rm=TRUE)
        #   datasets$db <- rbind(datasets$db,c('r',freqr))
        #   removeModal()
        # })
      }
    })


    #If user chooses stepwise model, allow range to be set
    #How to let range be greyed out when model is not stepwise?
    observeEvent(input$mutModel, {
      if (input$mutModel == "Stepwise") {
        updateNumericInput(session, "range", value=0.5, min=0, max=1, step=0.01)
      }  else {
        updateNumericInput(session, "range", value=NULL)
      }
    })


    # Import data tab ---------------------------------------------------------

    #Example data
    observeEvent(input$load_btn2, {
      # Load the specific data set
      datasets$mix <- read.table(here::here("inst","extdata","mixture.txt"), header = TRUE, sep = "\t")
      datasets$ref <- read.table(here::here("inst","extdata","references.txt"), header = TRUE, sep = "\t")

      # Render the data table
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

  # #Check that data are valid
  # observe({
  #   print("Checking marker names")
  #   req(datasets$mix, datasets$ref, datasets$db)
  #
  #   missing_markers <- checkMissingMarkers(mix=datasets$mix, ref=datasets$ref, db=datasets$db)
  #
  #   if (nrow(missing_markers) > 0) {
  #     shinyalert::shinyalert(
  #       title = "Missing markers",
  #       text = paste(missing_markers$Marker, collapse = "\n"),
  #       type = "info"
  #     )
  #   } else {
  #     shinyalert::shinyalert(
  #       title = "OK",
  #       text = "Data is valid",
  #       type = "info"
  #     )
  #   }
  # })





  # Pedigrees tab -------------------

  pedigrees <- reactiveValues(ped1 = NULL, ped2 = NULL)

  toListen1 <- reactive({
    list( input$pedigree1,input$customPed1)
  })

  # Create pedigree under H1, give choice of contributors to include in mixture, and plot pedigree
  observeEvent(toListen1(),{
    if (!is.null(input$pedigree1) && input$pedigree1 != "") {
      req(input$pedigree1)
      pedigrees$ped1 <- createPedigree(input$pedigree1)
    } else if (!is.null(input$customPed1)) {
      req(input$customPed1)
      pedigrees$ped1 <- readCustomPedigree(input$customPed1$datapath)
    }

     #Contributor choices are updated according to the names of the individuals in the pedigree
     if (!is.null(pedigrees$ped1$id)) {
      updateCheckboxGroupInput(session, "cont1", choices=pedigrees$ped1$id)
     }
    req(pedigrees$ped1)
    #Plot pedigree. First convert from Familias pedigree to ped
    ped1 <- pedFamilias::Familias2ped(pedigrees$ped1, datamatrix = NULL, loci = NULL)
    output$plot1 <- renderPlot(plotPeds(list(ped1), typed=list(input$cont1), title="H1"))

  })

  toListen2 <- reactive({
    list( input$pedigree2,input$customPed2)
  })

   # Create pedigree under H2, give choice of contributors to include in mixture, and plot pedigree
  observeEvent(toListen2(),{
    if (!is.null(input$pedigree2) && input$pedigree2 != "") {
      req(input$pedigree2)
      print(input$pedigree2)
      pedigrees$ped2 <- createPedigree(input$pedigree2)
    } else if (!is.null(input$customPed2)) {
      req(input$customPed2)
      print(input$customPed2$datapath)
      pedigrees$ped2 <- readCustomPedigree(input$customPed2$datapath)
    }


     #Contributor choices are updated according to the names of the individuals in the pedigree
     if (!is.null(pedigrees$ped2$id)) {
      updateCheckboxGroupInput(session, "cont2", choices=pedigrees$ped2$id)
     }

    req(pedigrees$ped2)
    #Plot pedigree. First convert from Familias pedigree to ped
    ped2 <- pedFamilias::Familias2ped(pedigrees$ped2, datamatrix = NULL, loci = NULL)
    output$plot2 <- renderPlot(plotPeds(list(ped2), typed=list(input$cont2), title="H2"))

   })

   #Dropout for chosen contributors
   output$numericInputs <- renderUI({
     req(input$cont1, input$cont2)
     cont <- unique(c(input$cont1,input$cont2))

     # Create numeric inputs for checked boxes
     numericInputsList <- lapply(cont, function(opt) {
       numericInput(inputId = opt,label = paste("Dropout for", opt),
                    value = 0,min = 0,max = 1, step=0.01)
     })

     # Add the "Drop-in" numeric input
     numericInputsList <- c(numericInputsList, list(
       numericInput(inputId = "dropin",label = "Drop-in",value = 0, min = 0, max = 1, step=0.01)
     ))

     # Return the combined UI elements
     do.call(tagList, numericInputsList)

  })
#
#    #Plot both pedigrees
#    observeEvent({
#      pedigrees$ped1
#      pedigrees$ped2
#      input$cont1
#      input$cont2
#    },{
#
#    #Plot pedigree
#    #Convert FamiliasPedigree to ped
#    ped1 <- pedFamilias::Familias2ped(pedigrees$ped1, datamatrix = NULL, loci = NULL)
#    ped2 <- pedFamilias::Familias2ped(pedigrees$ped2, datamatrix = NULL, loci = NULL)
#    output$plot_peds <- renderPlot(plotPeds(list(ped1,ped2), typed=list(input$cont1,input$cont2)))
# })



  # LR calculations tab --------------------

   #output the chosen parameters
   output$database <- DT::renderDT({
     datatable(cbind(c("theta","Silent allele freq.","Min. allele freq."),c(input$theta,input$silent, input$maf)),
               colnames = c("Parameter","Value"), rownames = FALSE,
               options = list(paging = FALSE, dom = "t"))
   })

   output$mutations <- DT::renderDT({
     dt <- cbind(c("Model","Female mutation rate","Male mutation rate"),c(input$mutModel,input$mutFem, input$mutMale))
     if(input$mutModel=="Stepwise") dt <- rbind(dt,c("Mutation range",input$range))
     datatable(dt, colnames = c("Parameter","Value"), rownames = FALSE,
               options = list(paging = FALSE, dom = "t"))
   })

   output$drop <- DT::renderDT({
     #Gather dropout/drop-in in a list
     cont <- unique(c(input$cont1,input$cont2))
     # Create a list to store the dropout values
     dropList <- sapply(cont, function(opt) {
       value <- input[[opt]]
       value
     })
    dt <- cbind(c(paste("Dropout",cont), "Drop-in"), c(dropList, input$dropin))
    datatable(dt, colnames = c("Parameter","Value"), rownames = FALSE,
               options = list(paging = FALSE, dom = "t"))
   })

   #Plot pedigrees
   #First convert FamiliasPedigree to ped
   output$plot3 <- renderPlot({
     req(pedigrees$ped1,pedigrees$ped2)
     ped1 <- pedFamilias::Familias2ped(pedigrees$ped1, datamatrix = NULL, loci = NULL)
     ped2 <- pedFamilias::Familias2ped(pedigrees$ped2, datamatrix = NULL, loci = NULL)
     plotPeds(list(ped1, ped2), typed=list(input$cont1, input$cont2))
     })


  #LR calculations
   observeEvent(input$LRbut, {

    req(datasets$mix,datasets$ref,datasets$db)
    req(pedigrees$ped1,pedigrees$ped2)
    req(input$cont1,input$cont2)
    req(input$mutModel, input$mutFem, input$mutMale, input$range)

    #Gather dropout/drop-in in a list
    cont <- unique(c(input$cont1,input$cont2))
    # Create a list to store the dropout values
    numericInputsList <- lapply(cont, function(opt) {
      value <- input[[opt]]
      value
    })
    names(numericInputsList) <- cont
    # Add the "Drop-in" value to the list
    numericInputsList[["dropin"]] <- input$dropin

    LRvalue <- calculateLR(datasets$mix,datasets$ref,datasets$db, list(pedigrees$ped1,pedigrees$ped2),
                           idxC1=input$cont1, idxC2=input$cont2,
                           drop=numericInputsList, theta=input$theta,
                           mutModel=input$mutModel, femaleMutationRate=input$mutFem,
                           maleMutationRate=input$mutMale, MutationRange=input$range)
    output$LR <- DT::renderDT({
      datatable(cbind(Marker=c(datasets$mix$Marker,"Total"),LR=round(c(LRvalue,prod(LRvalue)),2)), rownames = FALSE,
                options = list(scrollY = "300px", paging = FALSE, dom = "t"))
    })
   })


   # Reset ----------------

   #Reset all settings
   observeEvent(input$reset, {
     shinyjs::reset("dbform")
     shinyjs::reset("dataform")
     shinyjs::reset("pedform")

     # Clear data tables and plots
     output$preview_database <- DT::renderDT({
       datatable(data.frame())
     })
    output$preview_mixture <- DT::renderDT({
       datatable(data.frame())
     })
    output$preview_reference <- DT::renderDT({
       datatable(data.frame())
     })
    output$LR <- DT::renderDT({
      datatable(data.frame())
    })
    output$plot1 <- renderPlot({
      plot.new()
    })
    output$plot2 <- renderPlot({
      plot.new()
    })
    output$plot3 <- renderPlot({
      plot.new()
    })

   })

}




