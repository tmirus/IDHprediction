library(shiny)


ui <- fluidPage(
  ## Input
  # Main title of the app
  shiny::titlePanel(h1(strong("Prediction of the IDH mutation status"), align = "center"), windowTitle = "IDHprediction"),
  shiny::titlePanel(h4("NOTE: This application is for research purposes only", align = "center")),

  # Settings of the user
  wellPanel(
    titlePanel(h2("Settings")),
    fluidRow(
      column(5,
             numericInput("frequency", label = "Bandwidth in Hz:", value = 1200, min = 100, max = 10000),
             numericInput("magneticField", label = "Main magnetic field in T:", value = 3, min = 1, max = 10, step = 0.5),
             fileInput("spectrum", "Choose csv file of spectrum:", accept = ".csv", buttonLabel = "Browse...", placeholder = "No file selected")
             ),
      column(
        width = 7, 
        h2(strong("Predictions: ")), style = "text-align: center; color:black;",
        DT::dataTableOutput("prediction")
      )
    ),
    br(),
    actionButton("button", "Help", style = "color:gray"),
    actionButton("button1", "License", style = "color:gray"),
    actionButton("button2", "Privacy Policy", style = "color:gray"),
    h5(div(verbatimTextOutput("button"), style = "color:gray")),
    h4(div(textOutput("warning"), style = "color:red"))
  ),
  ## Output
  fluidRow(
    column(2, uiOutput("spectrumChoice")),
    column(10, plotOutput("spec",width = "90%", height = "800px"), align = 'center')),
  br(),
  fluidRow(
    column(3, h3(strong("Prediction: ")), style = "text-align: center;"),
    column(9, h3(strong(textOutput("result"))), style = "text-align: left;")
  ),
  br(),
  fluidRow(
    column(12, textOutput("footer"))
  )
)

server <- function(input, output) {
  # load model
  model_file <- system.file("trained_model.rda", package = "IDHprediction")
  load(model_file)
  reference_file <- system.file("reference_position.rds", package = "IDHprediction")
  ref_pos <- readRDS(reference_file)

  # training data parameters
  magnetic_field_train <- 3
  frequency_train <- 1200
  dp_train <- 1024
  
  display_help <- FALSE
  display_license <- FALSE
  display_policy <- FALSE

  # Print output of help button
  observeEvent(input$button, {
    if (!display_help){
    output$button <- renderText({
"The csv file has to consist of two rows. The first row is the header with the numeration, the second row contains the NMR-values.
However the first column shows the sample name or ID. The decimal separator is a dot, values are separated by a semicolon. Sample names/IDs must be unique.\n
For the prediction of the IDH mutation status a linear support vector machine (SVM) was trained on data from Regensburg.
These spectra are generated with a bandwidth of 1200 Hz and a main magnetic field of 3 T. The points in FID are 1024.
In advance a t-test based feature selection of the spectra of Regensburg was done to determine a feature selection for
the training of the support vector machine (SVM). To predict the IDH mutation status of a new test spectrum the settings
(bandwidth, main magnetic field, points in FID) could differ. Therefore some preprocessing steps are necessary. Concerning
the used bandwidth and main magnetic field of the train data the test spectrum will be cropped/extended to get the same
spectrum range. In addition the points in FID will be matched to 1024. The test spectrum is then shifted according to the
maximum choline (Cho) value of the train spectra and the residual-water signal and empty regions will removed. Afterwards
a normalization and log transformation of the spectrum is done.\n
Note that the result should only be trusted if the spectrum used for prediction looks of acceptable quality.
A plot comparing the used spectrum against an average over all training spectra can be seen below."
      })
    display_help <<- TRUE
    display_license <<- FALSE
    display_policy <<- FALSE
    } else {
      output$button <- renderText({""})
      display_help <<- FALSE
      display_policy <<- FALSE
      display_license <<- FALSE
    }
  })
  
  observeEvent(input$button1, {
    if (!display_license) {
    output$button <- renderText({
    "IDHprediction - Prediction of IDH mutation status from MRS spectra
    Copyright (C) 2022  Tim Mirus

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>."
    })
    display_license <<- TRUE
    display_help <<-FALSE
    display_policy <<- FALSE
    } else {
      output$button <- renderText({""})
      display_license <<- FALSE
      display_help <<- FALSE
      display_policy <<- FALSE
    }
  })
  
  observeEvent(input$button2, {
    if (!display_policy)
    {
      output$button <- renderText({
        "Privacy:
          Cookies
            This website does not use cookies.
          Server Log
            The web server keeps a log of all requests, with the following data:

              The request IP adress
              Date and Time of the request
              request type and path
              the User-Agent of the web browser 

            This data is only used to diagnose tecnical problems.

          Web Analytics / Other Tracking
            There are no other tracking methods.
        
        Data usage:
          Uploaded data is only used within the active R session. 
          It is not stored and does not remain on the server."
      })
      display_policy <<- TRUE
      display_license <<- FALSE
      display_help <<- FALSE
    } else {
      output$button <- renderText({""})
      display_license <<- FALSE
      display_help <<- FALSE
      display_policy <<- FALSE
    }
  })

  load_data <- reactive({
    # Browse information
    # Save infos of user input
    file <- input$spectrum
    # Save file format
    ext <- tools::file_ext(file$datapath)
    req(file)
    # Check if file format equals csv
    validate(need(ext == "csv", "Please upload a csv file"))
    # Save file values
    data<-try({read.csv2(file$datapath, header=TRUE,sep=";",quote="\"",dec=".",fill=TRUE,comment.char="", row.names=1)}, silent = TRUE)
    if (class(data) == "try-error"){
      output$warning <- renderText({"ERROR: Invalid Input. Check CSV file for campatibility (See help)."})
      return(NULL)
    }
    data <- as.matrix(data)
    
    if (ncol(data) < 2)
    {
      output$warning <- renderText({
        "ERROR: Invalid input. Data contains less than two columns. Please check file content and compatibility."
      })
      return(NULL)
    }
    if (!is.numeric(data[1,1]))
    {
      output$warning <- renderText({
        "ERROR: Invalid input. Data is not numeric. Please check file content and compatibility."
      })
      return(NULL)
    }
    if (nrow(data) < 1)
    {
      output$warning <- renderText({
        "ERROR: Invalid input. Too few rows. Please check file content and compatibility."
      })
      return(NULL)
    }

    # Save info of user input of parameters
    frequency_test <- input$frequency
    magneticField_value <- input$magneticField
    if (frequency_test == 0 || magneticField_value == 0 )
    {
      output$warning <- renderText({
        "ERROR: Invalid parameter(s)."
      })
      return(NULL)
    }
    numberOfPoints_test <- ncol(data)

    # calculate frequency based on magnetic field strength
    magneticField_difference <- magnetic_field_train/magneticField_value
    new_frequency <- frequency_test * magneticField_difference

    if(new_frequency < frequency_train){
      output$warning <- renderText({"WARNING: test bandwidth is smaller than train bandwidth!"})
    }
    output$warning <- renderText({""})
    return(data)
  })


  processed_data <- reactive({
    data <- load_data()
    if (is.null(data))
      return(NULL)
    
    sampleNames <- rownames(data)
    
    # Preprocessing
    data <- as.matrix(data)

    # Save info of user input of parameters
    frequency_test <- input$frequency
    magneticField_value <- input$magneticField
    numberOfPoints_test <- ncol(data)

    # adjust for technical differences
    data_spread <- IDHprediction:::adjust_positions(
      data,
      magnetic_field_train, magneticField_value,
      frequency_train, frequency_test,
      dp_train, numberOfPoints_test
    )

    # remove negative values (in log space)
    data_spread[data_spread < 1] <- 1

    # shift to choline peak
    shifted_data <- IDHprediction:::align_spectra(data_spread, 345:370, ref_pos)


    # Removal of residual-water signal and empty regions:
    # Empty regions: value of columns 1 to 89 and 451 to 1014 is near to zero
    # Residual-water signal: sharp peak in region of column 500
    shifted_data <- shifted_data[, 90:450, drop = FALSE]

    # normalize each spectrum to 1e6
    factors <- rowSums(shifted_data) / 1e6
    shifted_data <- sweep(shifted_data, 1, factors, "/")
    normed_data <- log2(shifted_data)

    # data binning
    test_data <- IDHprediction:::bin_spectra(normed_data, bin_size = 4)
    rownames(test_data) <- sampleNames
    return(test_data)
  })

  plot_data <- reactive({
    input$frequency
    input$magneticField
    sampleName <- input$spectrumList
    spectra <- isolate(processed_data())
    if (is.null(spectra))
    {
      return(NULL)
    }
    if (!sampleName %in% rownames(spectra))
    {
      return(NULL)
    }
    if (ncol(spectra) == length(avg_train_spectrum_wt))
    {
      title = paste(isolate(input$spectrum$name), "(preprocessed)", sep  = " ")
      plot_df <- data.frame(
        x = 1:ncol(spectra),
        Signal = c(avg_train_spectrum_wt, avg_train_spectrum_mut, spectra[sampleName,]), 
        Spectrum = c(
          rep("training average wildtype", length(avg_train_spectrum_wt)), 
          rep("training average mutation", length(avg_train_spectrum_mut)),
          rep(sampleName, ncol(spectra))
          )
      )
      p1 <- ggplot(plot_df, aes(x = x)) +
        geom_line(aes(y = Signal, group = Spectrum, col = Spectrum)) +
        ggtitle(title) +
        xlab("") +
        ylab("log-transformed intensitiy") +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(size = 12)
        )
      for (f in svm_list$features)
      {
        p1 <- p1 + geom_vline(xintercept = f, col = "black", alpha = 0.5, linetype = "dashed")
      }
      x_breaks <- seq(10, ncol(spectra), 10)
      p1 <- p1 + scale_x_reverse(breaks = x_breaks, labels = x_breaks)
      return(p1)
    }
  })

  predict_labels <- reactive({
    label <- ""
    if (is.null(processed_data()))
    {
      return(NULL)
    }
    if (ncol(processed_data()) != length(avg_train_spectrum_wt))
    {
      output$warning <- renderTExt({"ERROR: Number of columns in training data and new data do not match."})
      label <- NULL
    } else {
      # End of preprocessing
      # prediction
      prediction <- predict(object = svm_list$model, newdata = processed_data()[, svm_list$features, drop = FALSE])
      sampleNames <- rownames(processed_data())
      prediction <- ifelse(as.numeric(as.character(prediction)) == 0, "WILDTYPE", "MUTATION")
      label <- data.frame(sampleName = sampleNames, Prediction = prediction)
    }
    return(label)
  })
  
  spectrum_list <- reactive({
    l <- as.list(rownames(processed_data()))
    names(l) <- rownames(processed_data())
    return(l)
  })

  output$spec <- renderPlot({
    plot_data()
  })

  output$prediction <- DT::renderDataTable({
    DT::datatable(
      data = predict_labels(), 
      rownames = FALSE, fillContainer = FALSE, 
      options = list(
        pageLength = 5,
        columnDefs = list(list(className = 'dt-center', targets = 0:1))
      ))
  })
  
  output$spectrumChoice <- renderUI({
    shiny::selectInput("spectrumList", "Choose Spectrum", choices = spectrum_list())
  })
  
  output$result <- renderText({
    predict_labels()[which(predict_labels()[,"sampleName"] == input$spectrumList), "Prediction"]
  })
  
  output$footer <- renderText({
    "2022        Version 1.1.2"
  })
}


#' Run Shiny Application for prediction
#'
#' This starts the shiny app.
#' @param port the port on which the server is run, default 8080
#' @param launch.browser bool, should default browser be launched to display the app? default TRUE
#' @return NULL
#' @export

runWebApp <- function(port = 8080, launch.browser = TRUE)
{
  # packages and dependencies
  require(ggplot2, quietly = TRUE)
  require(e1071, quietly = TRUE)
  shiny::shinyApp(
		  ui = ui, 
		  server = server,
  		  options = list(
		   port = port,
		   launch.browser = launch.browser,
		   host = "0.0.0.0",
	           quiet = TRUE,
	           display.mode = "normal"	   
		  )
  )
}

