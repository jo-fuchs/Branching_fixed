## Summary Function
SummarizeNeurons <- function(folderdir) {
  
  # the magic function: count & measure length of individual processes
  SummarizeIndividual <- function(temp, ID, input) {
    ## empty vectors for lengths in each Category
    Axon <- vector()
    Primary <- vector()
    Secondary <- vector()
    Tertiary <- vector()
    Quartiary <- vector()
    Dendrite <- vector()
    Dendrite_primary <- vector()
    Other <- vector()
    k <- 1                         # reset counter
    
    ## store lengths for each category in individual vector, store uncategorized in remaining
    for (k in 1:nrow(temp)) {
      # alternative option: switch-statement
      # switch(tolower(temp$Type[k]), 
      #        axon             = Axon[k] <- temp[k, 6],
      #        primary          = Primary[k] <- temp[k, 6],
      #        secondary        = Secondary[k] <- temp[k, 6],
      #        tertiary         = Tertiary[k] <- temp[k, 6],
      #        quartiary        = Quartiary[k] <- temp[k, 6],
      #        dendrite         = Dendrite[k] <- temp[k, 6],
      #        dendrite_primary = Dendrite_primary[k] <- temp[k, 6],
      #        ##leftovers
      #        Other[k] <- temp[k, 6]
      # )
      if (tolower(temp$Type[k]) == "axon") {
        Axon[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "primary") {
        Primary[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "secondary") {
        Secondary[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "tertiary") {
        Tertiary[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "quartiary") {
        Quartiary[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "dendrite") {
        Dendrite[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "dendrite_branch") {
        Dendrite_primary[k] <- temp[k, 6]
      }
      else {
        Other[k] <- temp[k, 6]
      }
    }
    
    
    # Enter stats to table (somehow using column names instead of indices runs into error after second loop )
    Axon_results <- input[[1]]
    j = nrow(Axon_results) + 1 # always put the new results in the last row
    
    Axon_results[j, 1] <- ID                                                    # Image-Name
    Axon_results[j, 2] <- sum(Axon, Primary, Secondary, Tertiary, Quartiary, na.rm = TRUE)   # Total axon length
    Axon_results[j, 3] <- sum(Axon, na.rm = TRUE)                                            # Primary Axon length
    Axon_results[j, 4] <- sum(Primary, Secondary, Tertiary, Quartiary, na.rm = TRUE)         # Total Branch length
    Axon_results[j, 5] <- sum(Primary, na.rm = TRUE)                                         # Primary branch length
    Axon_results[j, 6] <- sum(Secondary, na.rm = TRUE)                                       # Secondary branch length
    Axon_results[j, 7] <- length(na.omit(Primary))                                           # Primary branch number
    Axon_results[j, 8] <- length(na.omit(Secondary))                                         # Secondary branch number
    Axon_results[j, 9] <- length(na.omit(Tertiary))                                          # Tertiary branch number
    Axon_results[j, 10] <- length(na.omit(Quartiary))                                        # Quartiary branch number
    Axon_results[j, 11] <- Axon_results[j, 7] / Axon_results[j, 3]                           # Primary branches / primary axon length
    Axon_results[j, 12] <- Axon_results[j, 8] / Axon_results[j, 5]                           # Secondary branches / primary branch length
    Axon_results[j, 13] <- Axon_results[j, 9] / Axon_results[j, 6]                           # Tertiary branches / secondary axon length
    Axon_results[j, 14] <- length(na.omit(Axon))                                             # Number of Axons
    Axon_results[j, 15] <- sum(Axon_results[j, 7:10])                                        # Total number of branches
    Axon_results[j, 16] <- Axon_results[j, 15] / Axon_results[j, 2]                          # Total branches / total axon length
    
    Dendrite_results <- input[[2]]
    Dendrite_results[j, 1] <- ID                                                 # Image-Name
    Dendrite_results[j, 2] <- sum(Dendrite, Dendrite_primary, na.rm = TRUE)    # Total dendrite length
    Dendrite_results[j, 4] <- length(na.omit(Dendrite))                        # Dendrite number
    Dendrite_results[j, 3] <- Dendrite_results[j, 2] / Dendrite_results[j, 4]  # Mean dendrite length (including branches)
    Dendrite_results[j, 5] <- length(na.omit(Dendrite_primary))                # Total dendrite branch length
    Dendrite_results[j, 6] <- Dendrite_results[j, 5] / Dendrite_results[j, 4]  # Branche number / Dendrite number (would branches / dendrite length make more sense here?)
    Dendrite_results[j, 7] <- Axon_results[j, 2] + Dendrite_results[j, 2]      # Total neurite length (Total axon + Total dendrite)
    Dendrite_results[j, 8] <- Axon_results[j, 2] / Dendrite_results[j, 2]      # Axon to Dendrite ratio (Total axon / Total dendrite)
    
    
    Leftover <- input[[3]]
    Leftover[j, 1] <- ID                                                        # Image-Name
    Leftover[j, 2] <- length(na.omit(Other))                                    # Number of non-/ mis-classified processes
    
    if(Leftover[i, 2] > 0) {
      warning(paste(ID, ": some tracings have an unfamiliar type. Consider revising the NeuronJ tracings before you continue. \nSupported types are: Axon, Primary, Secondary, Tertiary, Quartiary, Neurite & Neurite_primary\n"))
    }
    
    result <- list(Axon_results = Axon_results, Dendrite_results = Dendrite_results, Leftover = Leftover)
    return(result)
  }
    
  
  # create Results folder
  resultsdir <- file.path(folderdir, "Results")
  dir.create(resultsdir, showWarnings = FALSE)
  
  # read filelist
  folderlist <- list.files(path = folderdir, include.dirs = F, recursive = F, pattern = ".txt$") # pattern only includes txt-files
  csv_marker = FALSE
  if (length(folderlist) == 0) { # NeuronJ or Fiji-Update 2020 saves results as .csv
    folderlist <- list.files(path = folderdir, include.dirs = F, recursive = F, pattern = ".csv$")
    csv_marker = TRUE
    if(length(folderlist) == 0) {
      stop("This folder doesn't seem to contain any NeuronJ measurements files")
      }
  }
  
  # define variable names in Results dataframes
  Axon_results <- data.frame(
    Image = NA, total_axon = NA, primary_axon = NA, total_branch_length = NA, total_primary_length = NA, total_secondary_length = NA,
    Number_primary = NA, Number_secondary = NA, Number_tertiary = NA, Number_quartiary = NA, Primary_branches_by_primary_axon = NA, 
    Secondary_branches_by_primary_length = NA, Tertiary_branches_by_secondary_length = NA, Number_axons = NA, Total_branches = NA, 
    Total_branches_by_total_length = NA
  ) [numeric(0), ]
  
  Dendrite_results <- data.frame(
    Image = NA, total_dendrite = NA, mean_dendrite = NA, Number_dendrites = NA, Number_dendrite_branches = NA,
    Branches_per_dendrite = NA, total_neurite_length = NA, axon_by_dendrite = NA
  )[numeric(0), ]
  
  Leftover <- data.frame(Image = NA, Number_others = NA) [numeric(0), ]
  
  # combine dataframes to pass it into SummarizeIndividual function
  inputlist <- list(Axon_results = Axon_results, Dendrite_results = Dendrite_results, Leftover = Leftover)
  
  
  # open results per cell
  for (i in 1:length(folderlist)) {
   temp = NULL
    # if loading doesn't work, skip it
    fail <- tryCatch(error = function(cnd) {
        warning(paste(folderlist[i], "could not be read:", cnd$message))
        cnd
      },
      
      # load as csv or txt depending on which files are detected in folder
      if(csv_marker) {
        temp <- read.table(paste(folderdir, "/", folderlist[i], sep = ""), header = TRUE, sep = ",")
      } else {
        temp <- read.table(paste(folderdir, "/", folderlist[i], sep = ""), header = TRUE, sep = "\t")
      }
      )
      
    if(inherits(fail, "error")) next
    
    # Test for correct format of csv-files > else skip this file
    else if(is.null(temp$Type)) {  
    Axon_results[i, 1] <- folderlist[i]   
    Dendrite_results[i, 1] <- folderlist[i]      
    Leftover[i, 1] <- folderlist[i]
    warning(paste(folderlist[i], "seems to have a different structure than NeuronJ Tracing measurements.\n"))
    next
    }
    
    # Handling multiple cells at a time
    # Splitting neurons by cluster (if there are any)
    if(length(unique(temp$Cluster)) > 1) {
      list <- split(temp, temp$Cluster) 
      
      # go through every cell individually
      for (cluster in list){
        ID <- paste(folderlist[i], cluster$Cluster[1], sep = "_") # The Cluster gives the name
        inputlist <- SummarizeIndividual(cluster, ID, inputlist)
      }
    } else if (length(unique(temp$Cluster)) == 1) {
      ID <- paste(folderlist[i], temp$Cluster[1], sep = "_") # The Cluster gives the name
      inputlist <- SummarizeIndividual(temp, ID, inputlist)
    }
    
  }
  

  
  # # separate file name into Group & Image ID
  # Axon_results <- separate(data = Axon_results, col = Image, into = c("Condition", "ID"), sep = "_", remove = F, extra = "merge")
  # Dendrite_results <- separate(data = Dendrite_results, col = Image, into = c("Condition", "ID"), sep = "_", remove = F, extra = "merge")
  # 
  
  ## save results
  write.table(inputlist$Axon_results, file.path(resultsdir, "Axon_results.txt"), sep = "\t", row.names = F)
  write.table(inputlist$Dendrite_results, file.path(resultsdir, "Dendrite_results.txt"), sep = "\t", row.names = F)
  write.table(inputlist$Leftover, file.path(resultsdir, "Misclassified.txt"), sep = "\t", row.names = F)
  message("Success: All neurons have been analysed and Results have been saved\n")
  }
