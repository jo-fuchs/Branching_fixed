## Summary Function
SummarizeNeurons <- function(folderdir) {
  
  # create Results folder
  resultsdir <- file.path(folderdir, "Results")
  dir.create(resultsdir, showWarnings = FALSE)
  
  # read filelist
  folderlist <- list.files(path = folderdir, include.dirs = F, recursive = F, pattern = ".txt$") # pattern only includes txt-files
  
  
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
  
  
  # open results per cell
  for (i in 1:length(folderlist)) {
    
    ## open tracing measurements for individual neuron as temp
    temp <- read.table(paste(folderdir, "/", folderlist[i], sep = ""), header = TRUE, sep = "\t")
    
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
      if (temp$Type[k] == "Axon") {
        Axon[k] <- temp[k, 6]
      }
      else if (temp$Type[k] == "Primary") {
        Primary[k] <- temp[k, 6]
      }
      else if (temp$Type[k] == "Secondary") {
        Secondary[k] <- temp[k, 6]
      }
      else if (temp$Type[k] == "Tertiary") {
        Tertiary[k] <- temp[k, 6]
      }
      else if (temp$Type[k] == "Quartiary") {
        Quartiary[k] <- temp[k, 6]
      }
      else if (temp$Type[k] == "Dendrite") {
        Dendrite[k] <- temp[k, 6]
      }
      else if (temp$Type[k] == "Dendrite_primary") {
        Dendrite_primary[k] <- temp[k, 6]
      }
      else {
        Other[k] <- temp[k, 6]
      }
    }
    
    
    # Enter stats to table (somehow using column names instead of indices runs into error after second loop )
    
    Axon_results[i, 1] <- folderlist[i]                                                      # Image-Name
    Axon_results[i, 2] <- sum(Axon, Primary, Secondary, Tertiary, Quartiary, na.rm = TRUE)   # Total axon length
    Axon_results[i, 3] <- sum(Axon, na.rm = TRUE)                                            # Primary Axon length
    Axon_results[i, 4] <- sum(Primary, Secondary, Tertiary, Quartiary, na.rm = TRUE)         # Total Branch length
    Axon_results[i, 5] <- sum(Primary, na.rm = TRUE)                                         # Primary branch length
    Axon_results[i, 6] <- sum(Secondary, na.rm = TRUE)                                       # Secondary branch length
    Axon_results[i, 7] <- length(na.omit(Primary))                                           # Primary branch number
    Axon_results[i, 8] <- length(na.omit(Secondary))                                         # Secondary branch number
    Axon_results[i, 9] <- length(na.omit(Tertiary))                                          # Tertiary branch number
    Axon_results[i, 10] <- length(na.omit(Quartiary))                                        # Quartiary branch number
    Axon_results[i, 11] <- Axon_results[i, 7] / Axon_results[i, 3]                           # Primary branches / primary axon length
    Axon_results[i, 12] <- Axon_results[i, 8] / Axon_results[i, 5]                           # Secondary branches / primary branch length
    Axon_results[i, 13] <- Axon_results[i, 9] / Axon_results[i, 6]                           # Tertiary branches / secondary axon length
    Axon_results[i, 14] <- length(na.omit(Axon))                                             # Number of Axons
    Axon_results[i, 15] <- sum(Axon_results[i, 7:10])                                        # Total number of branches
    Axon_results[i, 16] <- Axon_results[i, 15] / Axon_results[i, 2]                          # Total branches / total axon length
    
    Dendrite_results[i, 1] <- folderlist[i]                                    # Image-Name
    Dendrite_results[i, 2] <- sum(Dendrite, Dendrite_primary, na.rm = TRUE)    # Total dendrite length
    Dendrite_results[i, 4] <- length(na.omit(Dendrite))                        # Dendrite number
    Dendrite_results[i, 3] <- Dendrite_results[i, 2] / Dendrite_results[i, 4]  # Mean dendrite length (including branches)
    Dendrite_results[i, 5] <- length(na.omit(Dendrite_primary))                # Total dendrite branch length
    Dendrite_results[i, 6] <- Dendrite_results[i, 5] / Dendrite_results[i, 4]  # Branche number / Dendrite number (would branches / dendrite length make more sense here?)
    Dendrite_results[i, 7] <- Axon_results[i, 2] + Dendrite_results[i, 2]      # Total neurite length (Total axon + Total dendrite)
    Dendrite_results[i, 8] <- Axon_results[i, 2] / Dendrite_results[i, 2]      # Axon to Dendrite ratio (Total axon / Total dendrite)
    
    Leftover[i, 1] <- folderlist[i]                                            # Image-Name
    Leftover[i, 2] <- length(na.omit(Other))                                   # Number of non-/ mis-classified processes
  }
  
  
  # seperate file name into Group & Image ID
  Axon_results <- separate(data = Axon_results, col = Image, into = c("Condition", "ID"), sep = "_", remove = F, extra = "merge")
  Dendrite_results <- separate(data = Dendrite_results, col = Image, into = c("Condition", "ID"), sep = "_", remove = F, extra = "merge")
  
  
  ## save results
  write.table(Axon_results, file.path(resultsdir, "Axon_results.txt"), sep = "\t", row.names = F)
  write.table(Dendrite_results, file.path(resultsdir, "Dendrite_results.txt"), sep = "\t", row.names = F)
  write.table(Leftover, file.path(resultsdir, "Misclassified.txt"), sep = "\t", row.names = F)
}
