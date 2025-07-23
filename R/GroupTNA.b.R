# This file is a generated template, your changes will not be overwritten

GroupTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "GroupTNAClass",
  inherit = GroupTNABase,
  private = list(
    .run = function() {
      library("tna")

      type <- self$options$buildModel_type
      scaling <- self$options$buildModel_scaling

      ### Build Model

      # Check if required variables are provided, if not, hide error and return early
      if(is.null(self$options$buildModel_variables_long_action) || 
         is.null(self$options$buildModel_variables_long_actor) ||
         is.null(self$options$buildModel_variables_long_group)) {
        self$results$errorText$setVisible(FALSE)
        return()
      }

      model <- NULL

      if(!isTRUE(self$options$buildModel_show_matrix) &&
        !isTRUE(self$options$buildModel_show_plot) &&
        !isTRUE(self$options$buildModel_show_histo) &&
        (
          !isTRUE(self$options$buildModel_show_mosaic) ||
          (
            isTRUE(self$options$buildModel_show_mosaic) && 
            self$options$buildModel_type == "relative"
          )
        )
      )
      {
        self$results$buildModelTitle$setVisible(FALSE)
      }

      if(self$results$buildModelContent$isFilled()) {
        model <- self$results$buildModelContent$state
      }
      else if(!is.null(self$data) && ncol(self$data) >= 1) {

        # Wrap data preparation in error handling
        tryCatch({
          dataForTNA <- NULL

          copyData <- self$data
          copyData[[self$options$buildModel_variables_long_action]] <- as.character(copyData[[self$options$buildModel_variables_long_action]])

          if(!is.null(self$options$buildModel_variables_long_time)) {
            copyData[[self$options$buildModel_variables_long_time]] <- as.POSIXct(copyData[[self$options$buildModel_variables_long_time]])
          }
          if(!is.null(self$options$buildModel_variables_long_actor)) {
            copyData[[self$options$buildModel_variables_long_actor]] <- as.character(copyData[[self$options$buildModel_variables_long_actor]])
          }
          if(!is.null(self$options$buildModel_variables_long_order)) {
            copyData[[self$options$buildModel_variables_long_order]] <- as.character(copyData[[self$options$buildModel_variables_long_order]])
          }

          
          threshold <- self$options$buildModel_threshold

          columnToUseLong <- c(
            self$options$buildModel_variables_long_time,
            self$options$buildModel_variables_long_actor,
            self$options$buildModel_variables_long_action,
            self$options$buildModel_variables_long_group,
            self$options$buildModel_variables_long_order
          )

          longData <- copyData[columnToUseLong]

          if(ncol(longData) > 0) {
            actorColumn <- self$options$buildModel_variables_long_actor
            timeColumn <- self$options$buildModel_variables_long_time
            actionColumn <- self$options$buildModel_variables_long_action
            groupColumn <- self$options$buildModel_variables_long_group
            orderColumn <- self$options$buildModel_variables_long_order

            ##### TO REMOVE
            values_to_replace <- c("Applications", "Ethics", "General", "La_types", "Theory")
            new_value <- "Resources"
            longData[[actionColumn]] <- replace(
                longData[[actionColumn]], 
                longData[[actionColumn]] %in% values_to_replace, 
                new_value
            )
            ##### END TO REMOVE

            args_prepare_data <- list(
                data = longData,
                actor = actorColumn,
                time = timeColumn,
                action = actionColumn,
                time_threshold = threshold,
                order = orderColumn
            ) 

            args_prepare_data <- args_prepare_data[!sapply(args_prepare_data, is.null)]

            dataForTNA <- do.call(tna::prepare_data, args_prepare_data)
          }

          if(!is.null(dataForTNA)) {

              if(scaling == "noScaling") {
                  scaling = character(0L)
              }

              group <- dataForTNA$long_data[!duplicated(dataForTNA$long_data$.session_id),]

              model <- tna::group_model(x=dataForTNA, group=group[[groupColumn]], type=type, scaling=scaling)   
          }
          
        }, error = function(e) {
          # Check if error is related to time format or contains time-related keywords
          error_msg <- tolower(as.character(e$message))
          if(grepl("time|date|posix|format", error_msg) || 
             grepl("character string is not in a standard unambiguous format", error_msg)) {
              self$results$errorText$setContent("Please enter an appropriate time format")
          } else {
              self$results$errorText$setContent(paste("Data preparation error:", e$message))
          }
          self$results$errorText$setVisible(TRUE)
          return()
        })
      }

      if(!is.null(model)) { 
          
          if(!self$results$buildModelContent$isFilled()) {
              self$results$buildModelContent$setContent(model)
              self$results$buildModelContent$setState(model)
          }
          self$results$buildModelContent$setVisible(self$options$buildModel_show_matrix)

          # model
          self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot) # plot
          

          # # histo
          self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo) # plot
          
          # # frequencies
          self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies) # plot
          

          # # mosaic
          self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic ) # plot

      }

      ### Centrality

      if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot)) {
          centrality_loops <- self$options$centrality_loops
          centrality_normalize <- self$options$centrality_normalize

          # Build vectorCharacter based on selected options (always needed)
          vectorCharacter <- character(0)
          if(self$options$centrality_OutStrength) {
              vectorCharacter <- append(vectorCharacter, "OutStrength")
          }
          if(self$options$centrality_InStrength) {
              vectorCharacter <- append(vectorCharacter, "InStrength")
          }
          if(self$options$centrality_ClosenessIn) {
              vectorCharacter <- append(vectorCharacter, "ClosenessIn")
          }
          if(self$options$centrality_ClosenessOut) {
              vectorCharacter <- append(vectorCharacter, "ClosenessOut")
          }
          if(self$options$centrality_Closeness) {
              vectorCharacter <- append(vectorCharacter, "Closeness")
          }
          if(self$options$centrality_Betweenness) {
              vectorCharacter <- append(vectorCharacter, "Betweenness")
          }
          if(self$options$centrality_BetweennessRSP) {
              vectorCharacter <- append(vectorCharacter, "BetweennessRSP")
          }
          if(self$options$centrality_Diffusion) {
              vectorCharacter <- append(vectorCharacter, "Diffusion")
          }
          if(self$options$centrality_Clustering) {
              vectorCharacter <- append(vectorCharacter, "Clustering")
          }

          # Use state to store centrality results and avoid recalculation, but allow recalculation when needed
          cent <- self$results$centralityTable$state
          
          if(length(vectorCharacter) > 0 && !is.null(model)) {
              # Only calculate if not already filled or if explicitly requested
              if(is.null(cent) || !self$results$centralityTable$isFilled()) {
                  tryCatch({
                      cent_result <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
                      
                      # Validate the result before storing
                      if(!is.null(cent_result) && is.data.frame(cent_result)) {
                          cent <- cent_result
                          self$results$centralityTable$setState(cent)
                      } else {
                          self$results$centralityTable$setNote(key = "centrality_error", note = "Invalid centrality calculation result")
                      }
                  }, error = function(e) {
                      # Add error handling for centrality calculation
                      self$results$centralityTable$setNote(key = "centrality_error", note = paste("Centrality calculation error:", e$message))
                      cent <- NULL
                  })
              }
          }

          # Add columns if needed (jamovi will handle duplicates)
          self$results$centralityTable$addColumn(name="group", type="text")
          self$results$centralityTable$addColumn(name="state", type="text")

          if(self$options$centrality_OutStrength) {
              self$results$centralityTable$addColumn(name="OutStrength", type="number")
          }
          if(self$options$centrality_InStrength) {
              self$results$centralityTable$addColumn(name="InStrength", type="number")
          }
          if(self$options$centrality_ClosenessIn) {
              self$results$centralityTable$addColumn(name="ClosenessIn", type="number")
          }
          if(self$options$centrality_ClosenessOut) {
              self$results$centralityTable$addColumn(name="ClosenessOut", type="number")
          }
          if(self$options$centrality_Closeness) {
              self$results$centralityTable$addColumn(name="Closeness", type="number")
          }
          if(self$options$centrality_Betweenness) {
              self$results$centralityTable$addColumn(name="Betweenness", type="integer")
          }
          if(self$options$centrality_BetweennessRSP) {
              self$results$centralityTable$addColumn(name="BetweennessRSP", type="number")
          }
          if(self$options$centrality_Diffusion) {
              self$results$centralityTable$addColumn(name="Diffusion", type="number")
          }
          if(self$options$centrality_Clustering) {
              self$results$centralityTable$addColumn(name="Clustering", type="number")
          }

          # Populate table if we have data (either from state or newly calculated)
          if(!is.null(cent) && length(vectorCharacter) > 0) {
              # Handle Group TNA centrality data structure
              row_count <- 1
              
              # GroupTNA centralities returns a data frame with columns: group, state, measures...
              if(is.data.frame(cent) && !is.null(cent) && !is.na(nrow(cent)) && nrow(cent) > 0) {
                  for (i in 1:nrow(cent)) {
                      rowValues <- list()
                      
                      # Extract values directly from data frame columns
                      rowValues$group <- as.character(cent[i, "group"])
                      rowValues$state <- as.character(cent[i, "state"])

                      if ("OutStrength" %in% vectorCharacter && "OutStrength" %in% colnames(cent)) {
                          rowValues$OutStrength <- as.numeric(cent[i, "OutStrength"])
                      }
                      if ("InStrength" %in% vectorCharacter && "InStrength" %in% colnames(cent)) {
                          rowValues$InStrength <- as.numeric(cent[i, "InStrength"])
                      }
                      if ("ClosenessIn" %in% vectorCharacter && "ClosenessIn" %in% colnames(cent)) {
                          rowValues$ClosenessIn <- as.numeric(cent[i, "ClosenessIn"])
                      }
                      if ("ClosenessOut" %in% vectorCharacter && "ClosenessOut" %in% colnames(cent)) {
                          rowValues$ClosenessOut <- as.numeric(cent[i, "ClosenessOut"])
                      }
                      if ("Closeness" %in% vectorCharacter && "Closeness" %in% colnames(cent)) {
                          rowValues$Closeness <- as.numeric(cent[i, "Closeness"])
                      }
                      if ("Betweenness" %in% vectorCharacter && "Betweenness" %in% colnames(cent)) {
                          rowValues$Betweenness <- as.numeric(cent[i, "Betweenness"])
                      }
                      if ("BetweennessRSP" %in% vectorCharacter && "BetweennessRSP" %in% colnames(cent)) {
                          rowValues$BetweennessRSP <- as.numeric(cent[i, "BetweennessRSP"])
                      }
                      if ("Diffusion" %in% vectorCharacter && "Diffusion" %in% colnames(cent)) {
                          rowValues$Diffusion <- as.numeric(cent[i, "Diffusion"])
                      }
                      if ("Clustering" %in% vectorCharacter && "Clustering" %in% colnames(cent)) {
                          rowValues$Clustering <- as.numeric(cent[i, "Clustering"])
                      }
                      
                      self$results$centralityTable$addRow(rowKey=row_count, values=rowValues)
                      row_count <- row_count + 1
                  }
              }
              
              # Add informational note if no rows were added
              if(row_count == 1) {
                  self$results$centralityTable$setNote(key = "centrality_info", note = "No centrality data available. Please check your data and settings.")
              }
          } else {
              # Add informational note if conditions not met
              if(is.null(cent)) {
                  self$results$centralityTable$setNote(key = "centrality_info", note = "Centrality calculation not performed.")
              } else if(length(vectorCharacter) == 0) {
                  self$results$centralityTable$setNote(key = "centrality_info", note = "Please select at least one centrality measure to display results.")
              }
          }
          
          # Set visibility based on user selection
          self$results$centralityTitle$setVisible(self$options$centrality_show_table || self$options$centrality_show_plot)
          self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
          self$results$centralityTable$setVisible(self$options$centrality_show_table)
      }


      ### Community
      if(!is.null(model) && isTRUE(self$options$community_show_plot) ) {
        community_gamma <- as.numeric(self$options$community_gamma)
        methods <- self$options$community_methods

        # Use state to avoid recalculating community detection, but allow recalculation when needed
        coms <- self$results$community_plot$state
        if(is.null(coms)) {
          resultComs <- tryCatch({
            coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
            
            # Store state to avoid recalculation
            self$results$community_plot$setState(coms)
            # Community table disabled for GroupTNA
            # if(isTRUE(self$options$community_show_table)) {
            #     self$results$communityContent$setContent(coms)
            # }
            TRUE
          }, error = function(e) {
            self$results$communityTitle$setVisible(TRUE)
            self$results$communityErrorText$setContent(paste("The methods", methods, "should be change :\n\t", conditionMessage(e)) )
            self$results$communityErrorText$setVisible(TRUE)
            FALSE
          })
          
          if(!resultComs) return()
        }

        # Set visibility
        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityContent$setVisible(FALSE)  # Hide community table in GroupTNA
        self$results$communityTitle$setVisible(isTRUE(self$options$community_show_plot))
      }

      ### Cliques

      cliques_size <- as.numeric(self$options$cliques_size)
      cliques_threshold <- as.numeric(self$options$cliques_threshold)

      if(!is.null(model) && (isTRUE(self$options$cliques_show_text) || isTRUE(self$options$cliques_show_plot)) ) {

          # Use state to avoid recalculating expensive cliques analysis, but allow recalculation when needed
          cliques <- self$results$cliques_multiple_plot$state
          if(is.null(cliques)) {
              cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)

              # Store state to avoid recalculation
              self$results$cliques_multiple_plot$setState(cliques)
              
              if(isTRUE(self$options$cliques_show_text)) {
                self$results$cliquesContent$setContent(cliques)
              }
          }
          
          # Set visibility
          self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
          self$results$cliquesContent$setVisible(self$options$cliques_show_text)
          self$results$cliquesTitle$setVisible(isTRUE(self$options$cliques_show_text) || isTRUE(self$options$cliques_show_plot))
      }

      ### Bootstrap

      if(!is.null(model) && (isTRUE(self$options$bootstrap_show_table) || isTRUE(self$options$bootstrap_show_plot))) {
        
        # Use state to avoid recalculating expensive bootstrap, but allow recalculation when needed
        bs <- self$results$bootstrap_plot$state
        if(is.null(bs)) {
          iteration <- self$options$bootstrap_iteration
          level <- self$options$bootstrap_level
          method <- self$options$bootstrap_method

          range_low <- self$options$bootstrap_range_low
          range_up <- self$options$bootstrap_range_up

          threshold <- self$options$bootstrap_threshold

          bs <- tna::bootstrap(
            x=model, 
            iter=iteration,
            level=level,
            method=method,
            threshold=threshold,
            consistency_range=c(range_low, range_up)
          )

          # Store state to avoid recalculation
          self$results$bootstrap_plot$setState(bs)
        }

        # Populate table if we have data and table option is enabled
        if(!is.null(bs) && isTRUE(self$options$bootstrap_show_table)) {
          row_key_counter <- 1
          
          # Debug: Print structure to understand data format
          tryCatch({
            # Iterate through each group (A, B, C, etc.)
            for (group_name in names(bs)) {
              group_data <- bs[[group_name]]
              
              # Check if group has summary data
              if (!is.null(group_data) && !is.null(group_data$summary)) {
                summary_data <- group_data$summary
                
                # Check if summary_data has rows
                if (is.data.frame(summary_data) && !is.null(summary_data) && !is.na(nrow(summary_data)) && nrow(summary_data) > 0) {
                  # Sort by significance (significant first), then by p-value
                  sorted_summary <- summary_data[order(-summary_data$sig, summary_data$p_value), ]
                  
                  # Add each edge to the table
                  for (i in 1:nrow(sorted_summary)) {
                    rowValues <- list(
                      group = group_name,
                      from = as.character(sorted_summary[i, "from"]),
                      to = as.character(sorted_summary[i, "to"]),
                      weight = as.numeric(sorted_summary[i, "weight"]),
                      p_value = as.numeric(sorted_summary[i, "p_value"]),
                      cr_lower = as.numeric(sorted_summary[i, "cr_lower"]),
                      cr_upper = as.numeric(sorted_summary[i, "cr_upper"]),
                      ci_lower = as.numeric(sorted_summary[i, "ci_lower"]),
                      ci_upper = as.numeric(sorted_summary[i, "ci_upper"]),
                      significant = ifelse(sorted_summary[i, "sig"], "Yes", "No")
                    )
                    self$results$bootstrapTable$addRow(rowKey=as.character(row_key_counter), values=rowValues)
                    row_key_counter <- row_key_counter + 1
                  }
                }
              }
            }
            
            # If no data was added, add a debug message
            if (row_key_counter == 1) {
              self$results$bootstrapTable$setNote(key = "bootstrap_debug", note = "No bootstrap results found. Check that bootstrap analysis completed successfully.")
            }
            
          }, error = function(e) {
            # Error handling - show structure info
            self$results$bootstrapTable$setNote(key = "bootstrap_error", note = paste("Bootstrap table error:", e$message))
          })
        }
        
        self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
        self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
        self$results$bootstrapTitle$setVisible(isTRUE(self$options$bootstrap_show_plot) || isTRUE(self$options$bootstrap_show_table))

      }



      ## Permutation

      if(!is.null(model) && (isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))) {
        
        # Use state to avoid recalculating expensive permutation test, but allow recalculation when needed
        permutationTest <- self$results$permutation_plot$state
        if(is.null(permutationTest)) {
          permutationTest <- tna::permutation_test(
            x=model, 
            iter=self$options$permutation_iter, 
            paired=self$options$permutation_paired,
            level=self$options$permutation_level
          )

          # Store state to avoid recalculation
          self$results$permutation_plot$setState(permutationTest)
        }

        # Populate table if we have data and table option is enabled
        if (!is.null(permutationTest) && isTRUE(self$options$permutation_show_text)) {
          row_key_counter <- 1
          for (comparison_name in names(permutationTest)) {
            comparison_data <- permutationTest[[comparison_name]]
            if (!is.null(comparison_data$edges) && !is.null(comparison_data$edges$stats)) {
              stats_df <- comparison_data$edges$stats

              # Handle NAs and ensure numeric types
              stats_df$diff_true <- as.numeric(stats_df$diff_true)
              stats_df$effect_size <- as.numeric(stats_df$effect_size)
              stats_df$p_value <- as.numeric(stats_df$p_value)

              # Sort by p_value and then by diff_true (descending)
              filtered_sorted_stats <- stats_df[order(stats_df$p_value, -stats_df$diff_true), ]
              
              # Add debug info about edge names
              if("edge_name" %in% colnames(filtered_sorted_stats)) {
                sample_edges <- head(filtered_sorted_stats$edge_name, 3)
                # Also check character encoding and length
                debug_info <- paste("Found", nrow(filtered_sorted_stats), "edges. Sample:", 
                                  paste(sample_edges, collapse="; "),
                                  ". Lengths:", paste(nchar(sample_edges), collapse=","))
                self$results$permutationContent$setNote(key = "edge_debug", note = debug_info)
              } else {
                available_cols <- paste(colnames(filtered_sorted_stats), collapse=", ")
                self$results$permutationContent$setNote(key = "edge_debug", note = paste("No edge_name column. Available:", available_cols))
              }

              for (i in 1:nrow(filtered_sorted_stats)) {
                # Get edge name with robust handling
                edge_name_value <- filtered_sorted_stats[i, "edge_name"]
                
                # Handle various edge name formats and potential issues
                if(is.null(edge_name_value) || is.na(edge_name_value) || edge_name_value == "" || edge_name_value == " -> ") {
                  # Try to construct from row name or use fallback
                  edge_name_value <- rownames(filtered_sorted_stats)[i]
                  if(is.null(edge_name_value) || edge_name_value == "") {
                    edge_name_value <- paste("Edge", i)
                  }
                } else {
                  # Clean the edge name and ensure it's properly formatted
                  edge_name_value <- trimws(as.character(edge_name_value))
                  # Replace any problematic characters that might cause display issues
                  edge_name_value <- gsub("[\u00A0\u2013\u2014]", "->", edge_name_value)
                }
                
                rowValues <- list(
                  group_comparison = comparison_name,
                  edge_name = edge_name_value,
                  diff_true = as.numeric(filtered_sorted_stats[i, "diff_true"]),
                  effect_size = as.numeric(filtered_sorted_stats[i, "effect_size"]),
                  p_value = as.numeric(filtered_sorted_stats[i, "p_value"])
                )
                self$results$permutationContent$addRow(rowKey=as.character(row_key_counter), values=rowValues)
                row_key_counter <- row_key_counter + 1
              }
            }
          }
        }
        # Set visibility
        self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
        self$results$permutationContent$setVisible(self$options$permutation_show_text)
        self$results$permutationTitle$setVisible(isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))
      }

      ### Sequence Analysis

      if(isTRUE(self$options$sequences_show_plot)) {
          
          # Set visibility for sequence analysis
          self$results$sequences_plot$setVisible(TRUE)
          
      } else {
          self$results$sequences_plot$setVisible(FALSE)
      }
    },
    .showBuildModelPlot=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_plot)  {
        if(length(plotData) == 1) {
          par(mfrow = c(1, 1))
        } else if(length(plotData) <= 4) {
          par(mfrow = c(2, 2))
        } else if(length(plotData) <= 6) {
          par(mfrow = c(2, 3))
        } else if(length(plotData) <= 9) {
          par(mfrow = c(3, 3))
        } else {
          row <- ceiling(sqrt(length(plotData)))
          column <- ceiling(length(plotData) / row)
          par(mfrow = c(row, column))
        }
        plot(x=plotData, 
          cut=0.1, # Default cut level set to 0.1
          minimum=self$options$buildModel_plot_min_value,
          edge.label.cex=self$options$buildModel_plot_edge_label_size,
          node.width=self$options$buildModel_plot_node_size,
          label.cex=self$options$buildModel_plot_node_label_size,
          layout=self$options$buildModel_plot_layout
        )
        TRUE
      }
      else {
        FALSE
      }
    },
    .showBuildModelHisto=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_histo)  {
        # Check if plotData is a list (multiple groups) or single model
        if(is.list(plotData) && length(plotData) > 1) {
          # Multiple groups - set up layout like other plots
          if(length(plotData) == 1) {
            par(mfrow = c(1, 1))
          } else if(length(plotData) <= 4) {
            par(mfrow = c(2, 2))
          } else if(length(plotData) <= 6) {
            par(mfrow = c(2, 3))
          } else if(length(plotData) <= 9) {
            par(mfrow = c(3, 3))
          } else {
            row <- ceiling(sqrt(length(plotData)))
            column <- ceiling(length(plotData) / row)
            par(mfrow = c(row, column))
          }
          
          # Create histogram for each group
          for(i in 1:length(plotData)) {
            group_name <- names(plotData)[i]
            if(is.null(group_name)) group_name <- paste("Group", i)
            
            hist(x=plotData[[i]], 
                 main=paste("Histogram -", group_name), 
                 xlab="Edge Weights (Probabilities)", 
                 ylab="Frequency")
          }
        } else {
          # Single model - use single plot layout
          par(mfrow = c(1, 1))
          hist(x=plotData, main="Histogram of Edge Weights (Probabilities)", 
               xlab="Edge Weights (Probabilities)", ylab="Frequency")
        }
        TRUE
      } else {
        FALSE
      }
    },
    .showBuildModelFrequencies=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_frequencies)  {
        # Use the tna package plot_frequencies function and print the result
        tryCatch({
          p <- tna::plot_frequencies(x=plotData)
          if(!is.null(p)) {
            print(p)
          }
        }, error = function(e) {
          # Fallback to hist if plot_frequencies fails
          hist(x=plotData, main="Frequencies Plot", 
               xlab="Edge Weights", ylab="Frequency")
        })
        TRUE
      } else {
        FALSE
      }
    },
    .showBuildModelMosaic=function(image, ...) {
      plotData <- self$results$buildModelContent$state

      if(!is.null(plotData) && self$options$buildModel_show_mosaic)  {
        p <- tna::plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
        print(p)
        TRUE
      } else {
        FALSE
      }
    },
    .showCentralityPlot=function(image, ...) {

      plotData <- self$results$centralityTable$state

      if(!is.null(plotData) && self$options$centrality_show_plot)  {
        centPlot <- plot(plotData) 
        print(centPlot)
        TRUE
      } else {
        FALSE
      }
    },
    .showCommunityPlot=function(image, ...) {
      plotData <- self$results$community_plot$state

      if(!is.null(plotData) && self$options$community_show_plot)  {
        if(length(plotData) == 1) {
          par(mfrow = c(1, 1))
        } else if(length(plotData) <= 4) {
          par(mfrow = c(2, 2))
        } else if(length(plotData) <= 6) {
          par(mfrow = c(2, 3))
        } else if(length(plotData) <= 9) {
          par(mfrow = c(3, 3))
        } else {
          row <- ceiling(sqrt(length(plotData)))
          column <- ceiling(length(plotData) / row)
          par(mfrow = c(row, column))
        }
        methods <- self$options$community_methods
        plot(x=plotData, method=methods)
        TRUE
      }
      else {
        FALSE
      }
    },
    .showCliquesPlot1=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state

        number_value <- lengths(plotData[1])
        
        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 0)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=1, 
                n=1,
                cut=self$options$cliques_plot_cut,
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot1$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot2=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])
        
        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 1)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=2, 
                n=1,
                cut=self$options$cliques_plot_cut,
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot2$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot3=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 2)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=3, 
                n=1,
                cut=self$options$cliques_plot_cut,
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot3$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot4=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 3)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=4, 
                n=1,
                cut=self$options$cliques_plot_cut,
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot4$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot5=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 4)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=5, 
                n=1,
                cut=self$options$cliques_plot_cut,
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }
        else {
            self$results$cliques_multiple_plot$cliques_plot5$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot6=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 5)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=6, 
                n=1,
                cut=self$options$cliques_plot_cut,
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot6$setVisible(FALSE)
            FALSE
        }
    },
    .showBootstrapPlot=function(image, ...) {
        plotData <- self$results$bootstrap_plot$state
        if(!is.null(plotData) && self$options$bootstrap_show_plot)  {
            if(length(plotData) == 1) {
              par(mfrow = c(1, 1))
            } else if(length(plotData) <= 4) {
              par(mfrow = c(2, 2))
            } else if(length(plotData) <= 6) {
              par(mfrow = c(2, 3))
            } else if(length(plotData) <= 9) {
              par(mfrow = c(3, 3))
            } else {
              row <- ceiling(sqrt(length(plotData)))
              column <- ceiling(length(plotData) / row)
              par(mfrow = c(row, column))
            }
            plot(x=plotData, cut = 0.01)
        }
        TRUE     
    },

    .showPermutationPlot=function(image, ...) {

      plotData <- self$results$permutation_plot$state
      if(!is.null(plotData) && self$options$permutation_show_plot)  {

        if(length(plotData) == 1) {
          par(mfrow = c(1, 1))
        } else if(length(plotData) <= 4) {
          par(mfrow = c(2, 2))
        } else if(length(plotData) <= 6) {
          par(mfrow = c(2, 3))
        } else if(length(plotData) <= 9) {
          par(mfrow = c(3, 3))
        } else {
          row <- ceiling(sqrt(length(plotData)))
          column <- ceiling(length(plotData) / row)
          par(mfrow = c(row, column))
        }

        plot(x=plotData)
        TRUE
      }
      else {
        FALSE
      }
    },
        .showSequencesPlot=function(image, ...) {
            
            if(self$options$sequences_show_plot) {
                
                # Get the TNA data
                tna_data <- self$results$buildModelContent$state
                
                if(!is.null(tna_data)) {
                    
                    # Call the tna::plot_sequences function directly
                    tryCatch({
                        plot_result <- tna::plot_sequences(
                            x = tna_data,
                            type = self$options$sequences_type,
                            scale = self$options$sequences_scale,
                            geom = self$options$sequences_geom,
                            include_na = self$options$sequences_include_na,
                            tick = self$options$sequences_tick
                        )
                        
                        print(plot_result)
                    }, error = function(e) {
                        # Simple fallback plot
                        plot(1, type="n", main="Sequence Analysis Error", 
                             sub=paste("Error:", e$message))
                    })
                }
            }
            TRUE
        }
  )
)
