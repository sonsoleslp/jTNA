# This file is a generated template, your changes will not be overwritten

TNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "TNAClass",
    inherit = TNABase,
    private = list(
        .run = function() {

            library("tna")

            type <- self$options$buildModel_type
            scaling <- self$options$buildModel_scaling

            ### Build Model

            # Check if action variable is provided, if not, hide error and return early
            if(is.null(self$options$buildModel_variables_long_action)) {
                self$results$errorText$setVisible(FALSE)
                return()
            }

            model <- NULL

            if( !self$options$buildModel_show_matrix && 
                !self$options$buildModel_show_plot && 
                !self$options$buildModel_show_histo && 
                (    
                    !self$options$buildModel_show_mosaic ||
                    (self$options$buildModel_show_mosaic && self$options$buildModel_type == "relative") )    
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
                        self$options$buildModel_variables_long_order
                    )

                    longData <- copyData[columnToUseLong]

                    if(ncol(longData) > 0) {
                        actorColumn <- self$options$buildModel_variables_long_actor
                        timeColumn <- self$options$buildModel_variables_long_time
                        actionColumn <- self$options$buildModel_variables_long_action
                        orderColumn <- self$options$buildModel_variables_long_order

                        args_prepare_data <- list(
                            data = longData,
                            actor = actorColumn,
                            time = timeColumn,
                            action = actionColumn,
                            time_threshold = threshold,
                            order = orderColumn
                        ) 

                        args_prepare_data <- args_prepare_data[!sapply(args_prepare_data, is.null)]

                        dataForTNA <- do.call(prepare_data, args_prepare_data)
                    }

                    if(!is.null(dataForTNA)) {

                        if(scaling == "noScaling") {
                            scaling = character(0L)
                        }

                        model <- build_model(x=dataForTNA, type=type, scaling=scaling)     
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

                self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot) # plot
                
                self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo) # plot
                
                self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies) # plot
                
                self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic) # plot

            }
            


            ### Centrality

            if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot) ) {
                centrality_loops <- self$options$centrality_loops
                centrality_normalize <- self$options$centrality_normalize

                vectorCharacter <- character(0)    

                fullTable <- self$results$centralityTable$isFilled()             


                if(self$options$centrality_OutStrength) {
                    vectorCharacter <- append(vectorCharacter, "OutStrength")
                    self$results$centralityTable$addColumn(name="OutStrength", type="number")
                }
                if(self$options$centrality_InStrength) {
                    vectorCharacter <- append(vectorCharacter, "InStrength")
                    self$results$centralityTable$addColumn(name="InStrength", type="number")
                }
                if(self$options$centrality_ClosenessIn) {
                    vectorCharacter <- append(vectorCharacter, "ClosenessIn")
                    self$results$centralityTable$addColumn(name="ClosenessIn", type="number")
                }
                if(self$options$centrality_ClosenessOut) {
                    vectorCharacter <- append(vectorCharacter, "ClosenessOut")
                    self$results$centralityTable$addColumn(name="ClosenessOut", type="number")
                }
                if(self$options$centrality_Closeness) {
                    vectorCharacter <- append(vectorCharacter, "Closeness")
                    self$results$centralityTable$addColumn(name="Closeness", type="number")
                }
                if(self$options$centrality_Betweenness) {
                    vectorCharacter <- append(vectorCharacter, "Betweenness")
                    self$results$centralityTable$addColumn(name="Betweenness", type="integer")
                }
                if(self$options$centrality_BetweennessRSP) {
                    vectorCharacter <- append(vectorCharacter, "BetweennessRSP")
                    self$results$centralityTable$addColumn(name="BetweennessRSP", type="number")
                }
                if(self$options$centrality_Diffusion) {
                    vectorCharacter <- append(vectorCharacter, "Diffusion")
                    self$results$centralityTable$addColumn(name="Diffusion", type="number")
                }
                if(self$options$centrality_Clustering) {
                    vectorCharacter <- append(vectorCharacter, "Clustering")
                    self$results$centralityTable$addColumn(name="Clustering", type="number")
                }

                cent <- self$results$centralityTable$state
                if(length(vectorCharacter) > 0 && !is.null(model) && 
                    (!self$results$centrality_plot$isFilled() || !fullTable)) 
                {
                    cent <- centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
                    self$results$centralityTable$setState(cent)
                }

                for (i in 1:lengths(cent[1])) {
                    index <- 1
                    rowValues <- list()

                    rowValues$state <- as.character(cent[i, index])

                    if ("OutStrength" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$OutStrength <- as.numeric(cent[i, index])
                    }
                    if ("InStrength" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$InStrength <- as.numeric(cent[i, index])
                    }
                    if ("ClosenessIn" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$ClosenessIn <- as.numeric(cent[i, index])
                    }
                    if ("ClosenessOut" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$ClosenessOut <- as.numeric(cent[i, index])
                    }
                    if ("Closeness" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$Closeness <- as.numeric(cent[i, index])
                    }
                    if ("Betweenness" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$Betweenness <- as.numeric(cent[i, index])
                    }
                    if ("BetweennessRSP" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$BetweennessRSP <- as.numeric(cent[i, index])
                    }
                    if ("Diffusion" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$Diffusion <- as.numeric(cent[i, index])
                    }
                    if ("Clustering" %in% vectorCharacter) {
                        index <- index + 1
                        rowValues$Clustering <- as.numeric(cent[i, index])
                    }
                    self$results$centralityTable$addRow(rowKey=i, values=rowValues)
                }
                self$results$centralityTitle$setVisible(self$options$centrality_show_plot || self$options$centrality_show_table)
                self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
                self$results$centralityTable$setVisible(self$options$centrality_show_table)
                    
            }

            ### Edge betweenness

            if(!is.null(model) && (self$options$edgeBetweenness_show_table || self$options$edgeBetweenness_show_plot)) {
                edgeBetwenness <- betweenness_network(x=model)

                # Plot
                if(!self$results$edgeBetweenness_plot$isFilled()) {
                    self$results$edgeBetweenness_plot$setState(edgeBetwenness)
                }
                
                # Table - show weights
                matrix_to_from_to_value <- function(your_matrix) {
                  if (is.null(rownames(your_matrix))) {
                    rownames(your_matrix) <- paste0("Row", 1:nrow(your_matrix))
                  }
                  if (is.null(colnames(your_matrix))) {
                    colnames(your_matrix) <- paste0("Col", 1:ncol(your_matrix))
                  }
                  df <- setNames(as.data.frame(as.table(your_matrix)), c("from", "to", "value"))
                  return(df)
                }

                if(!is.null(edgeBetwenness) && !is.null(edgeBetwenness$weights)) {
                    betable <- matrix_to_from_to_value(edgeBetwenness$weights)

                    betable <- betable[betable$value > 0, ]

                    betable <- betable[order(-betable$value), ]

                    for (i in 1:nrow(betable)) {
                        self$results$edgeBetweennessTable$addRow(rowKey=i, values=list(
                            from=as.character(betable[i, "from"]),
                            to=as.character(betable[i, "to"]),
                            value=betable[i, "value"]
                        ))
                    }

                    has_results_to_display <- nrow(betable) > 0
                    self$results$edgeBetweennessTable$setVisible(self$options$edgeBetweenness_show_table && has_results_to_display)
                    self$results$edgeBetweennessNoResultsNote$setVisible(self$options$edgeBetweenness_show_table && !has_results_to_display)
                    if (self$options$edgeBetweenness_show_table && !has_results_to_display) {
                        self$results$edgeBetweennessNoResultsNote$setContent("No weights available for Edge Betweenness based on current filters.")
                    } else {
                        self$results$edgeBetweennessNoResultsNote$setContent("") # Clear the note if results are displayed
                    }

                } else {
                    self$results$edgeBetweennessTable$setVisible(FALSE)
                    self$results$edgeBetweennessNoResultsNote$setVisible(self$options$edgeBetweenness_show_table)
                    self$results$edgeBetweennessNoResultsNote$setContent("No weights available for Edge Betweenness.")
                }
                
                # Set plot visibility
                self$results$edgeBetweenness_plot$setVisible(self$options$edgeBetweenness_show_plot)
                self$results$edgeBetweennessTitle$setVisible(self$options$edgeBetweenness_show_table || self$options$edgeBetweenness_show_plot)
            }


            ### Community

            if(!is.null(model) && (self$options$community_show_table || self$options$community_show_plot)) {
                community_gamma <- as.numeric(self$options$community_gamma)
                methods <- self$options$community_methods

                # Initialize coms variable to avoid scope issues
                coms <- self$results$community_plot$state
                
                if((!self$results$communityContent$isFilled() || !self$results$community_plot$isFilled())) { 

                    if(is.null(coms)) {
                        coms <- NULL
                    }
                    
                    resultComs <- tryCatch({
                        coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
                        TRUE
                    }, error = function(e) {
                        self$results$communityTitle$setVisible(TRUE)
                        self$results$communityErrorText$setContent(paste("The methods", methods, "should be change :\n\t", conditionMessage(e)) )
                        self$results$communityErrorText$setVisible(TRUE)
                        FALSE
                    })
                    
                    if(!resultComs) {
                        return()
                    }

                    if(!self$results$community_plot$isFilled()) {
                        self$results$community_plot$setState(coms)
                    }
                    if(!self$results$communityContent$isFilled()) {
                        self$results$communityContent$setContent(coms)
                    }
                    
                }

                # Populate communities table
                if(!is.null(coms) && self$options$community_show_table) {
                    if(!is.null(coms$assignments)) {
                        assignments <- coms$assignments
                        
                        # Add columns for each community detection method
                        method_names <- colnames(assignments)[-1]  # Exclude 'state' column
                        for(method in method_names) {
                            self$results$communityTable$addColumn(name=method, title=method, type="integer")
                        }
                        
                        # Add rows with community assignments
                        for (i in 1:nrow(assignments)) {
                            rowValues <- list()
                            rowValues$state <- as.character(assignments[i, "state"])
                            
                            for(method in method_names) {
                                rowValues[[method]] <- as.integer(assignments[i, method])
                            }
                            
                            self$results$communityTable$addRow(rowKey=i, values=rowValues)
                        }
                    }
                }
                
                self$results$community_plot$setVisible(self$options$community_show_plot)
                self$results$communityTable$setVisible(self$options$community_show_table)
                self$results$communityContent$setVisible(FALSE)
                self$results$communityTitle$setVisible(self$options$community_show_plot || self$options$community_show_table)

            }

            ### Cliques

            cliques_size <- as.numeric(self$options$cliques_size)
            cliques_threshold <- as.numeric(self$options$cliques_threshold)

            if(!is.null(model) && ( self$options$cliques_show_text || self$options$cliques_show_plot) ) {

                if(!self$results$cliques_multiple_plot$isFilled() || !self$results$cliquesContent$isFilled()) {
                    cliques <- cliques(x=model, size=cliques_size, threshold=cliques_threshold)

                    if(!self$results$cliquesContent$isFilled()) {
                        self$results$cliquesContent$setContent(cliques)
                    }

                    if(!self$results$cliques_multiple_plot$isFilled()) {
                        self$results$cliques_multiple_plot$setState(cliques)
                    }
                }
                self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
                self$results$cliquesContent$setVisible(self$options$cliques_show_text)
                self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)


            }

                        ### Bootstrap

            if(!is.null(model) && ( self$options$bootstrap_show_plot || self$options$bootstrap_show_table)) {

                # Check if we need to compute bootstrap
                bs <- self$results$bootstrap_plot$state
                if(is.null(bs) || !self$results$bootstrap_plot$isFilled()) {
                    iteration <- self$options$bootstrap_iteration
                    level <- self$options$bootstrap_level
                    method <- self$options$bootstrap_method

                    range_low <- self$options$bootstrap_range_low
                    range_up <- self$options$bootstrap_range_up

                    threshold <- self$options$bootstrap_threshold

                    bs <- bootstrap(
                                    x=model, 
                                    iter=iteration,
                                    level=level,
                                    method=method,
                                    threshold=threshold,
                                    consistency_range=c(range_low, range_up)
                    )

                    self$results$bootstrap_plot$setState(bs)
                }

                # Populate bootstrap table
                if(!is.null(bs) && !is.null(bs$summary) && self$options$bootstrap_show_table) {
                    all_edges <- bs$summary
                    if(nrow(all_edges) > 0) {
                        # Sort by significance (TRUE/significant first, then FALSE/non-significant)
                        all_edges <- all_edges[order(-all_edges$sig), ]
                        
                        for (i in 1:nrow(all_edges)) {
                            self$results$bootstrapTable$addRow(rowKey=i, values=list(
                                from=as.character(all_edges[i, "from"]),
                                to=as.character(all_edges[i, "to"]),
                                weight=all_edges[i, "weight"],
                                p_value=all_edges[i, "p_value"],
                                cr_lower=all_edges[i, "cr_lower"],
                                cr_upper=all_edges[i, "cr_upper"],
                                ci_lower=all_edges[i, "ci_lower"],
                                ci_upper=all_edges[i, "ci_upper"],
                                significant=ifelse(all_edges[i, "sig"], "Yes", "No")
                            ))
                        }
                    }
                }
                
                self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
                self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
                self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_table)

            }

            ### Sequence Analysis

            if(self$options$sequences_show_plot) {
                
                self$results$sequences_plot$setVisible(TRUE)
                
            } else {
                self$results$sequences_plot$setVisible(FALSE)
            }

        },
        .showBuildModelPlot=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_plot)  {
                plot(x=plotData, 
                    cut=self$options$buildModel_plot_cut,
                    minimum=self$options$buildModel_plot_min_value,
                    edge.label.cex=self$options$buildModel_plot_edge_label_size,
                    node.width=self$options$buildModel_plot_node_size,
                    label.cex=self$options$buildModel_plot_node_label_size,
                    layout=self$options$buildModel_plot_layout,
                    bg="transparent"
                )
            }   
            TRUE
        },
        .showBuildModelHisto=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_histo)  {
                par(mfrow = c(1, 1))
                hist(x=plotData, main="Histogram of Edge Weights (Probabilities)", 
                     xlab="Edge Weights (Probabilities)", ylab="Frequency")
            }   
            TRUE
        },
        .showBuildModelFrequencies=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_frequencies)  {
                tryCatch({
                    p <- plot_frequencies(x=plotData)
                    if(!is.null(p)) {
                        print(p)
                    }
                }, error = function(e) {
                    hist(x=plotData, main="Frequencies Plot", 
                         xlab="Edge Weights", ylab="Frequency")
                })
            }   
            TRUE
        },
        .showBuildModelMosaic=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_mosaic && self$options$buildModel_type == "frequency")  {
                p <- plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
                print(p)
            }   
            TRUE
        },
        .showCentralityPlot=function(image, ...) {

            plotData <- self$results$centralityTable$state

            if(!is.null(plotData) && self$options$centrality_show_plot)  {
                centPlot <- plot(plotData) 
                print(centPlot)
            }
            TRUE     
        },
        .showEdgeBetweennessPlot=function(image, ...) {
            plotData <- self$results$edgeBetweenness_plot$state
            
            if(!is.null(plotData) && self$options$edgeBetweenness_show_plot)  {
                plot(
                    x=plotData,
                    cut=self$options$edgeBetweenness_plot_cut,
                    minimum=self$options$edgeBetweenness_plot_min_value,
                    edge.label.cex=self$options$edgeBetweenness_plot_edge_label_size,
                    node.width=self$options$edgeBetweenness_plot_node_size,
                    label.cex=self$options$edgeBetweenness_plot_node_label_size,
                    layout=self$options$edgeBetweenness_plot_layout,
                    bg="transparent"
                )
                TRUE
            } else {
                FALSE
            }
        },
        .showCommunityPlot=function(image, ...) {
            plotData <- self$results$community_plot$state
            
            if(!is.null(plotData) && self$options$community_show_plot)  {
                methods <- self$options$community_methods
                plot(x=plotData, method=methods, bg="transparent")
            }   
            TRUE
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
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
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
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
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
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
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
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
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
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
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
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
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
                plot(x=plotData, cut = 0.01)
            }
            TRUE     
        },
        .showSequencesPlot=function(image, ...) {
            
            if(self$options$sequences_show_plot) {
                
                tna_data <- self$results$buildModelContent$state
                
                if(!is.null(tna_data)) {
                    
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
                        plot(1, type="n", main="Sequence Analysis Error", 
                             sub=paste("Error:", e$message))
                    })
                }
            }
            TRUE
        },
        .showCliquesMultiPlot=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            if (!is.null(plotData) && self$options$cliques_show_plot) {
                plot(x=plotData, 
                    ask=FALSE,
                    cut=self$options$cliques_plot_cut,
                    minimum=self$options$cliques_plot_min_value,
                    edge.label.cex=self$options$cliques_plot_edge_label_size,
                    node.width=self$options$cliques_plot_node_size,
                    label.cex=self$options$cliques_plot_node_label_size,
                    layout=self$options$cliques_plot_layout,
                    bg="transparent"
                )
                TRUE
            } else {
                FALSE
            }
        }
    )
)