
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

            if(is.null(self$options$buildModel_variables_long_action)) {
                self$results$errorText$setVisible(TRUE)
                self$results$errorText$setContent("Action should be provided")
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

                    ##### TO REMOVE
                    values_to_replace <- c("Applications", "Ethics", "General", "La_types", "Theory")
                    new_value <- "Resources"
                    longData[[self$options$buildModel_variables_long_action]] <- replace(
                        longData[[self$options$buildModel_variables_long_action]], 
                        longData[[self$options$buildModel_variables_long_action]] %in% values_to_replace, 
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

                    dataForTNA <- do.call(prepare_data, args_prepare_data)
                }

                if(!is.null(dataForTNA)) {

                    if(scaling == "noScaling") {
                        scaling = character(0L)
                    }

                    model <- build_model(x=dataForTNA, type=type, scaling=scaling)     
                }
            }
            
            if(!is.null(model)) {
                    
                if(!self$results$buildModelContent$isFilled()) {
                    self$results$buildModelContent$setContent(model)
                    self$results$buildModelContent$setState(model)
                }
                self$results$buildModelContent$setVisible(self$options$buildModel_show_matrix)

                self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot) # plot
                
                self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo) # plot
                
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

            if(!is.null(model) &&(self$options$edgeBetweenness_show_text || self$options$edgeBetweenness_show_plot) ) {
                if(!is.null(model) && (!self$results$edgeBetweenness_plot$isFilled() || !self$results$edgeBetweennessContent$isFilled() )) { 
                    edgeBetwenness <- betweenness_network(x=model)

                    # Plot
                    if(!self$results$edgeBetweenness_plot$isFilled()) {
                        self$results$edgeBetweenness_plot$setState(edgeBetwenness)
                    }
                    # Text
                    if(!self$results$edgeBetweennessContent$isFilled()) {
                        self$results$edgeBetweennessContent$setContent(edgeBetwenness)
                    }
                }
                self$results$edgeBetweenness_plot$setVisible(self$options$edgeBetweenness_show_plot)
                self$results$edgeBetweennessContent$setVisible(self$options$edgeBetweenness_show_text)
                self$results$edgeBetweennessTitle$setVisible(self$options$edgeBetweenness_show_text || self$options$edgeBetweenness_show_plot)

            }


            ### Community

            if(!is.null(model) &&  (self$options$community_show_table || self$options$community_show_plot) ) {
                community_gamma <- as.numeric(self$options$community_gamma)
                methods <- self$options$community_methods

                if((!self$results$communityContent$isFilled() || !self$results$community_plot$isFilled())) { 

                    coms <- NULL
                    
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

                    # Plot
                    if(!self$results$community_plot$isFilled()) {
                        self$results$community_plot$setState(coms)
                    }
                    # Text
                    if(!self$results$communityContent$isFilled()) {
                        self$results$communityContent$setContent(coms)
                    }
                    
                }
                self$results$community_plot$setVisible(self$options$community_show_plot)
                self$results$communityContent$setVisible(self$options$community_show_table)
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

                    # Plot
                    if(!self$results$cliques_multiple_plot$isFilled()) {
                        self$results$cliques_multiple_plot$setState(cliques)
                    }
                }
                self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
                self$results$cliquesContent$setVisible(self$options$cliques_show_text)
                self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)


            }

            ### Bootstrap

            if(!is.null(model) && ( self$options$bootstrap_show_text || self$options$bootstrap_show_plot)) {

                if(!self$results$bootstrapContent$isFilled() || !self$results$bootstrap_plot$isFilled()) {
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

                    if(!self$results$bootstrapContent$isFilled()) {
                        self$results$bootstrapContent$setContent(bs)
                    }

                    # Plot
                    if(!self$results$bootstrap_plot$isFilled()) {
                        self$results$bootstrap_plot$setState(bs)
                    }
                }
                self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
                self$results$bootstrapContent$setVisible(self$options$bootstrap_show_text)
                self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_text)

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
                    layout=self$options$buildModel_plot_layout
                )
            }   
            TRUE
        },
        .showBuildModelHisto=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_histo)  {
                hist(x=plotData)
            }   
            TRUE
        },
        .showBuildModelMosaic=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_mosaic && self$options$buildModel_type != "relative" )  {
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
                    layout=self$options$edgeBetweenness_plot_layout
                )
            }   
            TRUE
        },
        .showCommunityPlot=function(image, ...) {
            plotData <- self$results$community_plot$state
            
            if(!is.null(plotData) && self$options$community_show_plot)  {
                methods <- self$options$community_methods
                plot(x=plotData, method=methods)
            }   
            TRUE
        },
        .showCliquesPlot1=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state

            number_value <- lengths(plotData[1])
            
            if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 0)  {
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
            }
            else {
                self$results$cliques_multiple_plot$cliques_plot1$setVisible(FALSE)
            } 
            TRUE
        },
        .showCliquesPlot2=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            
            number_value <- lengths(plotData[1])
            
            if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 1)  {
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
            }   
            else {
                self$results$cliques_multiple_plot$cliques_plot2$setVisible(FALSE)
            } 
            TRUE
        },
        .showCliquesPlot3=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            
            number_value <- lengths(plotData[1])

            if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 2)  {
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
            }   
            else {
                self$results$cliques_multiple_plot$cliques_plot3$setVisible(FALSE)
            } 
            TRUE
        },
        .showCliquesPlot4=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            
            number_value <- lengths(plotData[1])

            if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 3)  {
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
            }   
            else {
                self$results$cliques_multiple_plot$cliques_plot4$setVisible(FALSE)
            } 
            TRUE
        },
        .showCliquesPlot5=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            
            number_value <- lengths(plotData[1])

            if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 4)  {
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
            }
            else {
                self$results$cliques_multiple_plot$cliques_plot5$setVisible(FALSE)
            } 
            TRUE
        },
        .showCliquesPlot6=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            
            number_value <- lengths(plotData[1])

            if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 5)  {
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
            }   
            else {
                self$results$cliques_multiple_plot$cliques_plot6$setVisible(FALSE)
            } 
            TRUE
        },
        .showBootstrapPlot=function(image, ...) {

            plotData <- self$results$bootstrap_plot$state

            if(!is.null(plotData) && self$options$bootstrap_show_plot)  {

                m_sig <- plotData$weights_sig
                
                mat_color <- matrix()
                mat_color[m_sig == 0] <- "red"
                plot(
                    x=plotData,
                    cut=self$options$bootstrap_plot_cut,
                    minimum=self$options$bootstrap_plot_min_value,
                    edge.label.cex=self$options$bootstrap_plot_edge_label_size,
                    node.width=self$options$bootstrap_plot_node_size,
                    label.cex=self$options$bootstrap_plot_node_label_size,
                    layout=self$options$bootstrap_plot_layout,
                    edge.color = mat_color
                )
            }
            TRUE     
        })
)
