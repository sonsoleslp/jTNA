
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

      if(is.null(self$options$buildModel_variables_long_action)) {
        self$results$errorText$setVisible(TRUE)
        self$results$errorText$setContent("Action should be provided")
        return()
      }

      if(is.null(self$options$buildModel_variables_long_group)) {
        self$results$errorText$setVisible(TRUE)
        self$results$errorText$setContent("Group should be provided")
        return()
      }

      model <- NULL

      if(!self$options$buildModel_show_matrix &&
        !self$options$buildModel_show_plot &&
        !self$options$buildModel_show_histo &&
        (
          !self$options$buildModel_show_mosaic ||
          (
            self$options$buildModel_show_mosaic && 
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
          

          # # mosaic
          self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic ) # plot

      }

      ### Centrality

      if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot) ) {
          centrality_loops <- self$options$centrality_loops
          centrality_normalize <- self$options$centrality_normalize

          vectorCharacter <- character(0)    

          fullTable <- self$results$centralityTable$isFilled()

          self$results$centralityTable$addColumn(name="group", type="text")

          self$results$centralityTable$addColumn(name="state", type="text")

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
          if(length(vectorCharacter) > 0 && !is.null(model) && (!self$results$centrality_plot$isFilled() || !fullTable) ) {
              cent <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
              self$results$centralityTable$setState(cent)
          }

              for (i in 1:lengths(cent[1])) {
                  index <- 1
                  rowValues <- list()

                  rowValues$group <- as.character(cent[i, index])

                  index <- index + 1
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


      ### Community
      if(!is.null(model) &&  (self$options$community_show_table || self$options$community_show_plot) ) {
        community_gamma <- as.numeric(self$options$community_gamma)
        methods <- self$options$community_methods

        resultComs <- TRUE
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

          if(resultComs) {
            # Plot
            if(!self$results$community_plot$isFilled()) {
              self$results$community_plot$setState(coms)
            }
            # Text
            if(!self$results$communityContent$isFilled()) {
              self$results$communityContent$setContent(coms)
            }
          }

             
        }
        if(resultComs) {
          self$results$community_plot$setVisible(self$options$community_show_plot)
          self$results$communityContent$setVisible(self$options$community_show_table)
          self$results$communityTitle$setVisible(self$options$community_show_plot || self$options$community_show_table)
        }
      }

      ### Cliques

      cliques_size <- as.numeric(self$options$cliques_size)
      cliques_threshold <- as.numeric(self$options$cliques_threshold)

      if(!is.null(model) && ( self$options$cliques_show_text || self$options$cliques_show_plot) ) {

          if(!self$results$cliques_multiple_plot$isFilled() || !self$results$cliquesContent$isFilled()) {
              cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)

              # Text
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

          bs <- tna::bootstrap(
            x=model, 
            iter=iteration,
            level=level,
            method=method,
            threshold=threshold,
            consistency_range=c(range_low, range_up)
          )

          # Text
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

      ### Comparison

      if(!is.null(model) && ( self$options$compare_show_text || self$options$compare_show_plot || self$options$compare_show_TNAplot)) {
          indexModel1 <- self$options$compare_model1
          indexModel2 <- self$options$compare_model2
          size <- length(model)

          if(size < indexModel1 || size < indexModel2) {
              self$results$errorText$setVisible(TRUE)
              self$results$errorText$setContent(paste("Comparison - The index given can't be bigger than the number of TNA (", size, ")"))
              return()
          }

          if( !self$results$comparisonContent$isFilled() || 
              !self$results$comparison_plot$isFilled() || 
              !self$results$comparisonTNA_plot$isFilled()
          )
          {
              compare_group <- tna::compare(model, i=indexModel1, j=indexModel2)


              # Text
              if(!self$results$comparisonContent$isFilled()) {
                self$results$comparisonContent$setContent(compare_group)
              }

              # Plot compareTNA
              if(!self$results$comparisonTNA_plot$isFilled()) {
                self$results$comparisonTNA_plot$setState(compare_group)
              }
          }
          self$results$comparisonTitle$setVisible(self$options$compare_show_text || self$options$compare_show_plot || self$options$compare_show_TNAplot)
          self$results$comparisonContent$setVisible(self$options$compare_show_text)
          self$results$comparison_plot$setVisible(self$options$compare_show_plot)
          self$results$comparisonTNA_plot$setVisible(self$options$compare_show_TNAplot)
      }

      ## Permutation

      if(!is.null(model) && ( self$options$permutation_show_text || self$options$permutation_show_plot)) {
        if( !self$results$permutationContent$isFilled() || 
              !self$results$permutation_plot$isFilled()
          )
          {
            permutationTest <- tna::permutation_test(
              x=model, 
              iter=self$options$permutation_iter, 
              paired=self$options$permutation_paired,
              level=self$options$permutation_level
            )

            # Text
            if(!self$results$permutationContent$isFilled()) {
              self$results$permutationContent$setContent(permutationTest)
            }

            # Plot
            if(!self$results$permutation_plot$isFilled()) {
              self$results$permutation_plot$setState(permutationTest)
            }
          }
          self$results$permutationTitle$setVisible(self$options$permutation_show_text || self$options$permutation_show_plot)
          self$results$permutationContent$setVisible(self$options$permutation_show_text)
          self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
      }
    },
    .showBuildModelPlot=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_plot)  {
        if(length(plotData) <= 4) {
          row <- length(plotData)
          column <- 1
        } else if(length(plotData) <= 8) {
          row <- length(plotData)
          column <- 2
        } else if(length(plotData) <= 12) {
          row <- length(plotData)
          column <- 3
        } else {
          row <- floor(length(plotData)/4) + 1
          column <- 4
        }
        par(mfrow = c(row, column))
        plot(x=plotData, 
          cut=self$options$buildModel_plot_cut,
          minimum=self$options$buildModel_plot_min_value,
          edge.label.cex=self$options$buildModel_plot_edge_label_size,
          node.width=self$options$buildModel_plot_node_size,
          label.cex=self$options$buildModel_plot_node_label_size,
          layout=self$options$buildModel_plot_layout,
          pie=NULL
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
          
        if(length(plotData) <= 4) {
          row <- length(plotData)
          column <- 1
        } else if(length(plotData) <= 8) {
          row <- length(plotData)
          column <- 2
        } else if(length(plotData) <= 12) {
          row <- length(plotData)
          column <- 3
        } else {
          row <- floor(length(plotData)/4) + 1
          column <- 4
        }
        par(mfrow = c(row, column))
        hist(x=plotData)
        TRUE
      } else {
        FALSE
      }
    },
    .showBuildModelMosaic=function(image, ...) {
      plotData <- self$results$buildModelContent$state

      if(!is.null(plotData) && self$options$buildModel_show_mosaic && self$options$buildModel_type != "relative" )  {
        p <- tna::plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
        print(p)
        TRUE
      } else {
        self$results$buildModel_mosaic$setVisible(FALSE)
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
        if(length(plotData) <= 4) {
          row <- length(plotData)
          column <- 1
        } else if(length(plotData) <= 8) {
          row <- length(plotData)
          column <- 2
        } else if(length(plotData) <= 12) {
          row <- length(plotData)
          column <- 3
        } else {
          row <- floor(length(plotData)/4) + 1
          column <- 4
        }
        par(mfrow = c(row, column))
        methods <- self$options$community_methods
        plot(x=plotData, method=methods, pie=NULL)
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
            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else  {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
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
            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else  {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
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
            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else  {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
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

            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else  {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
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
            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else  {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
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
            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else  {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
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
            
            if(length(plotData) <= 4) {
                row <- length(plotData)
                column <- 1
            }
            else if(length(plotData) <= 8) {
                row <- length(plotData)
                column <- 2
            }
            else {
                row <- floor(length(plotData)/3) + 1
                column <- 3
            }
            par(mfrow = c(row, column))
            m_sig <- plotData[[1]]$weights_sig

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
                edge.color = mat_color,
                pie=NULL
            )
            TRUE
        }
        else {
            FALSE
        }     
    },
    .showComparisonPlot=function(image, ...) {
        plotData <- self$results$buildModelContent$state

        if(!is.null(plotData) && self$options$compare_show_plot)  {
            tna::plot_compare(
                x=plotData,
                i = self$options$compare_model1,
                j = self$options$compare_model2
            )
            TRUE
        }
        else {
            FALSE
        }
    },
    .showComparisonTNAPlot=function(image, ...) {
        plotData <- self$results$comparisonTNA_plot$state

        if(!is.null(plotData) && self$options$compare_show_TNAplot)  {
            p <- plot(
                    x=plotData,
                    type = self$options$compare_TNAPlot_type,
                    population = self$options$compare_TNAPlot_population,
                    method = self$options$compare_TNAPlot_method,
                    name_x = "Model A",
                    name_y = "Model B"
                )
            print(p)
            TRUE
        }
        else {
            FALSE
        }
    },
    .showPermutationPlot=function(image, ...) {

      plotData <- self$results$permutation_plot$state
      if(!is.null(plotData) && self$options$permutation_show_plot)  {

        if(length(plotData) <= 4) {
          row <- length(plotData)
          column <- 1
        }
        else if(length(plotData) <= 8) {
          row <- length(plotData)
          column <- 2
        }
        else {
          row <- floor(length(plotData)/3) + 1
          column <- 3
        }
        par(mfrow = c(row, column))

        plot(x=plotData)
        TRUE
      }
      else {
        FALSE
      }
    }
  )
)
