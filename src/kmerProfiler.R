library(R6)
library('reshape2')
library('ggplot2')


kmeanProfiler <- R6Class('kmeanProfile',
                    public = list(
                      df='matrix',
                      itermax='numeric',
                      initialize =  function(x,y=10) {
                        self$df <<- data.matrix(x);
                        self$itermax  <<- y;
                      },
                      k_extract = function(x) {
                        kmeans(self$df, x, iter.max=self$itermax);
                      },
                      k_child = function(x, y) {
                        k_obj <- self$k_extract(x);
                        self$df[k_obj$cluster == y,];
                      },
                      k_children = function(x){
                        output <- list();
                        for(y in 1:x) {
                          output[[y]] <- self$k_child(x, y);
                        }
                        output
                      },
                      scree_data = function(x){

                          results <- list()
                          for (i in 1:x){
                            results[[i]]<- sum(self$k_extract(i)$withinss)
                          }
                          melt(results)

                      },
                      assest_k = function(x){
                        require(ggplot2)
                        require(reshape)
                        k_list <- k_children(x);
                        k_df <- melt(k_list)
                        p <- ggplot(k_df, aes(x=X2, y=value, group=X1))
                        p <- p + geom_line()
                        p <- p + facet_wrap(~L1)
                        p
                      }
                      )
              )




kmeanValidation <- R6Class('kmeanValidator',
                          inherit = kmeanProfiler,
                          public = list(
                            kp = 'kmeanProfile',
                            initialize = function(x){
                              self$kp <<- x;
                              super$initialize(x$df, x$itermax)
                            },
                            gen_rand = function(x, max.rand=200){

                              results <- list()
                              for (i in 1:max.rand){

                                results[[i]] <- private$rand_k(x)

                              }
                              melt(results)

                              }

                            ),
                            private=list(
                              rand_k = function(x){
                                  results <- list()
                                  rand_df <- matrix(sample(self$df),
                                                  dim(self$df)[1],
                                                  dim(self$df)[2])
                                  for(i in 1:x){
                                     results[[i]] <- sum(kmeans(rand_df,
                                       i, self$itermax)$withinss)

                                 }
                                 print(results)
                                 results
                                }
                              )

)
