#'Create a flowchart of the sampling process
#'
#'
#'
#'@param solution object storing the output of \code{stratifier}; usually called \code{generalizer_output}
#'@return a flowchart of the sampling process from \code{stratifier}
#'@seealso \url{http://thegeneralizer.org/}, also add other resources
#' @examples
#' \dontrun{
#' # To view a table of means along with graphs:
#' flowchart(solution)
#' }

flowchart <- function(solution){

  # Define some sample data
  data <- list(a=1000, b=800, c=600, d=400)


  DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Biege]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']

a -> b -> c -> d

}

[1]:  paste0('Population Size: ', dim(generalizer_output[[1]]$centroids)[[2]])
[2]: paste0('Remove Errors (n = ', data$b, ')')
[3]: paste0('Identify Potential Customers (n = ', data$c, ')')
[4]: paste0('Select Top Priorities (n = ', data$d, ')')
")

}
