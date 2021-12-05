#' Compare common columns
#'
#' Compare and returns commom columns of the datasets within a list, faster and easy.
#'
#' @usage listReduce(mylist)
#'
#' @param mylist a list
#'
#' @details
#' reduceList function was written in base R, therefore, much faster and simpler to use compared to traditional packages. It always returns the common columns of the databases within the list.
#'
#' @examples
#' list1 <- list(
#' df_1 = data.frame(x = 1:2, y = 3:4, z = 5:6),
#' df_2 = data.frame(x = 7:8, y = 9:10, z = 11:12),
#' df_3 = data.frame(x = 13:14, j = 15:16, z = 17:18)
#' )
#'
#' listReduce(mylist = list1)
#'
#' @export

listReduce <- function(mylist) {
  mylist |>
    (\(x) Reduce(x = lapply(X = x, FUN = colnames), f = intersect))() |>
    (\(x) lapply(X = mylist, FUN = function(y) {
      if (is.list(x = y[x]) == TRUE) {
        return(y[x])
      }
      else (stop("Verify your list!"))
    }))()
}
