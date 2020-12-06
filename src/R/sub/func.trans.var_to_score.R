#'
#' @import tibble
#'
#' @parameter col_info.var.levels
#' @parameter var.levels
#' @parameter var.score
#' @parameter orig_name
#' @parameter input.data
#' 
#' @export
#' 
trans.var_to_score <-
  function(
    col_info.var.levels,
    var.levels="var.levels",
    var.score="var.score",
    orig_name="orig_name",
    col_name_scored="col_name_scored",
    input.data
    ){
    col_info.var.levels[,"orig_name"] <- 
      col_info.var.levels[,orig_name]
    
    col_info.var.levels <- 
      col_info.var.levels[
        !is.na(col_info.var.levels$var.score),
        ]
    result <- plyr::ddply(
        col_info.var.levels[
          !is.na(col_info.var.levels[,var.score]),
          ],
        plyr::.(col_name),
        function(D){
          print(D)
          levels.vec <- unlist(
            strsplit(
              gsub("^\\{(.+)\\}$","\\1",D[,var.levels]),
              split = "\\}\\{"
              )
            )
          score.vec <- unlist(
            strsplit(
              gsub("^\\{(.+)\\}$","\\1",D[,var.score]),
              split = "\\}\\{"
              )
            ) %>%
            as.numeric()
          
      x <-  as.numeric(data.frame(input.data)[,D$orig_name])
      print(head(x))
      additive.vec <-
        vapply(
          levels.vec, 
          FUN = function(i){
            test <- eval(parse(text = sprintf("x%s",i)))
            return(test)
            },
          FUN.VALUE = c(rep(0, length=length(x)))
          )
      weighted.additive.vec <-
        apply(additive.vec,1,function(vec) vec* score.vec)
      return(
        data.frame(
          col_name =  D[,col_name_scored],
          weighted.additive.vec
          )
        )
        }
      ) %>%
  data.frame() %>%
    mutate(
    orig_name_to_rowname = 
      sprintf("%s_%s", col_name_scored, 1:nrow(.))
    ) %>%
  tibble::column_to_rownames(
    "orig_name_to_rowname"
    )
    res <-
      result %>%
      ddply(
        .(col_name),
        function(D){
          res <- apply(D[,which(!(colnames(D)%in%c("orig_name","col_name")))],2,sum)
          return(res)
          }
        ) %>%
      tibble::column_to_rownames(col_name_scored)
    return(res)
    }
