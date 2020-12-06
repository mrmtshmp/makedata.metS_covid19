#' Make data
#' PI Dr Arimori
#' 2020/10/12


dir.sub <- './src/R/sub'
Bibtex <- TRUE

list.files.dir.sub <- list.files(path = dir.sub)

for(i in 1:length(list.files.dir.sub))
  source(sprintf("%s/%s", dir.sub, list.files.dir.sub[i]))


colinfo <- 
  readxl::read_excel(
    path = sprintf("%s/%s",dir.data,fn.data),
    sheet = "col_info"
    ) %>%
  data.frame()

data <- 
  readxl::read_excel(
    path = sprintf("%s/%s",dir.data,fn.data),
    col_names = colinfo[!is.na(colinfo$orig_name),"col_names"],
    col_types = colinfo[!is.na(colinfo$orig_name),"col_types"],
    skip=5,
    sheet="data_02"
    ) %>%
  data.frame()

data <- data[!is.na(data$VFA),]


data[,"year"] <- 
  as.character(
    lubridate::year(
      data[,"date.test"]
      )
    )

data[,"week"] <- 
  as.numeric(
    strftime(
      data[,"date.test"],"%V"
    ) 
  )

save(data,colinfo,file = sprintf("%s/%s", dir.ADS, fn.ADS))

quartz(family = 'Arial',type = 'pdf',file = sprintf("%s/cov_rel.pairwise.pdf", dir.output))
GGally::ggpairs(data)
dev.off()

