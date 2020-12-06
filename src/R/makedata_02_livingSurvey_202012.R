#' load dataset for analysis.

Bibtex <- TRUE
dir.sub <- "./src/R/sub"

StdMeanDiff <- "StdMeanDiff.1"
Psmodel <- "Psmodel"

for(fn in list.files(dir.sub))
  source(
    sprintf( "%s/%s", dir.sub,fn)
    )

df.col_info <- 
  read_excel(
    path = 
      sprintf(
        '%s/%s',
        dir.data_livingSurvey_202012,
        fn.data_livingSurvey_202012
        ),
    sheet = "colinfo", na = "NA"
    ) %>%
  data.frame()

var.ID <- df.col_info[
  !is.na(df.col_info$ID),
  c("col_name",'ID')
  ]

var.strata_in <- df.col_info[
  !is.na(df.col_info$strata_in),
  c("col_name",'strata_in')
  ]

var.exposure <- df.col_info[
  !is.na(df.col_info$exposure) & 
    df.col_info$col_type=="text",
  c("col_name","exposure", "timepoint")
  ]

var.exposure.conti <- df.col_info[
  !is.na(df.col_info$exposure) & 
    df.col_info$col_type=="numeric",
  "col_name"
  ]

var.trans <- df.col_info[
  !is.na(df.col_info$transform) & is.na(df.col_info$cutoff),
  c("transform","col_name", "col_type")
  ]

var.cutoff <- df.col_info[
  !is.na(df.col_info$cutoff),
  c("col_name","transform","var.score","cutoff", "col_type")
  ]

var.label <- df.col_info[
  !is.na(df.col_info$var.label),
  c("orig_name","col_name","var.level","var.label", "col_type")
  ]

var.event  <- df.col_info[
  (df.col_info$outcome=="event"|df.col_info$outcome=="rank") & !is.na(df.col_info$outcome),
  c("col_name","outcome","col_type")
  ]

var.timetoevent <- df.col_info[
  df.col_info$outcome=="time" & !is.na(df.col_info$outcome),
  c("col_name", "col_type")
  ]

var.cens_timetoevent  <- df.col_info[
  df.col_info$outcome=="censored.time" & !is.na(df.col_info$outcome),
  c("col_name", "col_type")
  ]

var.smd <- df.col_info[
  df.col_info[,StdMeanDiff]=="1" & !is.na(df.col_info[,StdMeanDiff]),
  c("col_name", "col_type")
  ]

var.Psmodel <- df.col_info[
  df.col_info[,Psmodel]=="1" & !is.na(df.col_info$Psmodel),
  c("col_name", "col_type")
  ]

# Import EXCEL dataset ----------------------------------------------------

df.imported_data.completed <- 
  read_excel(
    path =
      sprintf(
        '%s/%s', 
        dir.data_livingSurvey_202012,
        fn.data_livingSurvey_202012
        ),
    sheet = "data", skip=1,
    na = "*",
    col_names = df.col_info[!is.na(df.col_info$orig_name),"col_name"],
    col_types = df.col_info[!is.na(df.col_info$orig_name),"col_type"]
    ) %>%
  data.frame()


# Correction for abnormal data ---------------------------------------------

df.correction_abn <- 
  read_excel(
    path =
      sprintf(
        '%s/%s', 
        dir.data_livingSurvey_202012,
        fn.data_livingSurvey_202012.anorm_correct
        )
    ) %>%
  data.frame()

df.correction_abn <- 
  df.correction_abn %>%
        left_join(
          df.col_info[,c("orig_name","col_name")],suffix = c("",".flg"),
          by = c("orig_name.flg"="orig_name")
          ) %>% 
        left_join(
          df.col_info[,c("orig_name","col_name")],suffix = c("",".targ"),
          by = c("orig_name.targ"="orig_name")
          )
  
for(target.col in unique(df.correction_abn$col_name.targ)){
  test2 <-
    df.correction_abn[
      df.correction_abn$col_name.targ==target.col,
      ] %>%
    pivot_wider(
      id_col = seq, names_sep = "+NAMES_SEP+",
      names_from = c("col_name","col_name.targ"),
      values_from = c("var.flg","var.overwrite")
      )
  colnames(test2)[2] <- gsub("(var.flg\\+NAMES_SEP\\+|var.overwrite\\+NAMES_SEP\\+)(.+)\\+NAMES_SEP\\+(.+)","\\2",colnames(test2))[3]
  colnames(test2)[3] <- gsub("(var.flg\\+NAMES_SEP\\+|var.overwrite\\+NAMES_SEP\\+)(.+)\\+NAMES_SEP\\+(.+)","\\3",colnames(test2))[3]

  targ.ID <-
    left_join(
      x=data.frame(test2),#[,colnames(test2)[2]],
      y=data.frame(df.imported_data.completed),#[,colnames(test2)[2]],
      by=colnames(test2)[2]
      )[,var.ID$col_name]
  
  df.imported_data.completed[
    df.imported_data.completed[,var.ID$col_name]%in%targ.ID,
    colnames(test2)[3]
    ] <- 
    unname(test2[,3])
  }


# Variable transformation -------------------------------------------------

for(
  i in 1:length(var.trans[var.trans$col_type!="skip",]$col_name)
  ){
  if(nrow(var.trans)<1) break()
  transform <-
    gsub(
      "^(.+)\\((.+)\\)$", "\\1",
      var.trans[var.trans$col_type!="skip",]$transform[i]
      )
  if(length(transform)==0) transform <- "identity"
  args <-
    strsplit(
      gsub(
        "(.+)\\((.+))$", "\\2",
        var.trans[var.trans$col_type!="skip",]$transform[i]
        ),
      split = ","
      ) %>%
    unlist() %>%
    as.list()
    df.imported_data.completed[
      ,
      var.trans[var.trans$col_type!="skip",]$col_name[i]
      ] <-
      do.call(
        what = transform,
        args = args
        )
    }

# Classification based on pre-specified cutoff ------------------------------------------------------------------
var.label.orig_data <- 
  var.label[
    !is.na(var.label[,"orig_name"]) & 
      var.label[,"col_type"]!="skip",
    ]
for(
  i in 
  1:nrow(var.label.orig_data)
  ){
  df.imported_data.completed[,var.label.orig_data[i,"col_name"]] <-
    factor(
      df.imported_data.completed[,var.label.orig_data[i,"col_name"]],
      unlist(
        strsplit(
          gsub("^\\{(.+)\\}$","\\1",var.label.orig_data[i,"var.level"]),
          split = "\\}\\{"
          )
        ),
    unlist(
      strsplit(
        gsub("^\\{(.+)\\}$","\\1",var.label.orig_data[i,"var.label"]),
        split = "\\}\\{"
        )
      )
    )
  print(head(df.imported_data.completed[,var.label.orig_data[i,"col_name"]]))
}


df.discretized <-
  trans.var_to_score(
    col_info.var.levels = var.cutoff,
    var.levels="cutoff",
    var.score="var.score",
    orig_name="transform",
    col_name_scored = "col_name",
    input.data = df.imported_data.completed
    ) %>%
  t() %>%
  data.frame()

df.imported_data.completed <-
  bind_cols(df.imported_data.completed, df.discretized)
  
# test <- df.imported_data.completed[
#   ,
#   grep("WeightChange",colnames(df.imported_data.completed))
#   ]


# Format labels -------------------------------------------------------------------------
var.label.transformed_data <- 
  var.label[
    is.na(var.label[,"orig_name"]) &
      var.label[,"col_type"]!="skip",
    ]
var.label.transformed_data <- 
  plyr::ddply(
    var.label.transformed_data,
    plyr::.(col_name),
    function(vec){
      if(is.na(vec[,"var.level"])){
        vec["var.level"] <- 
          unique(df.imported_data.completed[,vec[,"col_name"]])[
            order(
              unique(df.imported_data.completed[,vec[,"col_name"]])
              )
            ] %>%
          paste(collapse = "}{")
        }else vec[,"var.level"] <- vec[,"var.level"]
        return(vec)
        }
    )

for(i in 1:nrow(var.label.transformed_data)){
  df.imported_data.completed[,var.label.transformed_data[i,"col_name"]] <-
    factor(
      df.imported_data.completed[,var.label.transformed_data[i,"col_name"]],
      unlist(
        strsplit(
          gsub("^\\{(.+)\\}$","\\1",var.label.transformed_data[i,"var.level"]),
          split = "\\}\\{"
          )
        ),
    unlist(
      strsplit(
        gsub("^\\{(.+)\\}$","\\1",var.label.transformed_data[i,"var.label"]),
        split = "\\}\\{"
        )
      )
    )
  }

# Exclude subjects based on the strata.out/in.
for(i in 1:nrow(var.strata_in)){
  df.imported_data.completed <-
    df.imported_data.completed[
      as.character(df.imported_data.completed[
        ,
        var.strata_in[i,"col_name"]
        ]) %in%
        unlist(
          strsplit(
            gsub("^\\{(.+)\\}$","\\1",var.strata_in[i,"strata_in"]),
            split = "\\}\\{"
            )
          )
    ,]
  }


for(i in 1:nrow(var.strata_in)){
  df.imported_data.completed <-
    df.imported_data.completed[
      df.imported_data.completed[
        ,
        var.strata_in[i,"col_name"]
        ] %in%
        unlist(
          strsplit(
            gsub("^\\{(.+)\\}$","\\1",var.strata_in[i,"strata_in"]),
            split = "\\}\\{"
            )
          )
    ,]
  }


res.summ.tableOne <-
  tableone::CreateTableOne(
    data= 
      df.imported_data.completed[
        ,
        df.col_info[
          is.na(df.col_info$ID) & 
            !is.na(df.col_info$col_name) & 
            df.col_info$col_type!="skip",
          "col_name"
          ]
      ]
    )
  

save(
  list = ls(pattern = "^var\\."),
  df.col_info,
  df.imported_data.completed, 
  res.summ.tableOne,
  file = 
    sprintf("%s/%s",
      dir.ADS_livingSurvey_202012,
      fn.ADS_livingSurvey_202012
      )
  )

sink(sprintf("%s/%s",dir.output,'Table_1.livingSurvey_202012.txt'))
print(res.summ.tableOne)
sink()

# quartz(
#   family = 'Arial',
#   type = 'pdf',
#   file = sprintf("%s/%s",dir.output,"cov_rel.pairwise.pdf"),
#   width=70,
#   height=70
#   )
# GGally::ggpairs(
#   df.imported_data.completed[
#     ,
#     df.col_info[
#       is.na(df.col_info$ID) & 
#         !is.na(df.col_info$col_name) &
#         df.col_info$col_type !="skip",
#       "col_name"
#       ]
#     ]
#   )
# dev.off()
# # End --------------------