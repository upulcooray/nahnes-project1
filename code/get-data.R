
library(tidyverse)


fast.rbind <- function(...,method=c("fill","common"),value=NA){
  if("fill"==method[1]) {
    fun1 <- function(x,y,value=NA){
      x[setdiff(colnames(y),colnames(x))] <- value

      y[setdiff(colnames(x),colnames(y))] <- value

      return(rbind(x,y))
    }
  }

  if("common"==method[1]) {
    fun1 <- function(x,y,value=NULL){
      common_cols <- intersect(colnames(x), colnames(y))
      return(rbind(x[, common_cols,drop=F],y[, common_cols,drop=F]))
    }
  }
  return(Reduce(function(x,y){fun1(x=x,y=y,value=value)},list(...)))
}




library(nhanesA)


nhanesTables(data_group = "DEMO", year = 2017)
nhanesTables(data_group = "EXAM", year = 2014)


demo <- do.call(fast.rbind,lapply(c("DEMO_H","DEMO_I","DEMO_J"), nhanes))
ohq <- do.call(fast.rbind,lapply(c("OHQ_H","OHQ_I","OHQ_J"), nhanes))
ohex <- do.call(fast.rbind,lapply(c("OHXDEN_H","OHXDEN_I","OHXDEN_J"), nhanes))
smok <- do.call(fast.rbind,lapply(c("SMQ_H","SMQ_I","SMQ_J"), nhanes))
alco <- do.call(fast.rbind,lapply(c("ALQ_H","ALQ_I","ALQ_J"), nhanes))
diet1 <- do.call(fast.rbind,lapply(c("DR1TOT_H","DR1TOT_I","DR1TOT_J"), nhanes))
diet2 <- do.call(fast.rbind,lapply(c("DR2TOT_H","DR2TOT_I","DR2TOT_J"), nhanes))
health <- do.call(fast.rbind,lapply(c("HSQ_H","HSQ_I","HSQ_J"), nhanes))
body<- do.call(fast.rbind,lapply(c("BMX_H","BMX_I","BMX_J"), nhanes))


nhanes-data <- merge(demo,ohq,ohex,smok,alco,diet1,diet2,health,body,by = )

nhanes_data <- Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE) ,list(demo,ohq,ohex,smok,alco,diet1,diet2,health,body))

demo_h_vars  <- nhanesTableVars('DEMO', 'DEMO_H', namesonly=TRUE)
ohq_h_vars  <- nhanesTableVars('Q', 'OHQ_H', namesonly=TRUE)
ohex_h_vars  <- nhanesTableVars('EXAM', 'OHXDEN_H', namesonly=TRUE)
smok_h_vars  <- nhanesTableVars('Q', 'SMQ_H', namesonly=TRUE)
alco_h_vars  <- nhanesTableVars('Q', 'ALQ_H', namesonly=TRUE)
health_h_vars  <- nhanesTableVars('Q', 'HSQ_H', namesonly=TRUE)
body_h_vars  <- nhanesTableVars('EXAM', 'BMX_H', namesonly=TRUE)

all.names <- c(demo_h_vars,ohq_h_vars,ohex_h_vars,smok_h_vars,alco_h_vars,health_h_vars,body_h_vars)


#Alternatively may use bpx_d_vars = names(bpx_d)
nhanes_data_temp <- suppressWarnings(nhanesTranslate('DEMO_H', demo_h_vars, data=nhanes_data))
nhanes_data_temp <- suppressWarnings(nhanesTranslate('OHQ_H', ohq_h_vars, data=nhanes_data_temp))
nhanes_data_temp <- suppressWarnings(nhanesTranslate('OHXDEN_H', ohex_h_vars, data=nhanes_data_temp))
nhanes_data_temp <- suppressWarnings(nhanesTranslate('SMQ_H', smok_h_vars, data=nhanes_data_temp))
nhanes_data_temp <- suppressWarnings(nhanesTranslate('ALQ_H', alco_h_vars, data=nhanes_data_temp))
nhanes_data_temp <- suppressWarnings(nhanesTranslate('HSQ_H', health_h_vars, data=nhanes_data_temp))

nhanes_data_lab <- suppressWarnings(nhanesTranslate('BMX_H', body_h_vars , data=nhanes_data_temp))


save(nhanes_data, file = "data/nhanes_data.R")
save(nhanes_data_lab, file = "data/nhanes_data_labelled.R")
save(demo,ohq,ohex,smok,alco,diet1,diet2,health,body, file = "data/nhanes_subsections.R")

# Select variables for the analysis

q_vars<- nhanes_data_lab %>%  select(
  id= SEQN ,
  psu= SDMVPSU,
  strata= SDMVSTRA,
  int_ex_status=RIDSTATR,
  int_wt= WTINT2YR,
  exam_wt= WTMEC2YR,
  cycle= SDDSRVYR,
  Age= RIDAGEYR,
  Sex= RIAGENDR,
  Married= DMDMARTL,
  Income= INDFMIN2,
  poverty= INDFMPIR,
  Education= DMDEDUC2,
  Ethnicity= RIDRETH1,
  hh_size= DMDHHSIZ,
  den_visit= OHQ033,
  den_pain= OHQ620,
  sroh= OHQ845,
  srgh= HSD010,
  brush_fq=OHQ848Q,
  bmi=BMXBMI,
  sugar1= DR1TSUGR,
  sugar2= DR2TSUGR,
  smoke= SMQ040)

d_data <- nhanes_data_lab %>%  select(matches("OHX[0-9][0-9]TC"))

nhanes_df <- cbind(q_vars,d_data)

save(nhanes_df, file = "data/working_df.R")
