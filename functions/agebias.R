agebias <- function(title, id, x=reader1, y=reader2){
  filename <- here::here("data", paste0(id, ".csv"))
  dat <- read.csv(filename, stringsAsFactors = FALSE)
  dat$x <- round(dat[[x]])
  dat$y <- round(dat[[y]])
  dat <- dat[complete.cases(y),] #only look at records that have a second age reader
  ap.pz<-FSA::agePrecision(~x+y, data=dat)
  summary(ap.pz, what="precision") #out put of IAPE and CV
  summary(ap.pz, what="difference", digits = 1) #table of differences on age estimates
}
