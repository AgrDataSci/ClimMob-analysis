## Packages ####
library("readxl")
library("here")
library("janitor")

# Read the data
list.files("dev/data/sp_taste/")

dt <- read_xls(here("dev/data/sp_taste/", "COMMUNITY Tasting COMPLETE DATA_Tricot study.xls"),
               na = "NA")
names(dt) <- make_clean_names(names(dt))

dt <- as.data.frame(as.matrix(dt))

# add project name
dt$project_name <- "Community tasting"

# Organise colnames as required by ClimMob
names(dt)[names(dt)=="best_code"] <- "best_overallperf"
names(dt)[names(dt)=="worst_code"] <- "worst_overallperf"
names(dt)[names(dt)=="gender"] <- "REG_gender"
names(dt)[names(dt)=="name_a"] <- "package_item_A"
names(dt)[names(dt)=="name_b"] <- "package_item_B"
names(dt)[names(dt)=="name_c"] <- "package_item_C"

# Gender
summary(as.factor(dt$REG_gender))

dt$REG_gender <- ifelse(dt$REG_gender == "Female", "Woman", 
                        ifelse(dt$REG_gender == "Male", "Man", dt$REG_gender))

summary(as.factor(dt$REG_gender))
# Check item names
unique(unlist(dt[,paste0("package_item_", LETTERS[1:3])]))

# remove equal best and worst
out <- dt$best_overallperf == dt$worst_overallperf
dt$best_overallperf[out] <- NA
dt$worst_overallperf[out] <- NA


cmdata <- dt

# Now fix the answers as required by ClimMob

# They must have A, B or C
charpattern <- c("_color","_taste")

for(i in seq_along(charpattern)) {
  index <- charpattern[i]
  b <- paste0("best", index)
  w <- paste0("worst", index)
  
  cmdata[,b] <- ifelse(cmdata[,b] == "Yes", cmdata$best_overallperf, cmdata[,b])
  cmdata[,w] <- ifelse(cmdata[,w] == "Yes", cmdata$worst_overallperf, cmdata[,w])
  
  overall <- cmdata[,c("best_overallperf", "worst_overallperf")]
  
  nr <- nrow(overall)

  for(j in seq_len(nr)){
    # the overall best and worst for that observer
    opt <- as.vector(t(overall[j,]))
    # the answer for the characteristic for that observer
    bw <- as.vector(t(cmdata[j,c(b,w)]))
    # if NA, then all is NA
    if (any(is.na(opt))) {
      cmdata[j, c(b,w)] <- c(NA, NA)
      cmdata[j,c("best_overallperf", "worst_overallperf")] <- c(NA, NA)
      next
    }
    # if both No then this require a reordering
    # the best will be the one not mentioned here
    # the middle will be the one placed as worst in overall
    # the worst will be the one placed as best in overall
    if (all(bw == "No")) {
      cmdata[j, b] <- LETTERS[1:3][!LETTERS[1:3] %in% opt]
      cmdata[j, w] <- opt[1]
    }
   
    # if the overall worst get No for that characteristic than it is placed and the middle one
    if (all((bw == "No") == c(FALSE, TRUE))) {
      cmdata[j, w] <- LETTERS[1:3][!LETTERS[1:3] %in% opt]
    }
    
    # if the overall best get No for that characteristic than it is placed and the middle one
    if (all((bw == "No") == c(TRUE, FALSE))) {
      cmdata[j, b] <- LETTERS[1:3][!LETTERS[1:3] %in% opt]
    }
    
  }
    
}

# ................................
# Make list of parameters ####
charpattern <- union("_overallperf", charpattern)
charpattern <- paste0(charpattern, "$")
newname <- c("Overall Characteristic","Color","Taste","Other")

chars <- data.frame()

for(i in seq_along(charpattern)){
  index <- charpattern[i]
  index <- which(grepl(index, names(cmdata)))
  
  ch <- data.frame(quest_1 = names(cmdata)[index[1]],
                   quest_2 = names(cmdata)[index[2]],
                   n_quest = 2,
                   char_full = newname[i],
                   char = newname[i])
  
  chars <- rbind(chars, ch)
  
}


perf <- NULL

expl <- data.frame(name = c("Gender","District"),
                   id = NA,
                   vars = c("REG_gender","district"))


pars <- list(chars = chars, expl = expl, perf = perf)
rm(ch, dt, chars, expl, perf, i, charpattern, newname, index)

# get the arguments from 
tag <- "community_tasting"
pathname    <- paste0("dev/output/",tag,"/")
infosheets  <- FALSE
language    <- "en"
extension   <- "docx"
ranker      <- "participant"
option      <- "variety"
fullpath    <- getwd()

# ................................................................
# ................................................................
# Run analysis ####
dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

source(paste0(fullpath, "/R/analysis_climmob.R"))

# ................................................................
# ................................................................
# Write outputs ####
#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))

# produce main report if output type is "summary" or "both"
rmarkdown::render(paste0(fullpath, "/report/", language, "/mainreport/mainreport.Rmd"),
                  output_dir = pathname,
                  output_format = output_format,
                  output_file = paste0(projname,"_report",".",extension))

