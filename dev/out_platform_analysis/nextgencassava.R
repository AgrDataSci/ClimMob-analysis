## Packages ####
library("readxl")
library("here")
library("janitor")

list.files("data")

dt <- read_excel("dev/data/nextgencassava/nextgencassava.xlsx", na = c(".", " "))

dt <- as.data.frame(dt)

# .............................
# .............................
# fix col names in dt ####
n1 <- as.vector(names(dt))
n2 <- as.vector(t(dt[1,]))
n3 <- as.vector(t(dt[2,]))

n <- paste(n1, n2, "_", n3, collapse = ",")

n <- gsub("NA|[...]|[/]| |[0-9]+", "", n)
n <- gsub("\\s*\\([^\\)]+\\)", "", n)

n <- strsplit(tolower(n), ",")[[1]]

index_w <- which(grepl("_worstoff", n))

chars <- n[index_w-1]

chars <- gsub("_best","",chars)

n[index_w] <- paste0(chars, n[index_w])

n <- gsub("imonth|month|off", "", n)

n[15:24] <- paste0("first_month_", n[15:24])

n[25:36] <- paste0("third_month_", n[25:36])

n[37:48] <- paste0("sixth_month_", n[37:48])

n[49:58] <- paste0("nineth_month_", n[49:58])

n <- gsub("_$", "", n)

names(dt) <- n

# remove first two rows since it is the col names
dt <- dt[-c(1:2), ]


# .............................
# .............................
# fix variety names ####
vars <- paste0("variety", letters[1:3])
itemnames <- unique(unlist(dt[, vars]))

dt[, vars] <- lapply(dt[,vars], function(x) {
  x <- gsub("[Cassava varieties: ]", "",x)
  x <- gsub("[\r\n]", "", x)
  x <- gsub("V", "", x)
  x <- gsub(" ", "", x)
  x <- gsub("ITA-", "IITA-", x)
  x <- gsub("IIITA-", "IITA-", x)
  x <- gsub("_[0-9]+$","", x)
  x <- gsub("-[0-9]+$","", x)
  x <- gsub("IITA", "", x)
  x <- gsub("_", "", x)
  x <- gsub("-", "", x)
})

dt <- dt[!is.na(dt$varietya), ]

itemnames <- sort(unique(unlist(dt[, vars])))

itemnames

# .............................
# .............................
# check explanatory variables
variables <- c("sex","communityme")

dt[, variables[1]] <- ifelse(dt[, variables[1]] == "Male", "M",
                             ifelse(dt[, variables[1]] == "Female", "F",
                                    dt[, variables[1]]))

#keep <- dt$sex == "F"
#dt <- dt[keep, ]

names(dt)

# Organise colnames as required by ClimMob
names(dt)[names(dt)=="nineth_month_suitabilitytosoilandenvironment_best"] <- "overallperf_best"
names(dt)[names(dt)=="nineth_month_suitabilitytosoilandenvironment_worst"] <- "overallperf_worst"
names(dt)[names(dt)=="sex"] <- "REG_gender"
names(dt)[names(dt)=="varietya"] <- "package_item_A"
names(dt)[names(dt)=="varietyb"] <- "package_item_B"
names(dt)[names(dt)=="varietyc"] <- "package_item_C"

# Gender
summary(as.factor(dt$REG_gender))

# Check item names
unique(unlist(dt[,paste0("package_item_", LETTERS[1:3])]))

cmdata <- dt

# Now fix the answers as required by ClimMob

# ................................
# Make list of parameters ####
charpattern <- union("overallperf", unique(gsub("_best|_worst","", names(dt)[15:56])))
charpattern <- paste0(charpattern)
# newname <- c("Overall Characteristic","1st month overall","1st month germination",
#              "1st month plant growth", "1st month weed competitiveness")
newname <- union("overall", unique(gsub("_best|_worst","", names(dt)[15:56])))

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

expl <- data.frame(name = c("Gender","Community"),
                   id = NA,
                   vars = c("REG_gender","communityme"))


pars <- list(chars = chars, expl = expl, perf = perf)
rm(ch, dt, chars, expl, perf, i, charpattern, newname, index)


# remove ties
for(i in seq_along(pars$chars$quest_1)) {
  
  b <- pars$chars$quest_1[i]
  w <- pars$chars$quest_2[i]
  
  o <- cmdata[, b] == cmdata[, w] 
  
  o <- ifelse(is.na(o), TRUE, o)
  
  
  cmdata[o, b] <- NA
  cmdata[o, w] <- NA
  
  
}

# get the arguments from 
cmdata$project_name <- "Cassava"
tag <- "cassava"
pathname    <- paste0("dev/output/",tag,"/")
infosheets  <- FALSE
language    <- "en"
extension   <- "docx"
ranker      <- "farmer"
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

