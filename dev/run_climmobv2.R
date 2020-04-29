# Make pars for projects in ClimMob v2.1
library("janitor")

list.dirs("dev/data")

path <- "dev/data/nic_apante_2015/"

projname <- "nic_apante_2015"

dt <- read.csv(paste0(path, projname, ".csv"), na.strings = c(""))
pkg <- read.csv(paste0(path,"paquete_", projname, ".csv"), na.strings = c(""))

names(pkg) <- c("id", paste0("package_item_", LETTERS[1:3]))
pkg$package_project_name <- projname

head(pkg)

names(dt) <- make_clean_names(names(dt))

names(dt)[names(dt)=="codigo_paquetes"] <- "id"

dt <- dt[!is.na(dt$id), ]

dt <- merge(pkg, dt, by = "id", all.y = TRUE)

class(dt) <- union('CM_df', class(dt))

nmdt <- names(dt) 

nmdt[nmdt=="genero_m_f" | nmdt=="gender_m_f"] <- "registration_REG_gender"

nmdt[nmdt=="edad" | nmdt=="age"] <- "registration_REG_age"

nmdt[nmdt=="nombre_del_padre_o_nombre_del_marido_esposa"] <- "registration_REG_father_name"

nmdt[nmdt=="distrito_o_region"] <- "registration_REG_district"

nmdt[nmdt=="nombre_y_apellidos"] <- "package_farmername" 

nmdt[nmdt=="localizacion_latitud"] <- "firstassess_lat" 

nmdt[nmdt=="localizacion_longitud"] <- "firstassess_lon" 

nmdt[nmdt=="fecha_de_siembra_yyyy_mm_dd"] <- "firstassess_planting_date"

nmdt[nmdt=="fecha_de_cosecha_yyyy_mm_dd"] <- "firstassess_harvesting_date"

nmdt[nmdt=="riego_si_no"] <- "firstassess_irrigation"

nmdt[nmdt=="pendiente_alta_media_plana"] <- "firstassess_slope"

nmdt[nmdt=="calidad_de_suelo_buena_mala_regular"] <- "firstassess_soil_quality"

nmdt[nmdt=="cuanto_llovio_durante_la_epoca_de_cultivo_mucho_normal_poco_nada"] <- "firstassess_rain"

nmdt[nmdt=="como_llovio_continua_intermitente_no_llovio_nada"] <- "firstassess_rain_how"

nmdt[nmdt=="la_epoca_de_cultivo_fue_mas_caliente_o_mas_frio_que_de_costumbre_mas_caliente_de_lo_normal_como_siempre_mas_fria_de_lo_normal"] <- "firstassess_temperature_how"


names(dt) <- nmdt

dt

# look for the characteristics
chr <- which(grepl("caracteristica|characteristic", names(dt)))

overall <- unlist(dt[1,chr])
overall <- which(grepl("VS_Local", overall))
overall <- chr[overall]

chr <- chr[chr!=overall]

chars <- data.frame()

for(i in seq_along(chr)){
  index <- chr[i]
  char_i <- dt[1, index]
  
  newmame_i <- make_clean_names(char_i)
  newname_i <- gsub("_a_b_c","", newmame_i)
  
  names(dt)[index+1] <- paste0("ASS0dfe2b4467a3_char_", newname_i, "_pos")
  names(dt)[index+2] <- paste0("ASS0dfe2b4467a3_char_", newname_i, "_neg")

  ch <- data.frame(quest_1 = paste0("ASS0dfe2b4467a3_char_", newname_i, "_pos"),
                   quest_2 = paste0("ASS0dfe2b4467a3_char_", newname_i, "_neg"),
                   n_quest = 2,
                   char_full = NA,
                   char = newname_i)
  
  chars <- rbind(chars, ch)

}

if (length(overall) > 0L) {
  
  
  for(i in 1:3) {
    dt[,overall + i] <- ifelse(dt[,overall + i] == "Mejor", "Better", 
                               ifelse(dt[,overall + i] == "Peor", "Worse", dt[,overall + i]))
    
    names(dt)[overall+i] <- paste0("ASS0dfe2b4467a3_perf_overallchar_",i)
  }
  
  perf <- data.frame(quest_1 = names(dt)[overall+1],
                     quest_2 = names(dt)[overall+2],
                     quest_3 = names(dt)[overall+3],
                     n_quest = 3,
                     perf_full = "Overall performance",
                     perf = "overall_performance")
  
}

dt <- dt[-c(chr, overall)]


# Fix some variables
dt$registration_REG_gender <- ifelse(dt$registration_REG_gender == "M", "Man",
                                     ifelse(dt$registration_REG_gender == "F", "Woman", 
                                            dt$registration_REG_gender))

# Make the parameters
chars$char_full <- c("Vigor", "Plant size", "Resistance to pests", "Resistance to diseases",
                     "Resistance to drought", "Yield", "Market value", "Taste", 
                     "Overall Characteristic")

neworder <- match(union("Overall Characteristic", chars$char_full), chars$char_full)

chars <- chars[neworder,]


expl <- data.frame(name = c("What is the gender?","What is the age?", "Longitude", "Latitude"),
                   id = NA,
                   vars = c("REG_gender","REG_age","firstassess_lon","firstassess_lat"))



pars <- list(chars = chars, expl = expl, perf = perf)

cmdata <- dt

option <- "variety"

rm(dt,neworder,overall,projname,chr,pkg,perf,ch,expl,chars)
