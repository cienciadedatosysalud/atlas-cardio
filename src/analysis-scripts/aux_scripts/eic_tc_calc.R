database_path <- '../../inputs/data.duckdb'

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

# INDICADORES
## ENFERMEDAD ISQUÉMICA CORONARIA


# Cuidados ambulatorios

### - Enfermedad isquémica cardiaca en la población mayor de 18 años.

### Total

df_eic_global <- data.frame()

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 1

df_eic_global <- rbind(df_eic_global,data_csv)


### Hombres

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and sexo_cd == 1
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 2

df_eic_global <- rbind(df_eic_global,data_csv)
### Mujeres

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and sexo_cd == 2
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 3

df_eic_global <- rbind(df_eic_global,data_csv)
### NS bajo

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 4

df_eic_global <- rbind(df_eic_global,data_csv)
### NS Alto

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 5

df_eic_global <- rbind(df_eic_global,data_csv)

df_eic_global$ind <- 'eic'

dbDisconnect(con, shutdown=TRUE)


### - Población con EIC vacunada de la gripe el año de estudio.
### - Ecocardiograma el año de estudio. 
### - Determinación/registro de la fracción de eyección el año de estudio. 

df_eic_exams <- data.frame()


df_ind_exams <- data.frame(ind = c('vacuna_gripe','ecocardio'),
                           descr = c('Vacunación antigripal','Ecocardiograma'),
                           cod = c('eic_gripe','eic_eco'))

eic_indicadores_exam <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023"))
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  df_ind_exams_ <- df_ind_exams %>% filter(ind %in% i)
  
  
  data_csv$tipo_analisis <- 1
  
  df_eic_exams <- rbind(df_eic_exams,data_csv)
  
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_eic_exams <- rbind(df_eic_exams,data_csv)
  
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_eic_exams <- rbind(df_eic_exams,data_csv)
  ### NS Bajo ###
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_eic_exams <- rbind(df_eic_exams,data_csv)
  
  ### NS Alto ###
  
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023"))
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_eic_exams <- rbind(df_eic_exams,data_csv)
  df_eic_exams$ind <- df_ind_exams_$cod
  
  dbDisconnect(con, shutdown=TRUE)            
  return(df_eic_exams)
}


df_eic_exams_all <- lapply(df_ind_exams$ind,eic_indicadores_exam)

df_eic_exams_all <- rbind(df_eic_exams_all[[1]],df_eic_exams_all[[2]])

### - Población con EIC e infarto de miocardio previo.

df_eic_iam_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.comorbilidad WHERE comorbilidad_cd == 1"))

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df <- df[!duplicated(df$paciente_id),]
df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
df_eic <- left_join(df_eic,df,by='paciente_id')
df_eic <- df_eic %>% filter(fecha_diag_comorbilidad_dt <= fecha_enfermedad_dt)
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 1

df_eic_iam_all <- rbind(df_eic_iam_all,data_csv)

### HOMBRES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))


df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
df_eic <- left_join(df_eic,df,by='paciente_id')
df_eic <- df_eic %>% filter(fecha_diag_comorbilidad_dt <= fecha_enfermedad_dt)
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 2

df_eic_iam_all <- rbind(df_eic_iam_all,data_csv)

### MUJERES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))


df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
df_eic <- left_join(df_eic,df,by='paciente_id')
df_eic <- df_eic %>% filter(fecha_diag_comorbilidad_dt <= fecha_enfermedad_dt)
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 3

df_eic_iam_all <- rbind(df_eic_iam_all,data_csv)
### NS Bajo ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))


df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
df_eic <- left_join(df_eic,df,by='paciente_id')
df_eic <- df_eic %>% filter(fecha_diag_comorbilidad_dt <= fecha_enfermedad_dt)
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 4

df_eic_iam_all <- rbind(df_eic_iam_all,data_csv)

### NS Alto ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))


df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
df_eic <- left_join(df_eic,df,by='paciente_id')
df_eic <- df_eic %>% filter(fecha_diag_comorbilidad_dt <= fecha_enfermedad_dt)
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 5

df_eic_iam_all <- rbind(df_eic_iam_all,data_csv)

df_eic_iam_all$ind <- 'eic_iam'

dbDisconnect(con, shutdown=TRUE)  




### - Registro de tensión arterial (TA).
### - Determinación de colesterol LDL.      


df_eic_test <- data.frame()


df_ind_test <- data.frame(ind = c('pas','ldl'),
                          descr = c('Tensión arterial','Colesterol LDL (mg/dL)'),
                          cod = c('eic_ta','eic_ldl'))

eic_indicadores_test <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  if(i == 'pas'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd IN ('pas','pad') and year(test_dt) == 2023"))
    df <- df %>%
      group_by(paciente_id) %>%
      filter(all(c("pas", "pad") %in% test_cd)) %>%
      ungroup()
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",i,"' and year(test_dt) == 2023"))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  df_ind_test_ <- df_ind_test %>% filter(ind %in% i)
  
  
  data_csv$tipo_analisis <- 1
  
  df_eic_test <- rbind(df_eic_test,data_csv)
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  
  data_csv$tipo_analisis <- 2
  
  df_eic_test <- rbind(df_eic_test,data_csv)
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  
  data_csv$tipo_analisis <- 3
  
  df_eic_test <- rbind(df_eic_test,data_csv)
  ### NS Bajo ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  
  data_csv$tipo_analisis <- 4
  
  df_eic_test <- rbind(df_eic_test,data_csv)
  
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  
  data_csv$tipo_analisis <- 5
  
  df_eic_test <- rbind(df_eic_test,data_csv)
  
  df_eic_test$ind <- df_ind_test_$cod
  
  dbDisconnect(con, shutdown=TRUE)   
  return(df_eic_test)
}


df_eic_test_all <-lapply(df_ind_test$ind,eic_indicadores_test)  

df_eic_test_all <- rbind(df_eic_test_all[[1]],df_eic_test_all[[2]])

### - TA <140/90 mmHg (en la última determinación).
### - Determinación de colesterol LDL menor de 70 mg/dl 
### - Determinación de colesterol LDL menor de 55 mg/dl

df_eic_test_nm <- data.frame()

df_ind_test <- data.frame(ind = c('pas','ldl','ldl'),
                          descr = c('Tensión arterial','Colesterol LDL menor de 70 (mg/dL)','Colesterol LDL menor de 55 (mg/dL)'),
                          cod = c('eic_ta_ctrl','eic_ldl70','eic_ldl55'),
                          nm=c(140,70,55))

eic_indicadores_test_nm <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_ind_test_ <- df_ind_test %>% filter(cod %in% i)
  if(df_ind_test_$cod == 'eic_ta_ctrl'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd IN ('pas','pad') and year(test_dt) == 2023"))
    df <- df %>%
      group_by(paciente_id) %>%
      filter(all(c("pas", "pad") %in% test_cd)) %>%
      ungroup()
    df <- df %>% 
      group_by(paciente_id, test_cd) %>%
      filter(test_dt == max(test_dt)) %>%
      ungroup()
    df1 <- df %>% filter(test_cd == 'pas' & test_resultado_nm < 140)
    df2 <- df %>% filter(paciente_id %in% df1$paciente_id) %>% filter(test_cd == 'pad' & test_resultado_nm < 90)
    df <- df %>% filter(paciente_id %in% df2$paciente_id)
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",df_ind_test_$ind,"' and test_resultado_nm < '",df_ind_test_$nm,"' and year(test_dt) == 2023"))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_eic_test_nm <- rbind(df_eic_test_nm,data_csv)
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_eic_test_nm <- rbind(df_eic_test_nm,data_csv)
  
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_eic_test_nm <- rbind(df_eic_test_nm,data_csv)
  ### NS Bajo ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_eic_test_nm <- rbind(df_eic_test_nm,data_csv)
  
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  
  data_csv$tipo_analisis <- 5
  
  df_eic_test_nm <- rbind(df_eic_test_nm,data_csv)
  
  df_eic_test_nm$ind <- df_ind_test_$cod
  
  dbDisconnect(con, shutdown=TRUE)   
  return(df_eic_test_nm)
}




df_eic_test_all_nm <- lapply(df_ind_test$cod,eic_indicadores_test_nm) 

df_eic_test_all_nm <- rbind(df_eic_test_all_nm[[1]],df_eic_test_all_nm[[2]],df_eic_test_all_nm[[3]])


# Prescripción farmacéutica
### - Antagonistas de la aldosterona.
### - Betabloqueantes.
### - Antiagregantes.
### - Ranolazina.
### - Trimetazidina.
### - Nitratos.
### - Bloqueantes del canal de calcio.
### - Agentes activos sobre el sistema renina-angiotensina.
### - iSGLT2 
### - arGLP-1


df_eic_atc <- data.frame()

df_atc_tratamiento <- data.frame(ind = c('C03DA','C07','B01AC','C01EB18','C01EB15','C01DA','C08','C09',
                                         'A10B','A10BJ'),
                                 filter = c('C03DA','C07','B01AC','C01EB18','C01EB15','C01DA','C08','C09',
                                            'A10BK02|A10BD16|A10BK03|A10BD20|A10BD19','A10BJ02|A10BJ06|A10BJ05'),
                                 cod = c('eic_antag_aldo','eic_bbloc','eic_antiagreg','eic_ranol','eic_trimet',
                                         'eic_nitra','eic_bloc_ca','eic_sra','eic_isglt2','eic_glp1'))

eic_indicadores_tratamiento <- function(i){
  
  
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento %>% filter(ind %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",i,"%'"))
  if(df_atc_tratamiento_$ind == 'A10B' | df_atc_tratamiento_$ind == 'A10BJ'){
    df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_eic_atc <- rbind(df_eic_atc,data_csv)
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_eic_atc <- rbind(df_eic_atc,data_csv)
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_eic_atc <- rbind(df_eic_atc,data_csv)
  ### NS Bajo ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_eic_atc <- rbind(df_eic_atc,data_csv)
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_eic_atc <- rbind(df_eic_atc,data_csv)
  df_eic_atc$ind <- df_atc_tratamiento_$cod
  
  dbDisconnect(con, shutdown=TRUE)            
  return(df_eic_atc)
}

df_eic_atc_all <- lapply(df_atc_tratamiento$ind,eic_indicadores_tratamiento)


df_eic_atc_all <- rbind(df_eic_atc_all[[1]],df_eic_atc_all[[2]],df_eic_atc_all[[3]],
                        df_eic_atc_all[[4]],df_eic_atc_all[[5]],df_eic_atc_all[[6]],
                        df_eic_atc_all[[7]],df_eic_atc_all[[8]],
                        df_eic_atc_all[[9]],df_eic_atc_all[[10]])

### - Población con EIC y diabetes 

df_eic_dm2_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.comorbilidad WHERE comorbilidad_cd == 2"))

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df <- df[!duplicated(df$paciente_id),]
df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 1

df_eic_dm2_all <- rbind(df_eic_dm2_all,data_csv)
### HOMBRES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))


df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 2

df_eic_dm2_all <- rbind(df_eic_dm2_all,data_csv)

### MUJERES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))

df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 3

df_eic_dm2_all <- rbind(df_eic_dm2_all,data_csv)

### NS Bajo ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))

df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 4

df_eic_dm2_all <- rbind(df_eic_dm2_all,data_csv)
### NS Alto ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))

df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 5

df_eic_dm2_all <- rbind(df_eic_dm2_all,data_csv)

df_eic_dm2_all$ind <- 'eic_dm2'
dbDisconnect(con, shutdown=TRUE) 


### - iSGLT2 (en población con diabetes)
### - arGLP-1 (en población con diabetes).

df_eic_atc_dm <- data.frame()

df_atc_tratamiento_dm <- data.frame(ind = c('A10B','A10BJ'),
                                    filter = c('A10BK02|A10BD16|A10BK03|A10BD20|A10BD19','A10BJ02|A10BJ06|A10BJ05'),
                                    cod = c('eic_dm2_isglt2','eic_dm2_glp1'))

eic_indicadores_tratamiento_diabetes <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento_dm %>% filter(ind %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",i,"%'"))
  if(df_atc_tratamiento_$ind == 'A10B' | df_atc_tratamiento_$ind == 'A10BJ'){
    df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_eic_atc_dm <- rbind(df_eic_atc_dm,data_csv)
  
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND sexo_cd ==1
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_eic_atc_dm <- rbind(df_eic_atc_dm,data_csv)
  
  ### MUJERES ###
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND sexo_cd ==2
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_eic_atc_dm <- rbind(df_eic_atc_dm,data_csv)
  ### NS Bajo ###
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND nivel_copago_cd == 0
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))    
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_eic_atc_dm <- rbind(df_eic_atc_dm,data_csv)
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND nivel_copago_cd == 1
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))  
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_eic_atc_dm <- rbind(df_eic_atc_dm,data_csv)
  
  df_eic_atc_dm$ind <- df_atc_tratamiento_$cod
  
  
  dbDisconnect(con, shutdown=TRUE)     
  return(df_eic_atc_dm)
}


df_eic_atc_dm_all <- lapply(df_atc_tratamiento_dm$ind,eic_indicadores_tratamiento_diabetes)

df_eic_atc_dm_all <- rbind(df_eic_atc_dm_all[[1]],df_eic_atc_dm_all[[2]])

### - iSGLT2 (en población sin diabetes)
### - arGLP-1 (en población sin diabetes).

df_eic_atc_sin_dm <- data.frame()

df_atc_tratamiento_sin_dm <- data.frame(ind = c('A10B','A10BJ'),
                                    filter = c('A10BK02|A10BD16|A10BK03|A10BD20|A10BD19','A10BJ02|A10BJ06|A10BJ05'),
                                    cod = c('eic_sin_dm2_isglt2','eic_sin_dm2_glp1'))

eic_indicadores_tratamiento_sin_diabetes <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento_sin_dm %>% filter(ind %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",i,"%'"))
  df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  
  pobla_dm <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  
  
  pobla <- pobla %>% filter(paciente_id %nin% pobla_dm$paciente_id)
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_eic_atc_sin_dm <- rbind(df_eic_atc_sin_dm,data_csv)
  
  ### HOMBRES ###
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd==1"))
  
  pobla_dm <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND sexo_cd ==1
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  
  pobla <- pobla %>% filter(paciente_id %nin% pobla_dm$paciente_id)
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_eic_atc_sin_dm <- rbind(df_eic_atc_sin_dm,data_csv)
  
  ### MUJERES ###
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  pobla_dm <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND sexo_cd ==2
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  
  pobla <- pobla %>% filter(paciente_id %nin% pobla_dm$paciente_id)
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_eic_atc_sin_dm <- rbind(df_eic_atc_sin_dm,data_csv)
  ### NS Bajo ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  pobla_dm <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND nivel_copago_cd == 0
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))    
  pobla <- pobla %>% filter(paciente_id %nin% pobla_dm$paciente_id)
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_eic_atc_sin_dm <- rbind(df_eic_atc_sin_dm,data_csv)
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  pobla_dm <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.*
                          FROM main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND nivel_copago_cd == 1
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))  
  pobla <- pobla %>% filter(paciente_id %nin% pobla_dm$paciente_id)
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_eic %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_eic_atc_sin_dm <- rbind(df_eic_atc_sin_dm,data_csv)
  
  df_eic_atc_sin_dm$ind <- df_atc_tratamiento_$cod
  
  
  dbDisconnect(con, shutdown=TRUE)     
  return(df_eic_atc_sin_dm)
}

df_eic_atc_sin_dm_all <- lapply(df_atc_tratamiento_sin_dm$ind,eic_indicadores_tratamiento_sin_diabetes)

df_eic_atc_sin_dm_all <- rbind(df_eic_atc_sin_dm_all[[1]],df_eic_atc_sin_dm_all[[2]])

df_all <- rbind(df_eic_global,df_eic_exams_all,df_eic_iam_all,df_eic_test_all,df_eic_test_all_nm,df_eic_atc_all,df_eic_dm2_all,df_eic_atc_dm_all,df_eic_atc_sin_dm_all)

write.table(df_all,'../../outputs/eic_ind_tablas.csv',sep='|',row.names = FALSE)

