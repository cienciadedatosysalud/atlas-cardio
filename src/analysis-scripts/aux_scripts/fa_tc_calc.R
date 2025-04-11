## FIBRILACIÓN AURICULAR

# Cuidados ambulatorios

### - Fibrilación Auricular en la población mayor de 18 años.

### Total

df_fa_global <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion_view where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
data_csv$tipo_analisis <- 1

df_fa_global <- rbind(df_fa_global,data_csv)


### Hombres

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion_view where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and sexo_cd == 1
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 2

df_fa_global <- rbind(df_fa_global,data_csv)

### Mujeres

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion_view where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and sexo_cd == 2
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 3

df_fa_global <- rbind(df_fa_global,data_csv)

### NS bajo

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion_view where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 4

df_fa_global <- rbind(df_fa_global,data_csv)

### NS Alto

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select  
	* from main.poblacion_view where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1
	')
df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

data_csv <- df %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(df_poblacion,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)



data_csv$tipo_analisis <- 5

df_fa_global <- rbind(df_fa_global,data_csv)

df_fa_global$ind <- 'fa'

dbDisconnect(con, shutdown=TRUE)





### - Ecocardiograma pacientes diagnosticados 2023
### - Evaluación CHA₂DS₂-VASc (CHADS2) al diagnóstico.
### - Evaluación HASBLED al diagnóstico
### - Determinación de TSH al diagnóstico

df_fa_exams_tsh <- data.frame()

df_ind_exams_tsh <- data.frame(ind = c('ecocardio','chads','HASBLED','tsh'),
                               descr = c('Ecocardiograma','Evaluación escala CHADS2Vasc','Evaluación escala HASBLED','Hormona estimulante de la tiroides'),
                               cod = c('fa_eco','fa_chads2','fa_hasbled','fa_tsh'))

fa_indicadores_exam_tsh <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  df_ind_exams_ <- df_ind_exams_tsh %>% filter(ind %in% i)
  if(i == 'tsh'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",i,"' and year(test_dt) == 2023")) 
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023")) 
  }

  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_fa_exams_tsh <- rbind(df_fa_exams_tsh,data_csv)
  
  
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_fa_exams_tsh <- rbind(df_fa_exams_tsh,data_csv)
  
  
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_fa_exams_tsh <- rbind(df_fa_exams_tsh,data_csv)
  ### NS Bajo ###
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_fa_exams_tsh <- rbind(df_fa_exams_tsh,data_csv)
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_fa_exams_tsh <- rbind(df_fa_exams_tsh,data_csv)
  
  df_fa_exams_tsh$ind <- df_ind_exams_$cod
  
  
  dbDisconnect(con, shutdown=TRUE)      
  return(df_fa_exams_tsh)
}


df_fa_exams_tsh_all <- lapply(df_ind_exams_tsh$ind,fa_indicadores_exam_tsh)

df_fa_exams_tsh_all <- rbind(df_fa_exams_tsh_all[[1]],df_fa_exams_tsh_all[[2]],df_fa_exams_tsh_all[[3]],df_fa_exams_tsh_all[[4]])




### - Determinación de función renal el año de estudio

df_fa_fg_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.test WHERE test_cd == 'fg' and year(test_dt) == 2023"))
df <- df %>% group_by(paciente_id) %>% summarise(count=n()) %>% filter(count >= 2)

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)


data_csv$tipo_analisis <- 1

df_fa_fg_all <- rbind(df_fa_fg_all,data_csv)

### HOMBRES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))

df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 2

df_fa_fg_all <- rbind(df_fa_fg_all,data_csv)

### MUJERES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))

df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 3

df_fa_fg_all <- rbind(df_fa_fg_all,data_csv)

### NS Bajo ###


pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))

df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 4

df_fa_fg_all <- rbind(df_fa_fg_all,data_csv)

### NS Alto ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))

df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 5

df_fa_fg_all <- rbind(df_fa_fg_all,data_csv)

df_fa_fg_all$ind <- 'fa_fr'

dbDisconnect(con, shutdown=TRUE)  



### Prescripción farmacéutica
### - Anticoagulantes orales.
### - Anticoagulantes orales de acción directa
### - Antagonistas de la vitamina K.
### - Antiagregantes.

df_fa_atc <- data.frame()


df_atc_tratamiento <- data.frame(ind = c('B01A','B01A','B01A','B01AC'),
                                 filter = c('B01AA|B01AE|B01AF','B01AE07|B01AF01|B01AF02|B01AF03','B01AA','B01AC'),
                                 cod = c('fa_aco','fa_acoad','fa_antagk','fa_antiagreg'))

fa_indicadores_tratamiento <- function(i){
  
  
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento %>% filter(cod %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",df_atc_tratamiento_$ind,"%'"))
  
  
  df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_fa_atc <- rbind(df_fa_atc,data_csv)
  
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_fa_atc <- rbind(df_fa_atc,data_csv)
  
  
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_fa_atc <- rbind(df_fa_atc,data_csv)
  ### NS Bajo ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_fa_atc <- rbind(df_fa_atc,data_csv)
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_fa %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_fa_atc <- rbind(df_fa_atc,data_csv)
  
  df_fa_atc$ind <- df_atc_tratamiento_$cod
  
  dbDisconnect(con, shutdown=TRUE)      
  return(df_fa_atc)           
}

df_fa_atc_all <- lapply(df_atc_tratamiento$cod,fa_indicadores_tratamiento)  


df_fa_atc_all <- rbind(df_fa_atc_all[[1]],df_fa_atc_all[[2]],df_fa_atc_all[[3]],df_fa_atc_all[[4]])


df_all <- rbind(df_fa_global,df_fa_exams_tsh_all,df_fa_fg_all,df_fa_atc_all)

write.table(df_all,'../../outputs/fa_ind_tablas.csv',sep='|',row.names = FALSE)

