database_path <- '../../inputs/data.duckdb'

# INSUFICIENCIA CARDIACA CONGESTIVA

# - Insuficiencia cardíaca congestiva en la población mayor de 18 años.

### Total

df_icc_global <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)


df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL
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

df_icc_global <- rbind(df_icc_global,data_csv)
### Hombres

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1
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

df_icc_global <- rbind(df_icc_global,data_csv)
### Mujeres

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2
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

df_icc_global <- rbind(df_icc_global,data_csv)
### NS bajo

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0
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

df_icc_global <- rbind(df_icc_global,data_csv)
### NS Alto

df <- dbGetQuery(conn = con, '
  select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1
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

df_icc_global <- rbind(df_icc_global,data_csv)
df_icc_global$ind <- 'icc'

dbDisconnect(con, shutdown=TRUE)



### - Determinación/registro de péptido natriurético el año de estudio
### - Determinación/registro de fracción de eyección el año de estudio
### - Vacuna de la gripe el año de estudio.


df_icc_exams_pn <- data.frame()


df_ind_exams_pn <- data.frame(ind = c('pn','fevi','vacuna_gripe'),
                              descr = c('Pépitido natriurético (BNP y NT-proBNP)','Determinación fracción eyección ventrículo izq.','Vacunación antigripal'),
                              cod = c('icc_pep_nat','icc_fe','icc_gripe'))

icc_indicadores_exam <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  df_ind_exams_ <- df_ind_exams_pn %>% filter(ind %in% i)
  if(i == 'pn'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",i,"' and year(test_dt) == 2023")) 
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023"))
  }
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  
  
  data_csv$tipo_analisis <- 1
  
  df_icc_exams_pn <- rbind(df_icc_exams_pn,data_csv)
  
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_icc_exams_pn <- rbind(df_icc_exams_pn,data_csv)
  
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_icc_exams_pn <- rbind(df_icc_exams_pn,data_csv)
  ### NS Bajo ###
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_icc_exams_pn <- rbind(df_icc_exams_pn,data_csv)
  
  ### NS Alto ###
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_icc_exams_pn <- rbind(df_icc_exams_pn,data_csv)
  df_icc_exams_pn$ind <- df_ind_exams_$cod
  
  dbDisconnect(con, shutdown=TRUE)            
  return(df_icc_exams_pn)
}

df_icc_exams_pn_all <- lapply(df_ind_exams_pn$ind,icc_indicadores_exam)


df_icc_exams_pn_all <- rbind(df_icc_exams_pn_all[[1]],df_icc_exams_pn_all[[2]],df_icc_exams_pn_all[[3]])




### - Ecocardiograma al diagnóstico.



df_icc_eco_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.examenes WHERE examen_cd == 'ecocardio' and year(examen_dt) == 2023"))
pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 2 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)


data_csv$tipo_analisis <- 1

df_icc_eco_all <- rbind(df_icc_eco_all,data_csv)

### HOMBRES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 2 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and sexo_cd ==1"))

df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 2

df_icc_eco_all <- rbind(df_icc_eco_all,data_csv)

### MUJERES ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 2 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and sexo_cd ==2"))

df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 3

df_icc_eco_all <- rbind(df_icc_eco_all,data_csv)

### NS Bajo ###


pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 2 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))

df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 4

df_icc_eco_all <- rbind(df_icc_eco_all,data_csv)

### NS Alto ###

pobla <- dbGetQuery(conn = con,
                    paste0("select * from main.paciente where enfermedad_cd == 2 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))

df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))


data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0

data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)

data_csv$tipo_analisis <- 5

df_icc_eco_all <- rbind(df_icc_eco_all,data_csv)

df_icc_eco_all$ind <- 'icc_eco'

dbDisconnect(con, shutdown=TRUE)  


# - Inhibidores de la Enzima Convertidora de Angiotensina (IECA) o ARA-II.
# - Sacubritilo /valsartán. 
# - Sacubritilo /valsartán como primer tratamiento.
# - IECA o ARA-II o sacubitrilo-valsartán
# - Betabloqueantes.
# - Antagonistas de la aldosterona.
# - iSGLT2 (dapagliflozina o empagliflozina).
# - IECA o ARA-II o sacubitrilo-valsartán Y betabloqueante Y antagonista de aldosterona y iSGLT2. 
# - Vericiguat.



df_icc_atc <- data.frame()


df_atc_tratamiento <- data.frame(ind = c('C09','C09DX04','C09','C07','C03DA','A10B','','C01DX22','C09DX04'),
                                 filter = c('C09C|C09D|C09A|C09B','C09DX04','C09DX04|C09C|C09D|C09A|C09B','C07','C03DA',
                                            'A10BK01|A10BD15|A10BD21|A10BK03|A10BD20|A10BD19',
                                            'C09C|C09D|C09A|C09B|C09DX04|C07|C03DA|A10BK01|A10BD15|A10BD21|A10BK03|A10BD20|A10BD19','C01DX22','C09DX04'),
                                 cod = c('icc_ieca_ara','icc_sac_val','icc_ieca_ara_sac_val','icc_bbloc','icc_antag_aldo','icc_isglt2',
                                         'icc_ieca-sac_bbloc_aa_isglt2','icc_ver','icc_prim_sac_val'))

icc_indicadores_tratamiento <- function(i){
  
  
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento %>% filter(cod %in% i)
  if(i == 'icc_prim_sac_val'){
    
    df <- dbGetQuery(conn=con,"SELECT paciente_id, atc_tratamiento_cd, fecha_inicio_prescripcion_dt
                                  FROM (
                                  SELECT *,
                                  ROW_NUMBER() OVER (PARTITION BY paciente_id ORDER BY fecha_inicio_prescripcion_dt) AS rn
                                  FROM main.tratamiento 
                                  ) sub
                                  WHERE rn = 1 and atc_tratamiento_cd = 'C09DX04'")
    
    
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",df_atc_tratamiento_$ind,"%'"))
  }
  
  df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 1
  
  df_icc_atc <- rbind(df_icc_atc,data_csv)
  
  ### HOMBRES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==1"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 2
  
  df_icc_atc <- rbind(df_icc_atc,data_csv)
  
  
  ### MUJERES ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and sexo_cd ==2"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 3
  
  df_icc_atc <- rbind(df_icc_atc,data_csv)
  ### NS Bajo ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 0"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 4
  
  df_icc_atc <- rbind(df_icc_atc,data_csv)
  
  ### NS Alto ###
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select * from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL and nivel_copago_cd == 1"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  
  
  data_csv <- df_icc %>% group_by(zbs_residencia_cd,ccaa_cd) %>% summarise(n_casos_zbs = n_distinct(paciente_id))
  data_csv <- left_join(pobla,data_csv,by=c('zbs_residencia_cd','ccaa_cd'))
  data_csv$n_casos_zbs[is.na(data_csv$n_casos_zbs)] <- 0
  
  data_csv$tc_zbs <- round(100*(data_csv$n_casos_zbs / data_csv$n_pobla_zbs),3)
  
  data_csv$tipo_analisis <- 5
  
  df_icc_atc <- rbind(df_icc_atc,data_csv)
  
  df_icc_atc$ind <- df_atc_tratamiento_$cod
  
  dbDisconnect(con, shutdown=TRUE)      
  return(df_icc_atc)           
}

df_icc_atc_all <- lapply(df_atc_tratamiento$cod,icc_indicadores_tratamiento)  

df_icc_atc_all <- rbind(df_icc_atc_all[[1]],df_icc_atc_all[[2]],df_icc_atc_all[[3]],
                        df_icc_atc_all[[4]],df_icc_atc_all[[5]],df_icc_atc_all[[6]],
                        df_icc_atc_all[[7]],df_icc_atc_all[[8]],df_icc_atc_all[[9]])



df_all <- rbind(df_icc_global,df_icc_exams_pn_all,df_icc_eco_all,df_icc_atc_all)

write.table(df_all,'../../outputs/icc_ind_tablas.csv',sep='|',row.names = FALSE)