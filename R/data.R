#' Weanings data
#'
#' A dataset containing the ....
#'
#' @format A data frame with 2.483.787 rows and 33 variables:
#' \describe{
#'   \item{folder}{(chr) name of the source folder, i.e, the code of the data center.}
#'   \item{file}{(chr) source filename. Pattern is CCNNN_MMM where CC is the center code, NNN is the patient ID for the center, and MMM is the sequential file code for the patient.}
#'   \item{pat_id}{(num) Patient id.}
#'   \item{date}{(Date) date for the measurements.}
#'   \item{ora}{(time) time for the measurements (granularity: 1 minute).}
#'   \item{caratteristiche_dinamiche_ml_cm_h2o}{(ml/cmH₂O)}
#'   \item{flusso_di_fine_esp_l_min}{(l/min) End expiratory flow}
#'   \item{pressione_di_fine_esp_cm_h2o}{(cmH₂O) (PEEP) Is the pressure in the lungs (alveolar pressure) above atmospheric pressure that exists at the end of expiration.}
#'   \item{vol_minuto_espirato_l_min}{(l/min), the volume of air that moves out of the lungs during a minute.}
#'   \item{vol_corrente_espirato_ml}{}
#'   \item{concentraz_o2_percent}{}
#'   \item{vol_minuto_inspirato_l_min}{}
#'   \item{vol_corrente_inspirato_ml}{}
#'   \item{press_media_vie_aeree_cm_h2o}{}
#'   \item{frequenza_respiraz_misurata_resp_min}{}
#'   \item{freq_spontanea_resp_min}{}
#'   \item{picco_edi_m_v}{}
#'   \item{edi_min_m_v}{}
#'   \item{press_di_picco_delle_vie_aeree_cm_h2o}{}
#'   \item{passa_a_backup_min}{}
#'   \item{backup_percent_min}{}
#'   \item{p_0_1_cm_h2o}{}
#'   \item{lavoro_respiratorio_del_ventilatore_joule_l}{}
#'   \item{lavoro_respiratorio_del_paziente_joule_l}{}
#'   \item{sbi}{}
#'   \item{volume_minuto_espirato_spontaneo_l_min}{}
#'   \item{perdita_percentuale_percent}{}
#'   \item{resistenza_esp_cm_h2o_l_s}{(cmH₂O/l/s), Is the resistance of the respiratory tract to airflow during expiration.}
#'   \item{resistenza_inspiratoria_cm_h2o_l_s}{}
#'   \item{press_di_pausa_vie_aeree_cm_h2o}{}
#'   \item{elastanza_cm_h2o_l}{(cmH₂O/l) a measure of the work that has to be exerted by the muscles of inspiration to expand the lungs}
#'   \item{compliance_statica_ml_cm_h2o}{}
#'   \item{et_co2_mm_hg}{}
#'   \item{eliminazione_co2_corrente_ml}{}
#'   \item{eliminazione_co2_minuto_ml_min}{}
#' }
#' @source ...
"weanings"
