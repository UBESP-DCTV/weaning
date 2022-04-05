#' Weanings data TRD
#'
#' A dataset containing the ....
#'
#' @format A data frame with 2.144.725 rows and 37 variables:
#' \describe{
#'   \item{folder}{(chr) main folder for the imported file, i.e., the
#'   center code} \item{file}{ (chr) imported file name, pattern:
#'   CCDDD_XXXX where CC is the center code, DDD is the patient id and
#'   XXXX is the sequence-code of the weaning file.} \item{id_pat}{(num)
#'   patient id as a number} \item{date}{date of transmission}
#'   \item{file}{(chr) }
#'   \item{id_pat}{(num) }
#'   \item{date}{(Date) }
#'   \item{ora}{time of signals (granularity: 1 minute)}
#'   \item{et_co2_percent}{(num) }
#'   \item{eliminazione_co2_corrente_ml}{(num) }
#'   \item{eliminazione_co2_minuto_ml_min}{(num) }
#'   \item{caratteristiche_dinamiche_ml_cm_h2o}{(num) }
#'   \item{elastanza_cm_h2o_l}{(num) a measure of the work that has to
#'   be exerted by the muscles of inspiration to expand the lungs
#'   \[cmH2O/l\]} \item{flusso_di_fine_esp_l_min}{(num) End expiratory
#'   flow \[l/min\]} \item{pressione_di_fine_esp_cm_h2o}{(num) (PEEP) Is
#'   the pressure in the lungs (alveolar pressure) above atmospheric
#'   pressure that exists at the end of expiration. There are two types
#'   of PEEP: intrinsic and extrinsic. Intrinsic PEEP depends on the
#'   progressive air trapping after incomplete expiration. Extrinsic
#'   PEEP it is set directly on the ventilator. A small amount of
#'   applied PEEP (4 to 5 cmH2O) is used in most mechanically ventilated
#'   patients to mitigate end-expiratory alveolar collapse.\[cmH2O\]  }
#'   \item{resistenza_esp_cm_h2o_l_s}{(num)  Is the resistance of the
#'   respiratory tract to airflow during expiration. This includes the
#'   resistance of the upper airways and oro-tracheal tube
#'   \[cmH2O/l/s\]} \item{vol_minuto_espirato_l_min}{(num) the volume of
#'   air that moves out of the lungs during a minute \[l/min\]}
#'   \item{vol_corrente_espirato_ml}{(num) the volume of air that moves
#'   out of the lungs during a single breath \[ml\]}
#'   \item{concentraz_o2_percent}{(num) Percentage of oxygen in a
#'   specific volume of air \[%\]}
#'   \item{resistenza_inspiratoria_cm_h2o_l_s}{(num) Is the resistance
#'   of the respiratory tract to airflow during inhalation. This
#'   includes the resistance of the upper airways and oro-tracheal tube
#'   \[cmH2O/l/s\]} \item{vol_minuto_inspirato_l_min}{(num) the volume
#'   of air that moves into of the lungs during a minute \[l/min\]}
#'   \item{vol_corrente_inspirato_ml}{(num) the volume of air that moves
#'   into of the lungs during a single breath \[ml\]}
#'   \item{press_media_vie_aeree_cm_h2o}{(num) The average pressure in
#'   the airways during inspiratory phase \[cmH2O\]}
#'   \item{frequenza_respiraz_misurata_resp_min}{(num) number of breath
#'   per minute recorded \[resp/min\]}
#'   \item{freq_spontanea_resp_min}{(num) number of spontaneous breath
#'   per minute \[resp/min\]} \item{picco_edi_m_v}{(num) Diaphragm
#'   electromiography peak  \[\eqn{\mu}V\]} \item{edi_min_m_v}{(num)
#'   Diaphragm electromiography minimum level \[\eqn{\mu}V\]}
#'   \item{press_di_pausa_vie_aeree_cm_h2o}{(num) Plateu Pressure: is
#'   the pressure applied to small airways and alveoli at the end of
#'   inspiration during positive-pressure mechanical ventilation
#'   \[cmH2O\]} \item{press_di_picco_delle_vie_aeree_cm_h2o}{(num) Peak
#'   Pressure: is the highest pressure level applied to the respiratory
#'   system during inspiration. Depends of any airways
#'   resistance.\[cmH2O\]} \item{passa_a_backup_min}{(num) Number of
#'   backup switch in a minute \[/min\]} \item{backup_percent_min}{(num)
#'   The percentage of breath switched in backup mode in a minute
#'   \[%/min\]} \item{compliance_statica_ml_cm_h2o}{(num) Static
#'   Compliance. Is the reciprocal of elastance measured in dynamic
#'   manner \[ml/cmH2O\]} \item{p_0_1_cm_h2o}{(num) The negative
#'   pressure generated at 0.1 sec. from the beginning of the
#'   inspiratory phase \[cmH2O\]}
#'   \item{lavoro_respiratorio_del_ventilatore_joule_l}{(num) Ventilator
#'   work of breathing \[Joule/l\]}
#'   \item{lavoro_respiratorio_del_paziente_joule_l}{(num) Patient work
#'   of breathing \[Joule/l\]} \item{sbi}{(num) }
#'   \item{volume_minuto_espirato_spontaneo_l_min}{(num) the volume of
#'   air that moves out of the lungs during a minute due to spontaneous
#'   breath \[l/min\]} \item{perdita_percentuale_percent}{(num)
#'   Percentage of airleaks during not invasive ventilation \[%\]}
#'   \item{stress_index}{(num) } \item{et_co2_mm_hg}{(num) }
#' }
#' @source ...
#' @name weanings_trd
NULL










#' Weanings data LOG
#'
#' A dataset containing the ....
#'
#' @format A data frame with 1.631.510 rows and 8 variables:
#' \describe{
#'   \item{folder}{(chr) }
#'   \item{file}{(chr) }
#'   \item{id_pat}{(num) }
#'   \item{data}{(Date) }
#'   \item{ora}{{hms} }
#'   \item{id_info}{(int) }
#'   \item{tipo}{(fct) }
#'   \item{informazioni}{(chr) }
#' }
#' @source ...
#' @name weanings_log
NULL



