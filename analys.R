argument <- if (sys.nframe() == 0L) commandArgs(trailingOnly = TRUE) else character()
registerfil <- if (length(argument) >= 1) argument[1] else 'EPSONIP_DATA_2026-01-19_0959.csv'
aktigrafifil <- if (length(argument) >= 2) argument[2] else 'EPSONIP_clean_actigraphy.xlsx'
utdatamapp <- if (length(argument) >= 3) argument[3] else 'resultat'
antal_bootstrap <- if (length(argument) >= 4) as.integer(argument[4]) else 2000L
datumfil <- if (length(argument) >= 5) argument[5] else ''
trosklar <- c(5, 10, 15, 20)
slumptal <- 20261023L
brytar <- 26L
paket <- c('readxl', 'pROC', 'ggplot2', 'rms', 'openxlsx')
saknade_paket <- paket[!vapply(paket, requireNamespace, logical(1), quietly = TRUE)]
if (length(saknade_paket)) stop('Installera paket: ', paste(saknade_paket, collapse = ', '))
if (!file.exists(registerfil) || !file.exists(aktigrafifil)) stop('Registerfilen eller aktigrafifilen saknas. Aktuell arbetskatalog: ',getwd())
if (nzchar(datumfil) && !file.exists(datumfil)) stop('Datumfilen saknas: ',datumfil)
if (!10 %in% trosklar || anyNA(trosklar) || anyDuplicated(trosklar) || any(trosklar<0 | trosklar>100)) stop('T90-granser maste vara unika, mellan 0 och 100 och omfatta 10.')
if (is.na(antal_bootstrap) || antal_bootstrap < 20) stop('Antalet bootstrapomdragningar maste vara minst 20.')
dir.create(utdatamapp,recursive=TRUE,showWarnings=FALSE)
utdatafil <- file.path(utdatamapp,'analysunderlag.csv')
if (normalizePath(utdatafil,mustWork=FALSE) %in% normalizePath(c(registerfil,aktigrafifil))) stop('Utdatafilen far inte skriva over indata.')
options(stringsAsFactors=FALSE,warn=1)
set.seed(slumptal)
talvarde <- function(varden) suppressWarnings(as.numeric(as.character(varden)))
nyckel <- function(varden) {
  vapply(as.character(varden),function(text) {
    if(is.na(text) || grepl('excluded',text,ignore.case=TRUE)) return(NA_character_)
    traff <- regexpr('[0-9]+',text)
    if(traff<0) return(NA_character_)
    nummer <- as.integer(regmatches(text,traff))
    paste0(ifelse(nummer<300,'EL','EN'),sprintf('%03d',nummer))
  },character(1))
}
forsta <- function(varden) {
  if (is.character(varden)) {
    varden <- trimws(varden)
    varden[varden==''] <- NA_character_
  }
  giltiga <- which(!is.na(varden))
  if (!length(giltiga)) return(varden[NA_integer_])
  varden[giltiga[1]]
}
artal <- function(varden) {
  vapply(as.character(varden),function(text) {
    if(is.na(text) || trimws(text)=='') return(NA_integer_)
    delar <- regmatches(text,gregexpr('[0-9]{2,4}',text))[[1]]
    delar <- delar[nchar(delar) %in% c(2,4)]
    if(!length(delar)) return(NA_integer_)
    siffror <- as.integer(delar)
    korta <- nchar(delar)==2
    siffror[korta] <- siffror[korta]+ifelse(siffror[korta]<=brytar,2000L,1900L)
    min(siffror)
  },integer(1))
}
klocktid <- function(varden) {
  vapply(as.character(varden),function(text) {
    if (is.na(text) || trimws(text)=='') return(NA_real_)
    if (!grepl('^[0-9]{1,2}:[0-9]{1,2}$',trimws(text))) stop('Ogiltig PSQI-klocktid; anvand HH:MM.')
    delar <- as.numeric(strsplit(trimws(text),':',fixed=TRUE)[[1]])
    if(delar[1]>24 || delar[2]>59 || (delar[1]==24 && delar[2]!=0)) stop('Ogiltig PSQI-klocktid; timmar skall vara 0-23 (eller 24:00) och minuter 0-59.')
    if(delar[1]==24) return(0)
    delar[1]+delar[2]/60
  },numeric(1))
}
poangsteg <- function(summa) {
  if(any(!is.na(summa) & !summa %in% 0:6)) stop('Ogiltig PSQI-delsumma; tillatna varden ar heltal 0-6.')
  ifelse(is.na(summa),NA_real_,ifelse(summa==0,0,ifelse(summa<=2,1,ifelse(summa<=4,2,3))))
}
formularvarden <- function(tabell,kolumner,minsta,storsta,heltal=TRUE) {
  as.data.frame(lapply(setNames(kolumner,kolumner),function(kolumn) {
    text <- trimws(as.character(tabell[[kolumn]]))
    saknas <- is.na(text) | text==''
    varden <- talvarde(text)
    fel <- !saknas & (!is.finite(varden) | varden<minsta | varden>storsta | (heltal & varden!=floor(varden)))
    if(any(fel)) stop('Ogiltiga formularsvar i ',kolumn,': ',sum(fel),'. Kontrollera kodning och tillatet intervall.')
    varden[saknas] <- NA_real_
    varden
  }),check.names=FALSE)
}
register <- read.csv(registerfil,check.names=FALSE,na.strings=c('','NA'),stringsAsFactors=FALSE)
krav <- c('study_id','fv_age','fv_sex','fv_date','bmi','ci_diabetes_yod','ci_hyperlipidemia_yod','ci_smoking','au_weekly_units_estimate','tid_rygg','tid_ickerygg','ahi_rygg','ahi_ickerygg','odi_rygg','odi_ickerygg','tid_90','avg_sp02','polygraphy_complete',paste0('ess_',1:8),paste0('stopb_',1:8),'psqi_1','psqi_2','psqi_3','psqi_4',paste0('psqi_5',letters[1:10]),paste0('psqi_',6:9))
saknade <- setdiff(krav,names(register))
if(length(saknade)) stop('Registerkolumner saknas: ',paste(saknade,collapse=', '))
if('redcap_repeat_instrument' %in% names(register)) {
  instrument <- tolower(as.character(register[['redcap_repeat_instrument']]))
  register <- register[is.na(instrument) | instrument!='medications',,drop=FALSE]
}
register[['fv_id']] <- nyckel(register[['study_id']])
register <- register[!is.na(register[['fv_id']]),,drop=FALSE]
kommentarkolumner <- intersect(c('psqi_5j_comment','psqi_5jcom','psqi_5j_text'),names(register))
if(length(kommentarkolumner)>1) stop('Flera kommentarkolumner for PSQI 5j finns; behall endast den verifierade.')
kolumner <- unique(c('fv_id',krav,setdiff(c('bw_hba1c','bw_creatinine','bw_mdrd','bw_ldl','bw_hdl','bw_tg','bw_hscrp','ci_highbp','ci_angina','ci_myoinfarct','ci_cor_angiography','ci_cabg','ci_heartfailure','ci_atrialfib','ci_ischstroke','ci_hypothy','ci_obesitysurg'),krav)))
kolumner <- unique(c(kolumner,kommentarkolumner))
kolumner <- intersect(kolumner,names(register))
grupper <- split(seq_len(nrow(register)),register[['fv_id']])
register <- do.call(rbind,lapply(grupper,function(rader) as.data.frame(lapply(register[rader,kolumner,drop=FALSE],forsta),check.names=FALSE)))
rownames(register) <- NULL
if(anyDuplicated(register[['fv_id']])) stop('Deltagarnyckeln ar inte unik efter sammanslagning.')
dygn <- as.data.frame(readxl::read_excel(aktigrafifil,sheet='24h time use'))
somn <- as.data.frame(readxl::read_excel(aktigrafifil,sheet='Sleep'))
lasnyckel <- function(tabell) {
  kandidater <- intersect(c('fv_id','ID','id','study_id','sleeplog_ID','sleeplog_id'),names(tabell))
  if(!length(kandidater)) stop('Deltagarnyckel saknas i aktigrafifilen.')
  tabell[['fv_id']] <- nyckel(tabell[[kandidater[1]]])
  if(anyNA(tabell[['fv_id']])) message('Aktigrafirader utan tolkbar deltagarnyckel som inte kan matchas: ',sum(is.na(tabell[['fv_id']])))
  tabell <- tabell[!is.na(tabell[['fv_id']]),,drop=FALSE]
  if(anyDuplicated(tabell[['fv_id']])) stop('Aktigrafifilen innehaller duplicerad deltagarnyckel.')
  tabell
}
dygn <- lasnyckel(dygn)
somn <- lasnyckel(somn)
dygnskrav <- c('fv_id','Nvaliddays','dur_day_total_MOD_min_wei','dur_day_total_VIG_min_wei')
somnkrav <- c('fv_id','SptDuration_AD_T5A5_mn','SleepDurationInSpt_AD_T5A5_mn','WASO_AD_T5A5_mn','SleepRegularityIndex_AD_T5A5_mn','sleep_efficiency_AD_T5A5_mn')
if(!all(dygnskrav %in% names(dygn)) || !all(somnkrav %in% names(somn))) stop('Obligatoriska GGIR-sammanfattningar saknas.')
register <- merge(register,dygn[dygnskrav],by='fv_id',all.x=TRUE,sort=TRUE)
register <- merge(register,somn[somnkrav],by='fv_id',all.x=TRUE,sort=TRUE)
antal_register <- nrow(register)
giltig_aktigrafi <- !is.na(register[['Nvaliddays']]) & talvarde(register[['Nvaliddays']])>0
giltig_andning <- !is.na(register[['polygraphy_complete']]) & talvarde(register[['polygraphy_complete']])==2
register <- register[giltig_aktigrafi & giltig_andning,,drop=FALSE]
underlag <- data.frame(fv_id=as.character(register[['fv_id']]),excluded=0L,check.names=FALSE)
hamta <- function(kolumn) if(kolumn %in% names(register)) talvarde(register[[kolumn]]) else rep(NA_real_,nrow(register))
underlag[['age']] <- hamta('fv_age')
underlag[['sex_male']] <- ifelse(hamta('fv_sex')==1,1L,ifelse(hamta('fv_sex')==2,0L,NA_integer_))
underlag[['bmi']] <- hamta('bmi')
underlag[['bmi']] <- ifelse(underlag[['bmi']]>1000,underlag[['bmi']]/100,underlag[['bmi']])
underlag[['bmi']][which(underlag[['bmi']]<10)] <- NA_real_
diagnosar <- artal(register[['ci_diabetes_yod']])
diagnosdatum <- as.Date(ifelse(is.na(diagnosar),NA_character_,paste0(diagnosar,'-01-01')))
underlag[['diabetes_duration']] <- as.numeric(as.Date(register[['fv_date']])-diagnosdatum)/365.25
totaltid <- hamta('tid_rygg')+hamta('tid_ickerygg')
ryggandel <- ifelse(!is.na(totaltid) & totaltid>0,hamta('tid_rygg')/totaltid,NA_real_)
underlag[['AHI']] <- hamta('ahi_rygg')*ryggandel+hamta('ahi_ickerygg')*(1-ryggandel)
underlag[['ODI']] <- hamta('odi_rygg')*ryggandel+hamta('odi_ickerygg')*(1-ryggandel)
underlag[['tid_90']] <- hamta('tid_90')
underlag[['avg_sp02']] <- hamta('avg_sp02')
namnkarta <- c(SRI='SleepRegularityIndex_AD_T5A5_mn',SE_pct='sleep_efficiency_AD_T5A5_mn',TSP_h='SptDuration_AD_T5A5_mn',TST_h='SleepDurationInSpt_AD_T5A5_mn',WASO_h='WASO_AD_T5A5_mn',hba1c='bw_hba1c',creatinine='bw_creatinine',egfr_mdrd='bw_mdrd',ldl='bw_ldl',hdl='bw_hdl',tg='bw_tg',hs_crp='bw_hscrp',audit_units_wk='au_weekly_units_estimate',vigorous_min='dur_day_total_VIG_min_wei')
for(kolumn in names(namnkarta)) underlag[[kolumn]] <- hamta(namnkarta[[kolumn]])
underlag[['moderate_h']] <- hamta('dur_day_total_MOD_min_wei')/60
essdata <- formularvarden(register,paste0('ess_',1:8),0,3)
stoppdata <- formularvarden(register,paste0('stopb_',1:8),0,1)
underlag[['ESS']] <- rowSums(essdata,na.rm=FALSE)
underlag[['STOPB']] <- rowSums(stoppdata,na.rm=FALSE)
psqidata <- formularvarden(register,c(paste0('psqi_5',letters[1:10]),paste0('psqi_',6:9)),0,3)
insomning <- formularvarden(register,'psqi_2',0,1440,heltal=FALSE)[['psqi_2']]
sovtimmar <- formularvarden(register,'psqi_4',0,24,heltal=FALSE)[['psqi_4']]
laggtid <- klocktid(register[['psqi_1']])
vakentid <- klocktid(register[['psqi_3']])
sangtid <- vakentid-laggtid
sangtid <- ifelse(sangtid<0,sangtid+24,sangtid)
effektivitet <- ifelse(!is.na(sangtid) & sangtid>0,sovtimmar/sangtid*100,NA_real_)
effektivitet <- pmin(effektivitet,100)
insomningspoang <- ifelse(insomning<=15,0,ifelse(insomning<=30,1,ifelse(insomning<=60,2,3)))
delpoang <- data.frame(kvalitet=psqidata[['psqi_6']],insomning=poangsteg(insomningspoang+psqidata[['psqi_5a']]),langd=ifelse(sovtimmar>7,0,ifelse(sovtimmar>=6,1,ifelse(sovtimmar>=5,2,3))),effektivitet=ifelse(effektivitet>=85,0,ifelse(effektivitet>=75,1,ifelse(effektivitet>=65,2,3))))
storningsdata <- psqidata[paste0('psqi_5',letters[2:10])]
storningsdata[['psqi_5j']][is.na(storningsdata[['psqi_5j']])] <- 0
if(length(kommentarkolumner)==1) {
  kommentar <- trimws(as.character(register[[kommentarkolumner]]))
  storningsdata[['psqi_5j']][is.na(kommentar) | kommentar==''] <- 0
} else {
  message('PSQI 5j: kommentaren finns inte i exporten.')
}
storningssumma <- rowSums(storningsdata,na.rm=FALSE)
delpoang[['storningar']] <- ifelse(is.na(storningssumma),NA_real_,ifelse(storningssumma==0,0,ifelse(storningssumma %in% 1:9,1,ifelse(storningssumma %in% 10:18,2,ifelse(storningssumma>=19,3,NA_real_)))))
delpoang[['medicin']] <- psqidata[['psqi_7']]
delpoang[['dagfunktion']] <- poangsteg(psqidata[['psqi_8']]+psqidata[['psqi_9']])
underlag[['PSQI']] <- rowSums(delpoang,na.rm=FALSE)
underlag[['psqi_5j_kommentar_kontrollerad']] <- length(kommentarkolumner)==1
underlag[['smoking']] <- as.character(factor(hamta('ci_smoking'),levels=c(1,2,3),labels=c('Never','Current','Former')))
underlag[['hyperlipidemia']] <- ifelse(is.na(artal(register[['ci_hyperlipidemia_yod']])),'No','Yes')
samsjuklighet <- c('ci_highbp','ci_angina','ci_myoinfarct','ci_cor_angiography','ci_cabg','ci_heartfailure','ci_atrialfib','ci_ischstroke','ci_hypothy','ci_obesitysurg')
for(kolumn in samsjuklighet) underlag[[paste0(kolumn,'.factor')]] <- as.character(factor(hamta(kolumn),levels=c(1,0),labels=c('Yes','No')))
if(anyDuplicated(underlag[['fv_id']]) || anyNA(underlag[['fv_id']])) stop('Felaktig deltagarnyckel i slutligt underlag.')

write.csv(underlag,utdatafil,row.names=FALSE,na='')
message('Analysunderlag skapat fran raexporterna: ',nrow(underlag),' deltagare.')
ursprungsdata <- underlag
namnkarta <- c(id='fv_id', exkluderad='excluded', alder='age', man='sex_male', kroppsmassa='bmi', diabetesar='diabetes_duration', apneindex='AHI', desaturationsindex='ODI', hypoxiprocent='tid_90', medelsaturation='avg_sp02', regelbundenhet='SRI', somneffektivitet='SE_pct', somnperiod='TSP_h', somntid='TST_h', vakenhet='WASO_h', stoppbang='STOPB', somnkvalitet='PSQI', somnighet='ESS', rokning='smoking', alkohol='audit_units_wk', hogtblodtryck='ci_highbp.factor', hyperlipidemi='hyperlipidemia', angina='ci_angina.factor', hjartinfarkt='ci_myoinfarct.factor', kranskarl='ci_cor_angiography.factor', bypass='ci_cabg.factor', hjartsvikt='ci_heartfailure.factor', formaksflimmer='ci_atrialfib.factor', stroke='ci_ischstroke.factor', hypotyreos='ci_hypothy.factor', obesitaskirurgi='ci_obesitysurg.factor', langtidsblodsocker='hba1c', kreatinin='creatinine', njurfunktion='egfr_mdrd', ldl='ldl', hdl='hdl', triglycerider='tg', inflammation='hs_crp', mattligaktivitet='moderate_h', intensivaktivitet='vigorous_min')
obligatoriska <- c('id','exkluderad','alder','man','kroppsmassa','diabetesar','apneindex','desaturationsindex','hypoxiprocent','regelbundenhet','somneffektivitet','stoppbang')
saknade <- setdiff(unname(namnkarta[obligatoriska]), names(ursprungsdata))
if (length(saknade)) stop('Saknade kolumner i det bearbetade analysunderlaget: ', paste(saknade, collapse=', '))
underlag <- as.data.frame(lapply(namnkarta, function(kolumn) if (kolumn %in% names(ursprungsdata)) ursprungsdata[[kolumn]] else rep(NA, nrow(ursprungsdata))))
names(underlag) <- names(namnkarta)
underlag$id <- as.character(underlag$id)
if (anyNA(underlag$id) || anyDuplicated(underlag$id)) stop('Indata maste innehalla en unik, icke-saknad deltagarnyckel per rad.')
underlag <- underlag[!is.na(underlag$exkluderad) & underlag$exkluderad == 0 & !is.na(underlag$apneindex), ]
for (kolumn in c(obligatoriska[-c(1,2)],'medelsaturation','somnperiod','somntid','vakenhet','somnkvalitet','somnighet','alkohol','langtidsblodsocker','kreatinin','njurfunktion','ldl','hdl','triglycerider','inflammation','mattligaktivitet','intensivaktivitet')) underlag[[kolumn]] <- suppressWarnings(as.numeric(as.character(underlag[[kolumn]])))
if (any(!underlag$man %in% c(0,1,NA))) stop('man skall vara 0 eller 1.')
if (any(underlag$hypoxiprocent < 0 | underlag$hypoxiprocent > 100, na.rm=TRUE)) stop('T90 skall anges i procent 0 till 100.')
if (any(underlag$somneffektivitet < 0 | underlag$somneffektivitet > 100, na.rm=TRUE)) stop('Somneffektivitet skall anges i procent 0 till 100.')
for (variabel in c('stoppbang','somnighet','somnkvalitet')) {
  tal <- underlag[[variabel]]
  hogsta <- c(stoppbang=8,somnighet=24,somnkvalitet=21)[[variabel]]
  if (any(!is.na(tal) & (!is.finite(tal) | tal<0 | tal>hogsta | tal!=round(tal)))) stop('Ogiltig formularpoang: ',variabel)
}
underlag$osagrupp <- cut(underlag$apneindex, c(-Inf,5,15,30,Inf), right=FALSE, labels=c('No OSA','Mild OSA','Moderate OSA','Severe OSA'))
underlag$hogeffektivitet <- ifelse(is.na(underlag$somneffektivitet), NA, as.integer(underlag$somneffektivitet > 80))
underlag$svarosa <- as.integer(underlag$apneindex >= 30)
underlag$mattligosa <- as.integer(underlag$apneindex >= 15)
underlag$loghypoxi <- log1p(underlag$hypoxiprocent)
kliniska <- c('man','alder','kroppsmassa','diabetesar')
aktigrafi <- c('regelbundenhet','somneffektivitet')
gemensamma <- c(kliniska,aktigrafi,'stoppbang')
resultat <- list()
resultat$Variabelkarta <- data.frame(variabel=names(namnkarta), ursprung=unname(namnkarta))
resultat$Metodnoter <- data.frame(amne=c('Formular','Gemensamt modellurval','Kalibrering','Kalibrering ensamprediktor','PSQI annan orsak'),beskrivning=c('ESS och STOP-Bang kraver samtliga items. PSQI kraver samtliga nodvandiga komponenter; saknad 5j-frekvens far 0 enligt instrumentets undantag.','Kliniska och kliniska+STOP-Bang-modeller anvander samma kompletta deltagarurval, inklusive komplett STOP-Bang.','Kalibrering kraver konvergens, full rang och finita skattningar. Ofullstandig kalibrering sammanfattas som NA. AUC och Brier bevaras.','Teckenbyte hos ensam prediktor i bootstrap ger status instabil_ensamprediktor och NA for sammanfattat intercept/lutning. Ingen trimning eller medianersattning.','Om kommentarfaltet till PSQI 5j saknas i exporten anvands observerad frekvens. Ej exporterad kommentar tolkas inte som obesvarad.'))
resultat$Urval <- data.frame(matt=c('Deskriptivt','SRI tillgangligt','AHI ODI SRI SE kompletta','Svar OSA modeller','T90 modeller'), antal=c(nrow(underlag),sum(!is.na(underlag$regelbundenhet)),sum(complete.cases(underlag[c('apneindex','desaturationsindex',aktigrafi)])),sum(complete.cases(underlag[c('svarosa',gemensamma)])),sum(complete.cases(underlag[c('hypoxiprocent',gemensamma)]))))
resultat$Bortfall <- do.call(rbind,lapply(names(namnkarta)[-c(1,2)],function(variabel) {
  korstabell <- table(underlag$osagrupp,is.na(underlag[[variabel]]))
  data.frame(variabel=variabel,antal=nrow(underlag),saknade=sum(is.na(underlag[[variabel]])),p=if(ncol(korstabell)>1) fisher.test(korstabell)$p.value else NA_real_)
}))
formatera <- function(tal, decimaler=1) ifelse(is.finite(tal),formatC(tal,format='f',digits=decimaler),'NA')
sammanfatta <- function(tal, medel=FALSE) {
  tal <- tal[is.finite(tal)]
  if (!length(tal)) return('NA')
  if (medel) return(paste0(formatera(mean(tal)),' +/- ',formatera(sd(tal))))
  paste0(formatera(median(tal)),' [',formatera(quantile(tal,.25)),', ',formatera(quantile(tal,.75)),']')
}
kontinuerliga <- c('alder','kroppsmassa','diabetesar','alkohol','somnkvalitet','somnighet','stoppbang','langtidsblodsocker','kreatinin','njurfunktion','ldl','hdl','triglycerider','inflammation','somnperiod','somntid','vakenhet','regelbundenhet','somneffektivitet','apneindex','desaturationsindex','hypoxiprocent','medelsaturation')
kategoriska <- c('man','rokning','hogtblodtryck','hyperlipidemi','angina','hjartinfarkt','kranskarl','bypass','hjartsvikt','formaksflimmer','stroke','hypotyreos','obesitaskirurgi','hogeffektivitet')
baslinje <- list()
for (variabel in kontinuerliga) {
  tal <- underlag[[variabel]]
  prov <- tryCatch(kruskal.test(tal ~ underlag$osagrupp)$p.value,error=function(fel) NA_real_)
  rad <- data.frame(variabel=variabel,niva='',p=prov)
  for(grupp in levels(underlag$osagrupp)) rad[[grupp]] <- sammanfatta(tal[underlag$osagrupp==grupp],variabel %in% c('alder','alkohol','somnkvalitet','somnighet','stoppbang','somnperiod','medelsaturation'))
  baslinje[[length(baslinje)+1]] <- rad
}
for (variabel in kategoriska) {
  tal <- as.character(underlag[[variabel]])
  korstabell <- table(underlag$osagrupp,tal)
  prov <- tryCatch(fisher.test(korstabell,workspace=2e7)$p.value,error=function(fel) NA_real_)
  for (niva in sort(unique(tal[!is.na(tal)]))) {
    rad <- data.frame(variabel=variabel,niva=niva,p=prov)
    for (grupp in levels(underlag$osagrupp)) {
      valt <- tal[underlag$osagrupp==grupp]
      rad[[grupp]] <- paste0(sum(valt==niva,na.rm=TRUE),' (',formatera(100*mean(valt==niva,na.rm=TRUE),0),'%)')
    }
    baslinje[[length(baslinje)+1]] <- rad
  }
}
resultat$Tabell1 <- do.call(rbind,baslinje)
anpassa <- function(formel, underlag) {
  varningar <- character()
  modell <- tryCatch(withCallingHandlers(glm(formel,data=underlag,family=binomial(),control=glm.control(maxit=100)),warning=function(varning) {
    varningar <<- c(varningar,conditionMessage(varning)); invokeRestart('muffleWarning')
  }),error=function(fel) NULL)
  if (is.null(modell)) return(NULL)
  if (!isTRUE(modell$converged) || any(!is.finite(coef(modell)))) return(NULL)
  attr(modell,'numerisk_varning') <- length(varningar)>0 || any(fitted(modell)<1e-8 | fitted(modell)>1-1e-8)
  modell
}
rocberakna <- function(utfall,sannolikhet) pROC::roc(utfall,sannolikhet,levels=c(0,1),direction='<',quiet=TRUE)
aucberakna <- function(utfall,sannolikhet) {
  fall <- sum(utfall==1)
  icke_fall <- sum(utfall==0)
  (sum(rank(sannolikhet,ties.method='average')[utfall==1])-fall*(fall+1)/2)/(fall*icke_fall)
}
modellmatt <- function(utfall,sannolikhet) {
  sannolikhet <- pmin(pmax(sannolikhet,1e-12),1-1e-12)
  logodds <- qlogis(sannolikhet)
  nara_konstant <- !all(is.finite(logodds)) || diff(range(logodds)) <= sqrt(.Machine$double.eps)*max(1,max(abs(logodds)))
  kalibrering <- if(nara_konstant) NULL else tryCatch(suppressWarnings(glm.fit(cbind(1,logodds),utfall,family=binomial(),control=glm.control(maxit=100))),error=function(fel) NULL)
  giltig <- !is.null(kalibrering) && isTRUE(kalibrering$converged) && kalibrering$rank==2 && all(is.finite(coef(kalibrering)))
  c(auc=aucberakna(utfall,sannolikhet),brier=mean((utfall-sannolikhet)^2),intercept=if(giltig) unname(coef(kalibrering)[1]) else NA_real_,lutning=if(giltig) unname(coef(kalibrering)[2]) else NA_real_,kalibrering_giltig=as.integer(giltig),kalibrering_konstant=as.integer(nara_konstant))
}
kalibreringssumma <- function(dragningar,prefix,ursprung,enprediktor,giltiga) {
  valda <- dragningar[giltiga,,drop=FALSE]
  antal_giltiga <- sum(valda[,paste0(prefix,'kalibrering')]==1,na.rm=TRUE)
  teckenbyten <- sum(valda[,paste0(prefix,'teckenbyte')],na.rm=TRUE)
  nara_konstanta <- sum(valda[,paste0(prefix,'konstant')],na.rm=TRUE)
  status <- if(ursprung['kalibrering_giltig']!=1 || !nrow(valda)) 'ej_skattningsbar' else if(enprediktor && (teckenbyten>0 || antal_giltiga<nrow(valda))) 'instabil_ensamprediktor' else if(antal_giltiga<nrow(valda)) 'ofullstandig_kalibrering' else 'beraknad'
  c(list(status=status,giltiga=antal_giltiga,teckenbyten=teckenbyten,nara_konstanta=nara_konstanta),lapply(c(intercept='intercept',lutning='lutning'),function(matt) if(status=='beraknad') unname(ursprung[matt]-mean(valda[,paste0(prefix,matt)])) else NA_real_))
}
koefficienter <- function(modell,utfall,modellnamn) {
  skattningar <- summary(modell)$coefficients
  data.frame(utfall=utfall,modell=modellnamn,variabel=rownames(skattningar),beta=skattningar[,1],standardfel=skattningar[,2],oddskvot=exp(skattningar[,1]),nedre=exp(skattningar[,1]-qnorm(.975)*skattningar[,2]),ovre=exp(skattningar[,1]+qnorm(.975)*skattningar[,2]),p=skattningar[,4],row.names=NULL)
}
jamfor <- function(underlag,utfallsnamn,troskel,basnamn,basvariabler,antal,fro) {
  if(length(unique(underlag$utfall))!=2) stop('Utfall saknar fall eller icke-fall vid ',utfallsnamn,'. Valj en meningsfull kanslighetsgrans.')
  grundformel <- reformulate(basvariabler,'utfall')
  plusformel <- reformulate(c(basvariabler,aktigrafi),'utfall')
  grundmodell <- anpassa(grundformel,underlag)
  plusmodell <- anpassa(plusformel,underlag)
  if (is.null(grundmodell) || is.null(plusmodell)) stop('Modellen kan inte skattas: ',utfallsnamn,' ',basnamn)
  grundroc <- rocberakna(underlag$utfall,fitted(grundmodell))
  plusroc <- rocberakna(underlag$utfall,fitted(plusmodell))
  grundmatt <- modellmatt(underlag$utfall,fitted(grundmodell))
  plusmatt <- modellmatt(underlag$utfall,fitted(plusmodell))
  grundintervall <- as.numeric(pROC::ci.auc(grundroc,method='delong'))
  plusintervall <- as.numeric(pROC::ci.auc(plusroc,method='delong'))
  prov <- anova(grundmodell,plusmodell,test='LRT')$`Pr(>Chi)`[2]
  set.seed(fro)
  dragningar <- matrix(NA_real_,nrow=antal,ncol=16)
  colnames(dragningar) <- c('delta','grundoptimism','plusoptimism','grundbrier','plusbrier','grundintercept','plusintercept','grundlutning','pluslutning','varning','grundkalibrering','pluskalibrering','grundteckenbyte','plusteckenbyte','grundkonstant','pluskonstant')
  for (omdragning in seq_len(antal)) {
    stickprov <- underlag[sample.int(nrow(underlag),replace=TRUE),,drop=FALSE]
    if (length(unique(stickprov$utfall))<2) next
    grundny <- anpassa(grundformel,stickprov)
    plusny <- anpassa(plusformel,stickprov)
    if (is.null(grundny) || is.null(plusny)) next
    grundtrana <- modellmatt(stickprov$utfall,fitted(grundny))
    plustrana <- modellmatt(stickprov$utfall,fitted(plusny))
    grundtest <- modellmatt(underlag$utfall,predict(grundny,newdata=underlag,type='response'))
    plustest <- modellmatt(underlag$utfall,predict(plusny,newdata=underlag,type='response'))
    grundbyte <- length(coef(grundmodell))==2 && coef(grundmodell)[2]*coef(grundny)[2]<=0
    plusbyte <- length(coef(plusmodell))==2 && coef(plusmodell)[2]*coef(plusny)[2]<=0
    dragningar[omdragning,] <- c(plustrana['auc']-grundtrana['auc'],grundtrana['auc']-grundtest['auc'],plustrana['auc']-plustest['auc'],grundtrana['brier']-grundtest['brier'],plustrana['brier']-plustest['brier'],grundtrana['intercept']-grundtest['intercept'],plustrana['intercept']-plustest['intercept'],grundtrana['lutning']-grundtest['lutning'],plustrana['lutning']-plustest['lutning'],as.integer(attr(grundny,'numerisk_varning') || attr(plusny,'numerisk_varning')),min(grundtrana['kalibrering_giltig'],grundtest['kalibrering_giltig']),min(plustrana['kalibrering_giltig'],plustest['kalibrering_giltig']),as.integer(grundbyte),as.integer(plusbyte),max(grundtrana['kalibrering_konstant'],grundtest['kalibrering_konstant']),max(plustrana['kalibrering_konstant'],plustest['kalibrering_konstant']))
  }
  giltiga <- is.finite(dragningar[,'delta'])
  if (!any(giltiga)) stop('Inga giltiga bootstrapdragningar: ',utfallsnamn,' ',basnamn)
  if (sum(giltiga)<.9*antal) warning('Fler an 10 procent misslyckade bootstrapdragningar: ',utfallsnamn,' ',basnamn)
  intervall <- quantile(dragningar[giltiga,'delta'],c(.025,.975),names=FALSE,na.rm=TRUE)
  grundkalibrering <- kalibreringssumma(dragningar,'grund',grundmatt,length(coef(grundmodell))==2,giltiga)
  pluskalibrering <- kalibreringssumma(dragningar,'plus',plusmatt,length(coef(plusmodell))==2,giltiga)
  sammanstallning <- data.frame(utfall=utfallsnamn,troskel=troskel,basmodell=basnamn,antal=nrow(underlag),fall=sum(underlag$utfall),icke_fall=sum(underlag$utfall==0),parametrar=length(coef(plusmodell))-1,fall_per_parameter=sum(underlag$utfall)/(length(coef(plusmodell))-1),auc_grund=grundmatt['auc'],auc_grund_nedre=grundintervall[1],auc_grund_ovre=grundintervall[3],auc_plus=plusmatt['auc'],auc_plus_nedre=plusintervall[1],auc_plus_ovre=plusintervall[3],delta=plusmatt['auc']-grundmatt['auc'],delta_nedre=intervall[1],delta_ovre=intervall[2],lrt_p=prov,auc_grund_korrigerad=grundmatt['auc']-mean(dragningar[,'grundoptimism'],na.rm=TRUE),auc_plus_korrigerad=plusmatt['auc']-mean(dragningar[,'plusoptimism'],na.rm=TRUE),brier_grund_korrigerad=grundmatt['brier']-mean(dragningar[,'grundbrier'],na.rm=TRUE),brier_plus_korrigerad=plusmatt['brier']-mean(dragningar[,'plusbrier'],na.rm=TRUE),intercept_grund_korrigerat=grundkalibrering$intercept,intercept_plus_korrigerat=pluskalibrering$intercept,lutning_grund_korrigerad=grundkalibrering$lutning,lutning_plus_korrigerad=pluskalibrering$lutning,bootstrap_begarda=antal,bootstrap_giltiga=sum(giltiga),bootstrap_numerisk_varning=sum(dragningar[,'varning'],na.rm=TRUE),bootstrap_kalibrering_giltiga=sum(apply(dragningar[,c('grundintercept','plusintercept','grundlutning','pluslutning'),drop=FALSE],1,function(varden)all(is.finite(varden)))),row.names=NULL)
  for(prefix in c('grund','plus')) {
    kontroll <- if(prefix=='grund') grundkalibrering else pluskalibrering
    sammanstallning[[paste0('kalibrering_',prefix,'_status')]] <- kontroll$status
    sammanstallning[[paste0('kalibrering_',prefix,'_giltiga')]] <- kontroll$giltiga
    sammanstallning[[paste0('kalibrering_',prefix,'_teckenbyten')]] <- kontroll$teckenbyten
    sammanstallning[[paste0('kalibrering_',prefix,'_nara_konstanta')]] <- kontroll$nara_konstanta
  }
  list(sammanstallning=sammanstallning,grundmodell=grundmodell,plusmodell=plusmodell,grundroc=grundroc,plusroc=plusroc,koefficienter=rbind(koefficienter(grundmodell,utfallsnamn,basnamn),koefficienter(plusmodell,utfallsnamn,paste0(basnamn,' + aktigrafi'))))
}
modellresultat <- list()
for (utfallsnummer in seq_len(2+length(trosklar))) {
  if (utfallsnummer<=2) {
    troskel <- c(30,15)[utfallsnummer]
    utfallsnamn <- paste0('AHI >= ',troskel)
    analysdata <- underlag[complete.cases(underlag[c('apneindex',gemensamma)]),]
    analysdata$utfall <- as.integer(analysdata$apneindex >= troskel)
  } else {
    troskel <- trosklar[utfallsnummer-2]
    utfallsnamn <- paste0('T90 >= ',troskel,'%')
    analysdata <- underlag[complete.cases(underlag[c('hypoxiprocent',gemensamma)]),]
    analysdata$utfall <- as.integer(analysdata$hypoxiprocent >= troskel)
  }
  for (basnummer in 1:2) {
    basnamn <- c('Klinisk','Klinisk + STOP-Bang')[basnummer]
    basvariabler <- c(kliniska,if(basnummer==2) 'stoppbang')
    nyckel <- paste(utfallsnamn,basnamn,sep=' | ')
    message('Analyserar ',nyckel,'; n=',nrow(analysdata),'; fall=',sum(analysdata$utfall))
    fro <- if(utfallsnummer==1) 100+basnummer else if(utfallsnummer>2 && troskel==10) 102+basnummer else slumptal+100*utfallsnummer+basnummer
    modellresultat[[nyckel]] <- jamfor(analysdata,utfallsnamn,troskel,basnamn,basvariabler,antal_bootstrap,fro)
  }
}
resultat$Modelljamforelser <- do.call(rbind,lapply(modellresultat,'[[','sammanstallning'))
resultat$Koefficienter <- do.call(rbind,lapply(modellresultat,'[[','koefficienter'))
resultat$T90_kanslighet <- resultat$Modelljamforelser[grepl('^T90',resultat$Modelljamforelser$utfall),]
resultat$Tabell2 <- resultat$Modelljamforelser[resultat$Modelljamforelser$utfall %in% c('AHI >= 30','T90 >= 10%'),]
resultat$Aktigrafieffekter <- resultat$Koefficienter[resultat$Koefficienter$variabel %in% aktigrafi,]
resultat$Aktigrafieffekter$forandring <- ifelse(resultat$Aktigrafieffekter$variabel=='regelbundenhet',-10,-5)
resultat$Aktigrafieffekter$oddskvot <- with(resultat$Aktigrafieffekter,exp(beta*forandring))
resultat$Aktigrafieffekter$nedre <- with(resultat$Aktigrafieffekter,exp(beta*forandring-qnorm(.975)*standardfel*abs(forandring)))
resultat$Aktigrafieffekter$ovre <- with(resultat$Aktigrafieffekter,exp(beta*forandring+qnorm(.975)*standardfel*abs(forandring)))
komparatorer <- list()
stoppmodeller <- list()
beslut <- list()
ridge <- list()
for (utfallsnamn in c('AHI >= 30','T90 >= 10%')) {
  nyckel <- paste(utfallsnamn,'Klinisk',sep=' | ')
  grund <- modellresultat[[nyckel]]
  analysdata <- model.frame(grund$plusmodell)
  valt <- if(grepl('AHI',utfallsnamn)) complete.cases(underlag[c('apneindex',gemensamma)]) else complete.cases(underlag[c('hypoxiprocent',gemensamma)])
  analysdata$stoppbang <- underlag$stoppbang[valt]
  stoppvariabel <- if(grepl('AHI',utfallsnamn)) 'apneindex' else 'hypoxiprocent'
  stoppdata <- underlag[complete.cases(underlag[c(stoppvariabel,'stoppbang',aktigrafi)]),]
  stoppdata$utfall <- as.integer(stoppdata[[stoppvariabel]] >= if(grepl('AHI',utfallsnamn))30 else 10)
  stoppmodeller[[utfallsnamn]] <- jamfor(stoppdata,utfallsnamn,if(grepl('AHI',utfallsnamn))30 else 10,'STOP-Bang','stoppbang',antal_bootstrap,if(grepl('AHI',utfallsnamn))401 else 402)
  for (modellnamn in c('STOP-Bang utan BMI','SRI enbart','SE enbart','SRI utover SE','SE utover SRI')) {
    basvariabler <- switch(modellnamn,'STOP-Bang utan BMI'=c('stoppbang','man','alder','diabetesar'),'SRI utover SE'=c(kliniska,'somneffektivitet'),'SE utover SRI'=c(kliniska,'regelbundenhet'),kliniska)
    tillagg <- switch(modellnamn,'SRI enbart'='regelbundenhet','SE enbart'='somneffektivitet','SRI utover SE'='regelbundenhet','SE utover SRI'='somneffektivitet',aktigrafi)
    grundmodell <- anpassa(reformulate(basvariabler,'utfall'),analysdata)
    plusmodell <- anpassa(reformulate(c(basvariabler,tillagg),'utfall'),analysdata)
    komparatorer[[length(komparatorer)+1]] <- data.frame(utfall=utfallsnamn,jamforelse=modellnamn,antal=nrow(analysdata),fall=sum(analysdata$utfall),auc_grund=aucberakna(analysdata$utfall,fitted(grundmodell)),auc_plus=aucberakna(analysdata$utfall,fitted(plusmodell)),lrt_p=anova(grundmodell,plusmodell,test='LRT')$`Pr(>Chi)`[2])
  }
  for (sannolikhetstroskel in seq(.05,.50,by=.01)) {
    nettovinst <- function(sannolikhet) mean(sannolikhet>=sannolikhetstroskel & analysdata$utfall==1)-mean(sannolikhet>=sannolikhetstroskel & analysdata$utfall==0)*sannolikhetstroskel/(1-sannolikhetstroskel)
    beslut[[length(beslut)+1]] <- data.frame(utfall=utfallsnamn,troskel=sannolikhetstroskel,klinisk=nettovinst(fitted(grund$grundmodell)),aktigrafi=nettovinst(fitted(grund$plusmodell)),alla=mean(analysdata$utfall)-(1-mean(analysdata$utfall))*sannolikhetstroskel/(1-sannolikhetstroskel),ingen=0)
  }
  if(grepl('AHI',utfallsnamn)) for (basnummer in 1:2) {
    basvariabler <- c(kliniska,aktigrafi,if(basnummer==2)'stoppbang')
    for(straff in c(0,1)) {
      modell <- rms::lrm(reformulate(basvariabler,'utfall'),data=analysdata,x=TRUE,y=TRUE,penalty=straff)
      ridge[[length(ridge)+1]] <- data.frame(modell=c('Klinisk + aktigrafi','Klinisk + STOP-Bang + aktigrafi')[basnummer],straff=straff,auc=aucberakna(analysdata$utfall,predict(modell,type='fitted')))
    }
  }
}
resultat$Komparatorer <- do.call(rbind,komparatorer)
resultat$STOP_Bang_ensamt <- do.call(rbind,lapply(stoppmodeller,'[[','sammanstallning'))
resultat$Koefficienter <- rbind(resultat$Koefficienter,do.call(rbind,lapply(stoppmodeller,'[[','koefficienter')))
resultat$OSA_grupper <- as.data.frame(table(underlag$osagrupp))
resultat$Beslutskurvor <- do.call(rbind,beslut)
resultat$Ridge <- do.call(rbind,ridge)
linjara <- list()
for (utfall in c(aktigrafi,'vakenhet','somntid','somnperiod')) for (extra in c(FALSE,TRUE)) {
  variabler <- c('osagrupp',kliniska,if(extra)'somnkvalitet')
  analysdata <- underlag[complete.cases(underlag[c(utfall,variabler)]),]
  if(nrow(analysdata)<20) next
  modell <- lm(reformulate(variabler,utfall),data=analysdata)
  skattningar <- summary(modell)$coefficients
  intervall <- confint(modell)
  linjara[[length(linjara)+1]] <- data.frame(utfall=utfall,psqi_justering=extra,antal=nrow(analysdata),variabel=rownames(skattningar),beta=skattningar[,1],nedre=intervall[,1],ovre=intervall[,2],p=skattningar[,4],row.names=NULL)
}
resultat$Linjara_modeller <- do.call(rbind,linjara)
korrelationer <- list()
for (aktigrafivariabel in c(aktigrafi,'vakenhet','somntid','somnperiod','mattligaktivitet','intensivaktivitet')) for (andningsvariabel in c('apneindex','desaturationsindex','hypoxiprocent','medelsaturation')) {
  valt <- complete.cases(underlag[c(aktigrafivariabel,andningsvariabel)])
  prov <- if(sum(valt)>=10) suppressWarnings(cor.test(underlag[[aktigrafivariabel]][valt],underlag[[andningsvariabel]][valt],method='spearman',exact=FALSE)) else NULL
  korrelationer[[length(korrelationer)+1]] <- data.frame(aktigrafi=aktigrafivariabel,andning=andningsvariabel,antal=sum(valt),rho=if(is.null(prov))NA else unname(prov$estimate),p=if(is.null(prov))NA else prov$p.value)
}
resultat$Korrelationer <- do.call(rbind,korrelationer)
resultat$Korrelationer$p_fdr <- p.adjust(resultat$Korrelationer$p,method='BH')
resultat$Tidsskillnad <- data.frame(status='HSAT-datum och verifierat aktigrafifonster saknas; inga tidsintervall har beraknats.')
if(nzchar(datumfil)) {
  datumunderlag <- read.csv(datumfil,check.names=FALSE,colClasses='character',na.strings=c('','NA'))
  if(!all(c('id','hsat_datum','aktigrafi_start','aktigrafi_slut') %in% names(datumunderlag))) stop('Datumfilen maste innehalla id, hsat_datum, aktigrafi_start, aktigrafi_slut.')
  if(anyDuplicated(datumunderlag$id)) stop('Dubbel deltagarnyckel i datumfilen.')
  for(kolumn in c('hsat_datum','aktigrafi_start','aktigrafi_slut')) {
    omvandlad <- as.Date(datumunderlag[[kolumn]],format='%Y-%m-%d')
    if(any(!is.na(datumunderlag[[kolumn]]) & is.na(omvandlad))) stop('Ogiltigt datum; anvand YYYY-MM-DD.')
    datumunderlag[[kolumn]] <- omvandlad
  }
  if(any(datumunderlag$aktigrafi_slut < datumunderlag$aktigrafi_start,na.rm=TRUE)) stop('Aktigrafins slutdatum ligger fore startdatum.')
  datumunderlag <- merge(underlag['id'],datumunderlag,by='id',all.x=TRUE,sort=FALSE)
  fullstandiga <- complete.cases(datumunderlag[c('hsat_datum','aktigrafi_start','aktigrafi_slut')])
  datumunderlag <- datumunderlag[fullstandiga,]
  if(!nrow(datumunderlag)) stop('Inga fullstandiga verifierade datumtripletter.')
  skillnad <- as.numeric(datumunderlag$hsat_datum)- (as.numeric(datumunderlag$aktigrafi_start)+as.numeric(datumunderlag$aktigrafi_slut))/2
  avstand <- pmax(as.numeric(datumunderlag$aktigrafi_start-datumunderlag$hsat_datum),as.numeric(datumunderlag$hsat_datum-datumunderlag$aktigrafi_slut),0)
  resultat$Tidsskillnad <- data.frame(matt=c('HSAT minus aktigrafins mittpunkt, dagar','Absolut avstand till aktigrafifonstret, dagar'),antal=length(skillnad),saknade=sum(!fullstandiga),medel=c(mean(skillnad),mean(avstand)),sd=c(sd(skillnad),sd(avstand)),median=c(median(skillnad),median(avstand)),q1=c(quantile(skillnad,.25),quantile(avstand,.25)),q3=c(quantile(skillnad,.75),quantile(avstand,.75)),minimum=c(min(skillnad),min(avstand)),maximum=c(max(skillnad),max(avstand)),overlappar=sum(avstand==0))
}
figurtema <- ggplot2::theme_classic(base_size=10,base_family='Arial') + ggplot2::theme(text=ggplot2::element_text(colour='black'),axis.text=ggplot2::element_text(colour='black'),legend.position='bottom',panel.spacing=grid::unit(26,'pt'),plot.background=ggplot2::element_rect(fill='white',colour=NA))
sparafigur <- function(namn,bredd,hojd,rita) {
  grDevices::tiff(file.path(utdatamapp,namn),width=bredd,height=hojd,units='in',res=600,compression='lzw',type='cairo',bg='white')
  tryCatch(rita(),finally=grDevices::dev.off())
  if(is.na(file.info(file.path(utdatamapp,namn))$size) || file.info(file.path(utdatamapp,namn))$size<1024) stop('Figuren exporterades inte korrekt: ',namn)
}
sparafigur('Fig1.tif',7.5,6.3,function() {
  grid::grid.newpage()
  ruta <- function(text,x,y,bredd=.48,hojd=.105) {
    grid::grid.rect(x=x,y=y,width=bredd,height=hojd,gp=grid::gpar(fill='white',col='black'))
    grid::grid.text(text,x=x,y=y,gp=grid::gpar(fontsize=10,fontfamily='Arial'))
  }
  pil <- function(x1,y1,x2,y2) grid::grid.lines(x=c(x1,x2),y=c(y1,y2),arrow=grid::arrow(length=grid::unit(.10,'inches')),gp=grid::gpar(lwd=.8))
  ruta('EPSONIP participants invited to sleep substudy\n(n = 345)',.35,.91,.57)
  ruta('Declined\n(n = 158)',.80,.76,.29)
  ruta('Included in sleep substudy\n(n = 187)',.35,.63)
  ruta('HSAT completed\n(n = 164)',.35,.45)
  ruta('HSAT not completed\n(n = 23)',.80,.54,.29)
  ruta(paste0('Descriptive sample with HSAT and actigraphy\n(n = ',nrow(underlag),')'),.35,.27,.57)
  ruta('Actigraphy not completed\n(n = 16)',.80,.36,.29)
  ruta(paste0('Complete AHI, ODI, SRI and sleep efficiency\n(n = ',sum(complete.cases(underlag[c('apneindex','desaturationsindex',aktigrafi)])),')'),.35,.09,.57)
  pil(.35,.857,.35,.684);pil(.35,.76,.65,.76)
  pil(.35,.578,.35,.503);pil(.35,.54,.65,.54)
  pil(.35,.398,.35,.323);pil(.35,.36,.65,.36)
  pil(.35,.218,.35,.143)
})
figurdata <- underlag[!is.na(underlag$regelbundenhet),]
gruppantal <- as.data.frame(table(figurdata$osagrupp))
names(gruppantal) <- c('osagrupp','antal')
figur2 <- ggplot2::ggplot(figurdata,ggplot2::aes(x=osagrupp,y=regelbundenhet))+ggplot2::geom_violin(fill='grey92',colour='grey40',trim=TRUE)+ggplot2::geom_boxplot(width=.16,outlier.shape=NA,fill='white')+ggplot2::geom_point(position=ggplot2::position_jitter(width=.10,height=0,seed=slumptal),size=1.0,alpha=.65)+ggplot2::geom_text(data=gruppantal,ggplot2::aes(x=osagrupp,y=max(figurdata$regelbundenhet)+6,label=paste0('n = ',antal)),inherit.aes=FALSE,size=3.2,family='Arial')+ggplot2::labs(x=NULL,y='Sleep Regularity Index')+figurtema
sparafigur('Fig2.tif',7.5,4.4,function() print(figur2))
rocdata <- list()
for(utfallsnamn in c('AHI >= 30','T90 >= 10%')) for(basnamn in c('Klinisk','Klinisk + STOP-Bang')) {
  vald <- modellresultat[[paste(utfallsnamn,basnamn,sep=' | ')]]
  for(tillagg in c(FALSE,TRUE)) {
    kurva <- if(tillagg) vald$plusroc else vald$grundroc
    rocdata[[length(rocdata)+1]] <- data.frame(falskpositiv=1-kurva$specificities,kanslighet=kurva$sensitivities,utfall=if(grepl('AHI',utfallsnamn))'Severe OSA' else 'T90 >= 10%',basmodell=if(basnamn=='Klinisk')'Clinical' else 'Clinical + STOP-Bang',modell=if(tillagg)'With actigraphy' else 'Without actigraphy')
  }
}
rocdata <- do.call(rbind,rocdata)
rocdata$utfall <- factor(rocdata$utfall,levels=c('Severe OSA','T90 >= 10%'))
paneler <- data.frame(utfall=rep(c('Severe OSA','T90 >= 10%'),2),basmodell=rep(c('Clinical','Clinical + STOP-Bang'),each=2),bokstav=LETTERS[1:4])
figur3 <- ggplot2::ggplot(rocdata,ggplot2::aes(falskpositiv,kanslighet,linetype=modell))+ggplot2::geom_abline(slope=1,intercept=0,colour='grey75',linewidth=.4)+ggplot2::geom_path(linewidth=.65)+ggplot2::geom_text(data=paneler,ggplot2::aes(x=.02,y=.98,label=bokstav),inherit.aes=FALSE,hjust=0,vjust=1,fontface='bold',size=3.3,family='Arial')+ggplot2::facet_grid(basmodell~utfall)+ggplot2::scale_linetype_manual(values=c('With actigraphy'='dashed','Without actigraphy'='solid'))+ggplot2::coord_equal()+ggplot2::scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25),expand=c(0,0))+ggplot2::scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.25),expand=c(0,0))+ggplot2::labs(x='1 - Specificity',y='Sensitivity',linetype=NULL)+figurtema
sparafigur('Fig3.tif',7.5,7,function() print(figur3))
beslutsdata <- do.call(rbind,lapply(c('klinisk','aktigrafi','alla','ingen'),function(modellnamn) data.frame(resultat$Beslutskurvor[c('utfall','troskel')],modell=unname(c(klinisk='Clinical',aktigrafi='Clinical + actigraphy',alla='Refer all',ingen='Refer none')[modellnamn]),nettovinst=resultat$Beslutskurvor[[modellnamn]])))
beslutsfigur <- ggplot2::ggplot(beslutsdata,ggplot2::aes(troskel,nettovinst,linetype=modell))+ggplot2::geom_line(linewidth=.6)+ggplot2::facet_wrap(~utfall)+ggplot2::coord_cartesian(ylim=c(-.02,max(.20,ceiling(max(beslutsdata$nettovinst,na.rm=TRUE)*10)/10)))+ggplot2::labs(x='Threshold probability',y='Net benefit',linetype=NULL)+figurtema
sparafigur('S1Fig.tif',7.5,4,function() print(beslutsfigur))
resultat$Korning <- data.frame(parameter=c('Tid','R','Bootstrap','Seed','T90 primar','T90 alternativ','Registerfilens MD5','Fig1 rekryteringssiffror'),varde=c(as.character(Sys.time()),R.version.string,as.character(antal_bootstrap),as.character(slumptal),'10',paste(trosklar[trosklar!=10],collapse=', '),unname(tools::md5sum(registerfil)),'345, 187, 164: rekryteringslogg; efterfoljande deltagarantal beraknas fran analysdata'))
resultat$Indata <- data.frame(fil=c('Register','Aktigrafi'),md5=unname(tools::md5sum(c(registerfil,aktigrafifil))))
resultat$Datakontroll <- data.frame(parameter='PSQI 5j kommentar kontrollerad',varde=as.character(all(ursprungsdata$psqi_5j_kommentar_kontrollerad %in% TRUE)))
resultat$Programversioner <- data.frame(program=c('R',paket),version=c(as.character(getRversion()),vapply(paket,function(namn) as.character(utils::packageVersion(namn)),character(1))))
arbetsbok <- openxlsx::createWorkbook()
for (blad in names(resultat)) {
  openxlsx::addWorksheet(arbetsbok,blad)
  openxlsx::writeData(arbetsbok,blad,resultat[[blad]])
  openxlsx::freezePane(arbetsbok,blad,firstRow=TRUE)
  bladnummer <- match(blad,names(resultat))
  arbetsbok$worksheets[[bladnummer]]$drawing <- character()
  arbetsbok$worksheets[[bladnummer]]$legacyDrawing <- character()
  arbetsbok$worksheets_rels[[bladnummer]] <- character()
  arbetsbok$worksheets[[bladnummer]]$dimension <- paste0('<dimension ref="A1:',openxlsx::int2col(ncol(resultat[[blad]])),nrow(resultat[[blad]])+1,'"/>')
}
openxlsx::saveWorkbook(arbetsbok,file.path(utdatamapp,'Analysresultat.xlsx'),overwrite=TRUE)
write.csv(resultat$T90_kanslighet,file.path(utdatamapp,'T90_kanslighet.csv'),row.names=FALSE,na='')
writeLines(capture.output(sessionInfo()),file.path(utdatamapp,'Programmiljo.txt'))
message('Klart. Resultat finns i ',normalizePath(utdatamapp))
