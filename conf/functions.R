## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).
Setup = function(){
  if(file.exists('temp/referencePoints.csv')){file.remove('temp/referencePoints.csv')}
  referencePoints <- data.frame(goal=as.character(),
                                method = as.character(),
                                reference_point = as.character())
  write.csv(referencePoints, 'temp/referencePoints.csv', row.names=FALSE)
}

FIS <- function(layers, status_year) {

  status_year<-2016

  #catch data

  c <- SelectLayersData(layers, layers='fis_meancatch_mra_2018', narrow = TRUE)%>%
    dplyr::select(region_id    = id_num,
    stock_id = category,
    year,
    catch          = val_num)




  #  b_bmsy data
  b <- SelectLayersData(layers, layers='fis_b_bmsy_mra_2018', narrow = TRUE) %>%
      dplyr::select(region_id    = id_num,
                  stock_id = category,
                  year,
                  bbmsy          = val_num)


  # general formatting:
  c <- c %>%
    dplyr::mutate(catch = as.numeric(catch)) %>%
    dplyr::mutate(year = as.numeric(as.character(year))) %>%
    dplyr::mutate(stock_id=as.character(stock_id))%>%
    dplyr::mutate(region_id = as.numeric(as.character(region_id))) %>%
    dplyr::select(region_id, year,  stock_id, catch)

  # general formatting:
  b <- b %>%
    dplyr::mutate(bbmsy = as.numeric(bbmsy)) %>%
    dplyr::mutate(region_id = as.numeric(as.character(region_id))) %>%
    dplyr::mutate(year = as.numeric(as.character(year))) %>%
    dplyr::mutate(stock_id = as.character(stock_id))


  ####
  # STEP 1. Calculate scores for Bbmsy values
  ####
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(
    b$bbmsy < lowerBuffer,
    b$bbmsy,
    ifelse (b$bbmsy >= lowerBuffer &
              b$bbmsy <= upperBuffer, 1, NA)
  )
  b$score = ifelse(!is.na(b$score),
                   b$score,
                   ifelse(
                     1 - alpha * (b$bbmsy - upperBuffer) > beta,
                     1 - alpha * (b$bbmsy - upperBuffer),
                     beta
                   ))


  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####
  data_fis <- c %>%
    dplyr::inner_join(b, by = c('region_id', 'stock_id', 'year')) %>%
    dplyr::select(region_id, stock_id, year, catch, bbmsy, score)


  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###

  ## this takes the mean score within each region and year
  ## assessments prior to 2018 used the median
  data_fis_mra <- data_fis %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::mutate(mean_score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup()



  data_fis_mra_status <- data_fis_mra %>%
    dplyr::mutate(score= mean_score)%>%
    dplyr::select(region_id,stock_id,year,catch,score)  #without filtering year


  data_fis_mra_2018 <- data_fis_mra_status %>%
    dplyr::select(region_id,
                  stock_id,
                  year,
                  catch,
                  score) %>%
    dplyr::filter(year == status_year) # referring to the current status

  write.csv(data_fis_mra_2018, file.path('eez/temp/FIS_summary.csv'), row.names = FALSE)


  status_data <- data_fis_mra_status %>%
    dplyr::select(region_id, stock_id, year, catch, score)


  ###
  # STEP 4. Calculate status for each region
  ###

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year

  status_data <- status_data %>%
    dplyr::group_by(year, region_id) %>%
    dplyr::mutate(SumCatch = sum(catch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wprop = catch / SumCatch)

  #4b. The "score" and "weight" values per taxon per SAUP region are used to
  #calculate a geometric weighted mean across taxa for each saup_id region

  status_data <- status_data %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status = prod(score ^ wprop)) %>%
    dplyr::ungroup()


  ###
  # STEP 5. Get yearly status and trend
  ###

  status <-  status_data %>%
    dplyr::filter(year >=max(year, na.rm=TRUE)) %>%
    dplyr::mutate(score = round(status * 100, 1),
                  dimension = 'status') %>%
    dplyr::select(region_id, score, dimension)

  # calculate trend
  trend_years <- (status_year - 4):(status_year)

  trend <-
    CalculateTrend(status_data = status_data, trend_years = trend_years)

  ## Reference Point Accounting
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "FIS", method = "Functional relationship (B/Bmsy)",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  ## Reference Point End

  # assemble dimensions
  scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'FIS') %>%
    dplyr::filter(region_id != 255)
  scores <- data.frame(scores)

  return(scores)

}



FP <- function(layers, scores) {

  # scores
  s <- scores %>%
    dplyr::filter(goal %in% c('FIS')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience')))


  s <- s  %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO <- function(layers, status_year) {

  Sustainability <- 1.0
  status_year<-2015


r <- SelectLayersData(layers, layers = "ao_access_mra_2018", narrow=TRUE) %>%
  dplyr::rename(region_id = id_num, access = val_num) %>%
  na.omit()

ry <-
  SelectLayersData(layers,layers= "ao_need_mra_2018", narrow=TRUE)%>%
  dplyr::rename(region_id = id_num, need = val_num)%>%
  dplyr::left_join(r, by = c("region_id"))%>%
  dplyr::filter(region_id==147)%>%
  dplyr::rename(year=year.x)

# model
ry <- ry %>%
  dplyr::mutate(Du = (1 - need) * (1 - access)) %>%
  dplyr::mutate(status = (1 - Du) * Sustainability)

# status
r.status <- ry %>%
  dplyr::filter(year == status_year) %>%
  dplyr::select(region_id, status) %>%
  dplyr::mutate(status = status * 100) %>%
  dplyr::select(region_id, score = status) %>%
  dplyr::mutate(dimension = 'status')

# trend
trend_years <- (status_year - 4):(status_year)

r.trend <- CalculateTrend(status_data = ry, trend_years = trend_years)

## Reference Point Accounting
rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  rbind(data.frame(goal = "AO", method = "Spatial and temporal comparision",
                   reference_point = NA))
write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


# return scores
scores <- rbind(r.status, r.trend) %>%
  dplyr::mutate(goal = 'AO')%>%
  filter(region_id==147)

return(scores)

}

CS <- function(layers) {

  status_year<-2014


# get data together:
extent <- layers$data [['hab_mangrove_extent_mra_2018']] %>%
  select(region_id=rgn_id, habitat, km2) %>%
  mutate(habitat = as.character(habitat))

health <-  layers$data[['hab_mangrove_health_mra_2018']] %>%
  select(region_id=rgn_id, habitat, health) %>%
  mutate(habitat = as.character(habitat))

trend <-layers$data[['hab_mangrove_health_trend_mra_2018']] %>%
  select(region_id=rgn_id, habitat, trend) %>%
  mutate(habitat = as.character(habitat))

## join layer data
d <-  extent %>%
  dplyr::full_join(health, by = c("region_id", "habitat")) %>%
  dplyr::full_join(trend, by = c("region_id", "habitat"))

## set ranks for each habitat
habitat.rank <- c('mangrove'= 139)# if there are more than 1 habitats
habitat.rank <- c('mangroves'= 1)

## limit to CS habitats and add rank
d <- d %>%
  mutate(
    rank = habitat.rank[habitat],
    km2 = ifelse(km2==0, NA, km2))


# status
status <- d %>%
  dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(km2)) %>%
  dplyr::group_by(region_id) %>%
  dplyr::summarize(score = pmin(1, sum(rank * health * km2, na.rm = TRUE) / (sum(
    km2 * rank, na.rm = TRUE
  ))) * 100,
  dimension= 'status') %>%
  ungroup()

# trend

trend <- d %>%
  filter(!is.na(rank) & !is.na(trend) & !is.na(km2)) %>%
  dplyr::group_by(region_id) %>%
  dplyr::summarize(score = sum(rank * trend * km2, na.rm = TRUE) / (sum(km2 *
                                                                          rank, na.rm = TRUE)),
                   dimension = 'trend') %>%
  dplyr::ungroup()

scores_CS <- rbind(status, trend)  %>%
  dplyr::mutate(goal = 'CS') %>%
  dplyr::select(goal, dimension, region_id, score)

## Reference Point Accounting
rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  rbind(data.frame(goal = "CS", method = "Health/condition variable based on current vs. historic extent",
                   reference_point = "varies for each region/habitat"))
write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
## Reference Point End

## create weights file for pressures/resilience calculations
weights <- extent %>%
  dplyr::filter(km2 > 0) %>%
  dplyr::mutate(rank = habitat.rank[habitat]) %>%
  dplyr::mutate(extent_rank = km2 * rank) %>%
  dplyr::mutate(layer = "element_wts_cs_km2_x_storage") %>%
  dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)

write.csv(
  weights,
  sprintf(here("temp/element_wts_cs_km2_x_storage_%s.csv"), status_year),
  row.names = FALSE
)

layers$data$element_wts_cs_km2_x_storage <- weights


# return scores
return(scores_CS)
}

CP <- function(layers) {

  ## read in layers
  # layers for coastal protection/ put data together

  extent <- layers$data[['hab_mangrove_extent_mra_2018']]%>%
    bind_rows(layers$data[['hab_coral_extent_mra_2018']])%>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health<- layers$data[['hab_mangrove_health_mra_2018']]%>%
    bind_rows(layers$data[['hab_coral_health_mra_2018']]) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))


  trend<-layers$data[['hab_mangrove_health_trend_mra_2018']]%>%
    bind_rows(layers$data[['hab_coral_health_trend_mra_2018']]) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))


  ## join layer data
  d <-  extent %>%
    dplyr::full_join(health, by = c("region_id", "habitat")) %>%
    dplyr::full_join(trend, by = c("region_id", "habitat"))

  # Removing countries within the Baltic, Iceland, and North Sea regions (UK, Germany, Denmark)
  # because seaice edge is due to ice floating into the environment and does not provide coastal protection
  # for these regions


  ## set ranks for each habitat
  habitat.rank <- c(
    'coral'            = 4,
    'mangroves'         = 4
  )

  ## limit to CP habitats and add rank
  d <- d %>%
    dplyr::filter(habitat %in% names(habitat.rank)) %>%
    dplyr::mutate(rank = habitat.rank[habitat],
                  extent = ifelse(extent == 0, NA, extent))


  # status
  scores_CP <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
                                    (sum(
                                      extent * rank, na.rm = TRUE
                                    ))) * 100) %>%
    dplyr::mutate(dimension = 'status') %>%
    ungroup()

  # trend
  d_trend <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

  if (nrow(d_trend) > 0) {
    scores_CP <- dplyr::bind_rows(
      scores_CP,
      d_trend %>%
        dplyr::group_by(region_id) %>%
        dplyr::summarize(
          score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent * rank, na.rm =
                                                                    TRUE)),
          dimension = 'trend'
        )
    )
  } else {
    # if no trend score, assign NA
    scores_CP <- dplyr::bind_rows(scores_CP,
                                  d %>%
                                    dplyr::group_by(region_id) %>%
                                    dplyr::summarize(score = NA,
                                                     dimension = 'trend'))
  }

  ## finalize scores_CP
  scores_CP <- scores_CP %>%
    dplyr::mutate(goal = 'CP') %>%
    dplyr::select(region_id, goal, dimension, score)

  ### output data file for checking and data review
  scores_check <- spread(scores_CP, dimension, score) %>%
    dplyr::select(region_id, status, trend_score=trend)

  d_check <- d %>%
    dplyr::select(region_id, habitat, extent, health, trend, rank) %>%
    arrange(region_id, habitat) %>%
    left_join(scores_check, by="region_id")

  ### end: output...

  scores_CP <- scores_CP %>%
    mutate(
      goal = 'CP') %>%
    dplyr::select(region_id=region_id, goal, dimension, score)


  ## Reference Point Accounting
## reference points
rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  rbind(data.frame(goal = "CP", method = "Health/condition variable based on current vs. historic extent",
                   reference_point = "varies for each region/habitat"))
write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(rank = habitat.rank[habitat]) %>%
    dplyr::mutate(extent_rank = extent * rank) %>%
    dplyr::mutate(layer = "element_wts_cp_km2_x_protection") %>%
    dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)

  status_year<-c(2018) # has to be just one if not next function does not work
  write.csv(
    weights,
    sprintf(here("temp/element_wts_cp_km2_x_protection_%s.csv"),status_year))

  layers$data$element_wts_cp_km2_x_protection <- weights

  # return scores
  return(scores_CP)
}

TR <- function(layers, status_year, debug =FALSE) {
  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S
  pct_ref <- 90
  status_year<-2017

  ## read in layers
  tourism<-layers$data[['tr_jobs_pct_tourism_mra_2018']] %>%
    select(region_id=rgn_id, year,Ep)

  sustain <- layers$data[['tr_sustainability']] %>%
    dplyr::select(region_id=rgn_id,year, S_score)

  tr_data  <-tourism%>%
    dplyr::inner_join(sustain, by = c('region_id'))%>%
    rename(year=year.x)


  tr_model <- tr_data %>%
    dplyr::mutate(E   = Ep,
                  S   = (S_score - 1) / (7 - 1),
                  # scale score from 1 to 7.
                  Xtr = E * S)


  # regions with Travel Warnings

  rgn_travel_warnings <-layers$data[['tr_travelwarnings']]%>%
    select(region_id=rgn_id,year,multiplier)


  ## incorporate Travel Warnings
  tr_model <- tr_model %>%
    dplyr::left_join(rgn_travel_warnings, by = c('region_id', 'year')) %>%
    dplyr::mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
    dplyr::select(-multiplier)


  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    dplyr::filter(year >=2008) %>%
    group_by(year)%>%
    dplyr::mutate(Xtr_q = quantile(Xtr, probs = pct_ref / 100, na.rm = TRUE)) %>%
    dplyr::mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
    dplyr::ungroup()

  ## Reference Point Accounting
  ref_point <- tr_model %>%
    group_by(region_id) %>%
    dplyr::filter(year== status_year) %>%
    dplyr::select(Xtr_q) %>%
    unique() %>%
    data.frame() %>%
    .$Xtr_q

   rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "TR", method = 'spatial, pct_ref, th quantile',
                     reference_point= ref_point))

 write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  ## Reference Point End

  # get status
  tr_status <- tr_model %>%
    dplyr::filter(year == status_year) %>%
    dplyr::select(region_id , score = status) %>%
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = 'status') %>%
    dplyr::filter(region_id==147)


  # calculate trend

  trend_data <- tr_model %>%
    group_by(region_id) %>%
    dplyr::filter(!is.na(status))

  trend_years <- (max(status_year) - 4):(max(status_year))

  tr_trend <-
    CalculateTrend(status_data = trend_data, trend_years = trend_years) %>%
    dplyr::filter(region_id==147)


  # bind status and trend by rows
  tr_score <- dplyr::bind_rows(tr_status, tr_trend) %>%
    dplyr::mutate(goal = 'TR')


  # return final scores
  scores <- tr_score %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}


LIV <- function(layers) {

  # NOTE: scripts and related files for calculating these subgoals is located:
  # eez/archive
  # These data are no longer available and status/trend have not been updated since 2013

  status_year=2017

  # NOTE: scripts and related files for calculating these subgoals is located:
  # eez/archive
  # These data are no longer available and status/trend have not been updated since 2013

  ## status data
  status_liv <- layers$data[['liv_status_mra_2018']] %>%
    dplyr::select(-layer) %>%
    dplyr::mutate(goal = "LIV") %>%
    dplyr::select(region_id = rgn_id, goal, score = status) %>%
    dplyr::mutate(dimension = 'status')

  # trend data
  trend_liv <-layers$data[['liv_trend_mra_2018']]%>%
    dplyr::select(-layer) %>%
    dplyr::mutate(goal = "LIV") %>%
    dplyr::select(region_id = rgn_id, goal, score = trend) %>%
    dplyr::mutate(dimension = 'trend')


  scores <- rbind(status_liv, trend_liv) %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}


ECO <- function(layers) {

  # NOTE: scripts and related files for calculating these subgoals is located:
  # eez/archive
  # These data are no longer available and status/trend have not been updated since 2013

  status_year<- 2017
  ## status data
  status_eco <-
    layers$data[['eco_status_mra_2018']] %>%
    dplyr::select(-layer) %>%
    dplyr::mutate(goal = "ECO") %>%
    dplyr::select(region_id = rgn_id, goal, score = status) %>%
    dplyr::mutate(dimension = 'status')


  # trend data
  trend_eco <-
    layers$data[['eco_trend_mra_2018']] %>%
    dplyr::select(-layer) %>%
    dplyr::mutate(goal = "ECO") %>%
    dplyr::select(region_id = rgn_id, goal, score = trend) %>%
    dplyr::mutate(dimension = 'trend')


  scores <- rbind(status_eco, trend_eco) %>%
    dplyr::select(region_id, goal, dimension, score)


  return(scores)

}

LE <- function(scores, layers) {

  s <- scores %>%
    dplyr::filter(goal %in% c('LIV', 'ECO'),
                  dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "LE") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))

}

ICO <- function(layers, status_year=2018) {
  status_year<- 2018

  rk <-layers$data[['ico_spp_iucn_status_mra_2018']] %>%
    dplyr::select(
      region_id = rgn_id,
      sciname,
      iucn_sid,
      iucn_cat = category,
      year,
      eval_yr
    ) %>%
    dplyr::mutate(iucn_cat = as.character(iucn_cat)) %>%
    dplyr::group_by(region_id, iucn_sid) %>%
    dplyr::mutate(sample_n = length(na.omit(unique(eval_yr[eval_yr > status_year-19])))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sciname, region_id) %>%
    dplyr::mutate(sample_n = min(sample_n)) %>%
    dplyr::ungroup()

  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"
  #  EN <- "ENDANGERED (E)"
  #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  #  DD <- "INSUFFICIENTLY KNOWN (K)"
  #  DD <- "INDETERMINATE (I)"
  #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"

  w.risk_category <-
    data.frame(
      iucn_cat = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
      risk_score = c(0,  0.2,  0.3,  0.4,  0.6,  0.8,  1, NA)
    ) %>%
    dplyr::mutate(status_score = 1 - risk_score) %>%
    dplyr::mutate(iucn_cat = as.character(iucn_cat))

  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    dplyr::left_join(w.risk_category, by = 'iucn_cat') %>%
    dplyr::group_by(region_id, sciname, year) %>%
    dplyr::summarize(spp_mean = mean(status_score, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    dplyr::group_by(region_id,year) %>%
    dplyr::summarize(status = mean(spp_mean, na.rm = TRUE)) %>%
    dplyr::ungroup()

  ####### status
  status <- r.status %>%
    filter(year == status_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    select(region_id, score, dimension)

  ####### trend
  trend_years <- (status_year - 19):(status_year)

  # trend calculated with status filtered for species with 2+ iucn evaluations in trend_years
  r.status_filtered <- rk %>%
    dplyr::filter(sample_n >= 2) %>%
    dplyr::left_join(w.risk_category, by = 'iucn_cat') %>%
    dplyr::group_by(region_id, sciname, year) %>%
    dplyr::summarize(spp_mean = mean(status_score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status = mean(spp_mean, na.rm = TRUE)) %>%
    dplyr::ungroup() #why it is different if i put grouping by stats year or just year¿?

  trend <-
    CalculateTrend(status_data = r.status_filtered, trend_years = trend_years)


  ## Reference Point Accounting
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "ICO", method = "scaled IUCN risk categories",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  ## Reference Point End

  # return scores
  scores <-  rbind(status, trend) %>%
    dplyr::mutate('goal' = 'ICO') %>%
    dplyr::select(goal, dimension, region_id, score) %>%
    data.frame()

  return(scores)

}

LSP <- function(layers, status_year=2018){

status_year<-2018

ref_pct_cmpa <- 30
ref_pct_cp <- 30

total_area <-
  rbind(layers$data$area_inland20m_mra_2018,
        layers$data$area_offshore_mra_2018) %>% #total offshore/inland areas
  dplyr::select(region_id = rgn_id, area_km2, layer) %>%
  tidyr::spread(layer, area_km2) %>%
  dplyr::select(region_id,
                area_inland=area_inland20m_mra_2018,
                area_offshore=area_offshore_mra_2018
  )


offshore <-
  layers$data [["lsp_prot_area_lagfo_mra_2018"]]%>%
  dplyr::select(region_id = rgn_id,
                year,
                cmpa = a_prot_lag_km2)
inland <-
  layers$data [["lsp_prot_area_inland_mra_2018"]]%>%
  dplyr::select(region_id = rgn_id,
                year ,
                cp = a_prot_inl_km2)


# ry_offshore <-  layers$data$lsp_prot_area_offshore3nm %>%
#   select(region_id = rgn_id, year, cmpa = a_prot_3nm)
# ry_inland <- layers$data$lsp_prot_area_inland1km %>%
#   select(region_id = rgn_id, year, cp = a_prot_1km)
#
lsp_data <- full_join(offshore, inland, by = c("region_id", "year"))

# get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
# and calculate status score

status_data <- lsp_data %>%
  dplyr::full_join(total_area, by = "region_id") %>%
  dplyr::mutate(
    pct_cp    = pmin(cp   / area_inland   * 100, 100),
    pct_cmpa  = pmin(cmpa / area_offshore * 100, 100),
    status    = (pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1)) / 2
  ) %>%
  dplyr::filter(!is.na(status))

# extract status based on specified year

r.status <- status_data %>%
  dplyr::filter(year == status_year) %>%
  dplyr::mutate(score = status * 100) %>%
  dplyr::select(region_id, score) %>%
  dplyr::mutate(dimension = "status")

# calculate trend

trend_years <- (status_year - 4):(status_year)

r.trend <-
  CalculateTrend(status_data = status_data, trend_years = trend_years)


## Reference Point Accounting
rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  rbind(data.frame(goal = "LSP", method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                                 ref_pct_cp, "% coastal protected area"),
                   reference_point = "varies by area of region's eez and 1 km inland"))
write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
## Reference Point End

# return scores
scores <- dplyr::bind_rows(r.status, r.trend) %>%
  mutate(goal = "LSP")

return(scores[, c('region_id', 'goal', 'dimension', 'score')])

}


SP <- function(scores) {
  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    dplyr::filter(goal %in% c('ICO', 'LSP'),
                  dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW <- function(layers) {

  stat
  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  # layers

  cw_chemical_tr <- layers$data[['cw_chemical_trend']]%>%
    dplyr::filter(rgn_id==147)%>%
    dplyr::filter (year==max(year))%>%
    dplyr::select(-layer)

  cw_trash_trend<-  layers$data[['cw_trash_trend']] %>%
    dplyr::filter(rgn_id==147)%>%
    dplyr::filter (year==max(year))%>%
    dplyr::select(-layer)

  cw_pathogen_trend<-layers$data[['cw_pathogens_trend_mra_2018']]%>%
    dplyr::filter(rgn_id==147) %>%
    dplyr::filter (year==max(year))%>%
    dplyr::select(-layer)

  prs_nutrients<-layers$data[['po_nutrients_mra_2018']]%>%
    dplyr::filter(rgn_id==147) %>%
        dplyr::select(-layer)

  prs_chemical<-layers$data[['po_chemicals_mra_2018']]%>%
    dplyr::filter(rgn_id==147) %>%
        dplyr::select(-layer)

  prs_pathogens<-layers$data[['po_pathogens_mra_2018']]%>%
    dplyr::filter(rgn_id==147) %>%
     dplyr::select(-layer)
  # get data together:
  prs_data<- prs_chemical %>%
    bind_rows(prs_nutrients) %>%
    bind_rows(prs_pathogens)%>%
    dplyr::select(region_id=rgn_id, value=pressure_score)

  d_pressures <- prs_data %>%
    dplyr::mutate(pressure = 1 - value) %>%  # invert pressure
    dplyr::mutate(pressure = ifelse(pressure == 0 , pressure + 0.01, pressure)) %>% # add small modifier to zeros to
    dplyr::group_by(region_id) %>%                                                  # prevent zeros with geometric mean
    dplyr::summarize(score = geometric.mean2(pressure, na.rm = TRUE)) %>% # take geometric mean
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::ungroup()


  # get trend data together:
  trend_data<- cw_chemical_tr %>%
    bind_rows(cw_trash_trend) %>%
    bind_rows(cw_pathogen_trend)%>%
    dplyr::select(region_id=rgn_id, value=trend)


  d_trends <- trend_data %>%
    dplyr::mutate(trend = -1 * value)  %>%  # invert trends
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = mean(trend, na.rm = TRUE)) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::ungroup()


  # return scores
  scores <- rbind(d_pressures, d_trends) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  ## Reference Point Accounting
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CW", method = "spatial: pressures scaled from 0-1 at raster level",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  ## Reference Point End

  return(scores)
}


HAB <- function(layers) {
  status_year<-c(2014, 2018)


  extent <- layers$data[['hab_mangrove_extent_mra_2018']]%>%
    bind_rows(layers$data[['hab_coral_extent_mra_2018']])%>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health<- layers$data[['hab_mangrove_health_mra_2018']]%>%
    bind_rows(layers$data[['hab_coral_health_mra_2018']]) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))


  trend<-layers$data[['hab_mangrove_health_trend_mra_2018']]%>%
    bind_rows(layers$data[['hab_coral_health_trend_mra_2018']]) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))




  # join and limit to HAB habitats
  d <- health %>%
    dplyr::full_join(trend, by = c('region_id', 'habitat')) %>%
    dplyr::full_join(extent, by = c('region_id', 'habitat')) %>%
    dplyr::filter(
      habitat %in% c(
        'coral',
        'mangroves'
      )
    ) %>%
    dplyr::mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    dplyr::filter(!is.na(w))

  ## calculate scores
  status <- d %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(!is.na(health)) %>%
    dplyr::summarize(score = pmin(1, sum(health) / sum(w)) * 100,
                     dimension = 'status') %>%
    ungroup()

  trend <- d %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(!is.na(trend)) %>%
    dplyr::summarize(score =  sum(trend) / sum(w),
                     dimension = 'trend')  %>%
    dplyr::ungroup()

  scores_HAB <- rbind(status, trend) %>%
    dplyr::mutate(goal = "HAB") %>%
    dplyr::select(region_id, goal, dimension, score)

  ## Reference Point Accounting
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "HAB", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    filter(
      habitat %in% c(
        'mangrove',
        'coral'
      )
    ) %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(boolean = 1) %>%
    dplyr::mutate(layer = "element_wts_hab_pres_abs") %>%
    dplyr::select(rgn_id = region_id, habitat, boolean, layer)

  write.csv(weights,
            sprintf(here("temp/element_wts_hab_pres_abs_%s.csv"),
            row.names = FALSE))

  layers$data$element_wts_hab_pres_abs <- weights


  # return scores
  return(scores_HAB)
}


SPP <- function(layers) {

  status_year=2018


  status <- layers$data[[ "spp_status"]] %>%
    dplyr::filter(year == status_year, rgn_id==147) %>%
    dplyr::select(region_id = rgn_id,
                  score) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::mutate(score = score * 100)

  trend <- layers$data$spp_trend %>%
    dplyr::select(region_id = rgn_id,
                  score) %>%
    dplyr::filter(region_id==147) %>%
    dplyr::mutate(dimension = "trend")

  scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'SPP') %>%
    dplyr::select(region_id, goal, dimension, score)

  ## Reference Point Accounting
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "SPP", method = "Average of IUCN risk categories, scaled to historic extinction",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  ## Reference Point End

  return(scores)
}

BD <- function(scores) {
  d <- scores %>%
    dplyr::filter(goal %in% c('HAB', 'SPP')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[, c('region_id', 'goal', 'dimension', 'score')]))
}

FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')))
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}
