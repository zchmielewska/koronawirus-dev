aggregate <- function(v) {
  result <- c()
  for(i in 1:length(v)) {
    if(i == 1) {
      result[i] <- v[i] 
    } else {
      result[i] <- result[i-1] + v[i]
    }
  }
  return(result)
}

loadLastFile <- function() {
  last.file <- drop_dir("ECDC") %>% select(name) %>% arrange(desc(name)) %>% slice(1) %>% pull()
  last.data <- drop_read_csv(paste0("ECDC/", last.file))
  return(last.data)
}

checkNewData <- function(data) {
  result <- data
  
  # if the data is from yesterday; check if new exists
  last.file <- drop_dir("ECDC") %>% select(name) %>% arrange(desc(name)) %>% slice(1) %>% pull()
  if(as.Date(gsub(".csv", "", last.file)) < Sys.Date()) {
    base.url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
    new.exists <- !(testit::has_error(rio::import(paste0(base.url, Sys.Date(), ".xlsx"))))
    if(new.exists) {
      ecdc.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = FALSE)
      ecdc.filename <- paste0(Sys.Date(), ".csv")
      write.csv(ecdc.data, ecdc.filename)
      drop_upload(ecdc.filename, path = "ECDC")
      last.data <- drop_read_csv(paste0("ECDC/", ecdc.filename))
      result <- last.data
    }
  }
  
  return(result)
}

prepareECDCdata <- function(data) {
  result <- data %>% 
    as_tibble() %>% 
    mutate(Date = as.Date(paste(year, month, day, sep="-"))) %>% 
    rename(
      Country = `countriesAndTerritories`,
      CasesDelta = cases,
      DeathsDelta = deaths) %>% 
    mutate(Date = as.Date(Date),
           DeathsDelta = as.integer(DeathsDelta))
  
  return(result)    
}

getPolandData <- function(data) {
  
  # filter and rename
  result <- data %>% 
    filter(Country == "Poland")  %>% 
    select(-c(day, month, year, Country, popData2018, geoId)) %>% 
    arrange(Date) %>% 
    mutate(
      CasesTotal  = aggregate(CasesDelta),
      DeathsTotal = aggregate(DeathsDelta)
    )
  
  # add missing days
  date.min     <- min(result$Date)
  date.max     <- max(result$Date)
  date.period  <- seq(date.min, date.max, by = "1 day")
  
  for(i in 1:length(date.period)) {
    d <- date.period[i]
    n <- result %>% filter(Date == d) %>% nrow()
    
    # for no data, assume the same as previous day
    if(n == 0) {
      prev.data <- result %>% filter(Date == d-1) %>% select(-c(Date))
      prev.day  <- cbind(data.frame(Date = d), prev.data)
      result     <- rbind(result, prev.day)
    }
  }
  
  # add epidemia day
  result <- result %>% 
    arrange(Date) %>% 
    mutate(EpidemiaDay = 1:nrow(result))
  
  return(result)
}

getWorldData <- function(data) {
  result <- data %>% 
    select(Date, CasesDelta, DeathsDelta, Country, popData2018) %>% 
    group_by(Country) %>% 
    arrange(Country, Date) %>% 
    mutate(
      CasesTotal = aggregate(CasesDelta),
      DeathsTotal = aggregate(DeathsDelta),
    ) %>% 
    ungroup()
  return(result)
}
