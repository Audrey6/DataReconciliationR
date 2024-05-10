## Systems Data True Up
## Created by Audrey Wong

## How to Run Program---------------------------------------------------------------------

## ctrl+R shortcut to Run 
## load all the packages
## Under Data Load, paste the file link as .csv
## Run the entire program at once, should only take a couple minutes


# Packages --------------------------------------------------------------
# install.packages("backports")
# install.packages("ggplot2")
library(tidyverse)
install.packages("pacman")
library(pacman)
p_load(dplyr, stringr, lubridate, skimr, tidyr, readr, ggplot2)

# Data Load ---------------------------------------------------------------
# Paste in the file location in ""
dataset1 <- read.csv(file = "./Data/dataset1.csv")
dataset2 <- read.csv(file = "./Data/dataset2.csv")


# Parameters --------------------------------------------------------------
# Custom function adds in leading 0's
id_10_char <- function(id) {
  idfixed <- str_pad(str_trim(id), width = 10, side = "left", pad = "0")
  return(idfixed)
}
id_10_char(42)

# Custom function removes unnecessary characters from Serial Number column
serialFix <- function(serialNumber) {
  alphaNum <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = serialNumber)
  alphaNumNoSerial <- str_remove(string = alphaNum, pattern = "Serial")
  result <- str_trim(alphaNumNoSerial)
  return(result)
}

# Sample Str sub function
# str_sub(string =  "123456789", start = 1, end = 5)


# Data Transformation -----------------------------------------------------
# Calling our customer 10char function and applying to tables
# dataset1 file:
dataset1 <- dataset1 %>%
  mutate(DRUID = id_10_char(DRUID),
         SPID = id_10_char(SPID),
         AccountID = id_10_char(AccountID),
         SAID = id_10_char(SAID),
         SerialNumber = serialFix(SerialNumber)) %>%
  
# Changes enrollment status to char and changes NotEnrolled tag to Unenrolled
  mutate(EnrollmentStatus = as.character(EnrollmentStatus)) %>%
  mutate(CustomerLastName = as.character(CustomerLastName)) %>%
  mutate(CustomerLastName = tolower(CustomerLastName)) %>%
  mutate(FullAddress = tolower(FullAddress)) %>%
  mutate(EnrollmentStatus = case_when(EnrollmentStatus == "NotEnrolled" ~ "Unenrolled", 
                                      TRUE ~ EnrollmentStatus)) 
  
# dataset2 file:
dataset2 <- dataset2 %>% 
  mutate(DRUID = id_10_char(DRUID),
         SPID = id_10_char(SPID),
         AccountID = id_10_char(AccountID),
         SAID = id_10_char(SAID),
         SerialNumber = serialFix(SerialNumber)) %>%
  
# Separating the address into columns and reading in the state to confirm address input
  separate(FullAddress, into = c("street", "city", "state"), sep = ",", remove = FALSE) %>%
# Separating the customer full name into last and first name columns  
  separate(CustomerFullName, into = c("LastName", "FirstName"), sep = ",", remove = FALSE) %>%
  mutate(LastName = tolower(LastName)) %>%
  

# Separates the state and zip and removes whitespace
  mutate(zip = gsub(pattern = "[[:alpha:]]", x = state, replacement = "")) %>%
  mutate(state = gsub(pattern = "[[:digit:]]", x = state, replacement = "")) %>%
  mutate(state = gsub(pattern = " -", x = state, replacement = "")) %>%
  mutate(state = gsub(pattern = "-", x = state, replacement = "")) %>%
  mutate(state = str_trim(state)) %>%
  mutate(zip = str_sub(string = str_trim(zip), start = 1, end = 5))

# Test Data Frame---------------------------------------------------
# Loaded dataset1 table and added dataset2 table to bottom
TestDF <- dataset1 %>%
  mutate(dataSource = "dataset1") %>%
  select(DRUID, SPID, AccountID, SAID, SerialNumber, EnrollmentStatus, DeviceInstallDate, DeviceType, PremiseZip, dataSource) %>%
  bind_rows(dataset2 %>% 
              mutate(dataSource = "dataset2") %>%
              select(DRUID, SPID, AccountID, SAID, SerialNumber, EnrollmentStatus, DeviceInstallDate, DeviceType, zip, dataSource))

# Converts install date to date format, groups by enrollment status and sort
TestDF %>%
  mutate(DeviceInstallDate = mdy(DeviceInstallDate)) %>%
  group_by(EnrollmentStatus) %>%
  summarise(n = n(), 
            minDate = min(DeviceInstallDate),
            maxDate = max(DeviceInstallDate)) %>%
  arrange(desc(n))

# Taking new combined data frame and group by serial number
TestDF %>%
  group_by(SerialNumber) %>%
  summarise(n = n())
    
# Matching Unique ID ---------------------------------------------------------
# Match both dataset1 and dataset2 by DRUIDs, SPID, AccountID
# Changes enrollmentstatus to lowercase and filters to only look at enrolled and unenrolled
# Joins dataset1 table and only keeps matching observations using DRUID, SPID, AccountID 
MatchingUniqueID <- dataset2 %>%
  mutate(EnrollmentStatus = tolower(EnrollmentStatus)) %>%
  mutate(city = str_trim(city)) %>%
  mutate(DeviceInstallDate = mdy(DeviceInstallDate)) %>%
  filter(EnrollmentStatus == "enrolled" | EnrollmentStatus == "unenrolled") %>%
  #Let's label dataset2 columns as Seeload
  select(DRUID, SPID, AccountID, SeeloadSAID = SAID, SeeloadStatus = EnrollmentStatus, SeeloadCity = city, SeeloadInstall = DeviceInstallDate, SeeloadSerialNum = SerialNumber, SeeLoadLastName = LastName, SeeLoadZip = zip) %>%
    inner_join(dataset1 %>%
               mutate(EnrollmentStatus = tolower(EnrollmentStatus)) %>%
                 mutate(PremiseCity = str_trim(PremiseCity)) %>%
                 mutate(DeviceInstallDate = mdy(DeviceInstallDate)) %>%
                filter(EnrollmentStatus == "enrolled" | EnrollmentStatus == "unenrolled") %>%
                #Let's label dataset1 columns as Franklin
               select(DRUID, SPID, AccountID, FranklinSAID = SAID, FranklinStatus = EnrollmentStatus, FranklinCity = PremiseCity, FranklinInstall = DeviceInstallDate, FranklinSerialNum = SerialNumber, FranklinLastName = CustomerLastName, FranklinZip = PremiseZip),
              by = c("DRUID","SPID","AccountID") 
             ) %>%
  
# Flags status if it matches then true, else, false
  mutate(SAIDStatusMatch = ifelse(FranklinSAID == SeeloadSAID, yes = TRUE, no = FALSE)) %>%
  mutate(EnrollmentStatusMatch = ifelse(FranklinStatus == SeeloadStatus, yes = TRUE, no = FALSE)) %>%
  mutate(CityMatch = ifelse(FranklinCity == SeeloadCity, yes = TRUE, no = FALSE)) %>%
  mutate(InstallDateMatch = ifelse(FranklinInstall == SeeloadInstall, yes = TRUE, no = FALSE)) %>%
  mutate(SerialNumMatch = ifelse(FranklinSerialNum == SeeloadSerialNum, yes = TRUE, no = FALSE)) %>%
  mutate(LastNameMatch = ifelse(FranklinLastName == SeeLoadLastName, yes = TRUE, no = FALSE)) %>%
  mutate(ZipMatch = ifelse(FranklinZip == SeeLoadZip, yes = TRUE, no = FALSE)) %>%

# Order of columns
    select(DRUID, SPID, AccountID, SeeloadSAID, FranklinSAID, SAIDStatusMatch, SeeloadStatus, FranklinStatus, EnrollmentStatusMatch, SeeloadCity, FranklinCity, CityMatch,
           SeeloadInstall, FranklinInstall, InstallDateMatch, SeeloadSerialNum, FranklinSerialNum, SerialNumMatch, SeeLoadLastName, FranklinLastName, LastNameMatch, FranklinZip, SeeLoadZip, ZipMatch)

# Saves matching unique ID with true or false flags to file as csv into results folder
MatchingUniqueID %>%
  write_csv(file = paste0("./Results/", "MatchingUniqueID", " ", Sys.Date(), ".csv"))  

MatchingUniqueID %>%
  summary()

# Complete Match File --------------------------------------------------------
completeMatch <- dataset2 %>%
  mutate(EnrollmentStatus = tolower(EnrollmentStatus)) %>%
  separate(CustomerFullName, into = c("LastName", "FirstName"), sep = ",", remove = FALSE) %>%
  filter(EnrollmentStatus == "enrolled" | EnrollmentStatus == "unenrolled") %>%
  select(SAID, DRUID, SPID, AccountID, EnrollmentStatus, city, DeviceInstallDate, SerialNumber, LastName) %>%
  mutate(DeviceInstallDate = mdy(DeviceInstallDate)) %>%
  mutate(city = str_trim(city)) %>%
  inner_join(dataset1 %>%
               mutate(EnrollmentStatus = tolower(EnrollmentStatus)) %>%
               filter(EnrollmentStatus == "enrolled" | EnrollmentStatus == "unenrolled") %>%
               select(SAID, DRUID, SPID, AccountID, EnrollmentStatus, PremiseCity, DeviceInstallDate, SerialNumber, CustomerLastName) %>%
               mutate(DeviceInstallDate = mdy(DeviceInstallDate)) %>%
               mutate(PremiseCity = str_trim(PremiseCity)),
             by = c("SAID", "DRUID","SPID","AccountID", "EnrollmentStatus", 
                    "DeviceInstallDate", "city"="PremiseCity", "SerialNumber")
             
  )

# Saves complete matches to a file as csv into results folder
completeMatch %>%
  write_csv(file = paste0("./Results/", "CompleteMatch", " ", Sys.Date(), ".csv"))


# DRUID Comparison  ---------------------------------------------------
# Our unique ID combines the DRUID, SPID, and AccountID, see if these IDs match within the two files
# filter where column does not match

#DRUID is in dataset2 but does not match or exist in dataset1, then SPID or AccID does not match.
DRUIDMatch <- dataset2 %>%
  filter(!DRUID %in% MatchingUniqueID$DRUID) %>%
  filter(!DRUID %in% completeMatch$DRUID) %>%
  mutate(dataSource = "dataset2") %>%
  mutate(city = str_trim(city)) %>%
  select(DRUID, SeeLoadSAID = SAID, SeeLoadSPID = SPID, SeeLoadAccID = AccountID, SeeloadSource = dataSource,
         SeeLoadEnrollStatus = EnrollmentStatus, city, SeeLoadInstall = DeviceInstallDate, SeeLoadSerial = SerialNumber, LastName) %>%
    left_join(dataset1 %>%
                mutate(dataSource = "dataset1") %>%
                mutate(PremiseCity = str_trim(PremiseCity)) %>%
              select(DRUID, FranklinSAID = SAID, FranklinSPID = SPID, FranklinAccID = AccountID, FranklinSource = dataSource,
                     FranklinEnrollStatus = EnrollmentStatus, PremiseCity, FranklinInstall = DeviceInstallDate, FranklinSerial = SerialNumber, CustomerLastName),
              by = "DRUID") %>%

mutate(SAIDMatch = ifelse(SeeLoadSAID == FranklinSAID, yes = TRUE, no = FALSE)) %>%
mutate(SPIDMatch = ifelse(SeeLoadSPID == FranklinSPID, yes = TRUE, no = FALSE)) %>%
mutate(AccIDMatch = ifelse(SeeLoadAccID == FranklinAccID, yes = TRUE, no = FALSE)) %>%
mutate(SourceMatch = ifelse(SeeloadSource == FranklinSource, yes = TRUE, no = "In Both FE and SL")) %>%
  
select(DRUID, SeeLoadSAID, FranklinSAID, SAIDMatch, SeeLoadSPID, FranklinSPID, SPIDMatch, SeeLoadAccID, FranklinAccID, AccIDMatch, SourceMatch, SeeLoadEnrollStatus, FranklinEnrollStatus, city, 
       PremiseCity, SeeLoadInstall, FranklinInstall, SeeLoadSerial, FranklinSerial, LastName, CustomerLastName) 


# DUPLICATE DRUIDs PRINT OUT
DRUIDDuplicates <- DRUIDMatch %>%
filter(SPIDMatch == "TRUE" & AccIDMatch == "TRUE") %>%
select(DRUID, SeeLoadSAID, FranklinSAID, SeeLoadSPID, FranklinSPID, SeeLoadAccID, FranklinAccID)

DRUIDDuplicates %>%
  write_csv(file = paste0("./Results/", "DuplicateDRUIDs", " ", Sys.Date(), ".csv"))  


# SEPARATES IN ORDER TO SORT OUT THE DRUIDs
DRUIDSeparation<- DRUIDMatch %>%
  filter(SAIDMatch == "FALSE" & SPIDMatch == "FALSE" & AccIDMatch == "FALSE")


# DRUID ONLY IN dataset2 AND NOT dataset1
DRUIDdataset2Notdataset1 <- DRUIDMatch %>%
  filter(!DRUID %in% DRUIDDuplicates$DRUID) %>%
  filter(!DRUID %in% DRUIDSeparation$DRUID) %>%
  select(DRUID, SeeLoadSAID, FranklinSAID, SeeLoadSPID, FranklinSPID, SeeLoadAccID, FranklinAccID)

DRUIDdataset2Notdataset1 %>%
  write_csv(file = paste0("./Results/", "SeeLoadDRUIDs", " ", Sys.Date(), ".csv")) 

       
#DRUID ONLY IN dataset1 AND NOT dataset2
DRUIDdataset2Notdataset1 <- dataset1 %>%
  filter(!DRUID %in% MatchingUniqueID$DRUID) %>%
  filter(!DRUID %in% DRUIDMatch$DRUID) %>%
  mutate(dataSource = "dataset1") %>%
  select(DRUID, FranklinSAID = SAID, FranklinSPID = SPID, FranklinAccID = AccountID, FranklinSource = dataSource) %>%
  left_join(dataset2 %>%
              mutate(dataSource = "dataset2") %>%
              select(DRUID, SeeLoadSAID = SAID, SeeLoadSPID = SPID, SeeLoadAccID = AccountID, SeeloadSource = dataSource),
            by = "DRUID") %>%

mutate(SAIDMatch = ifelse(SeeLoadSAID == FranklinSAID, yes = TRUE, no = FALSE)) %>%
mutate(SPIDMatch = ifelse(FranklinSPID == SeeLoadSPID, yes = TRUE, no = FALSE)) %>%
mutate(AccIDMatch = ifelse(FranklinAccID == SeeLoadAccID, yes = TRUE, no = FALSE)) %>%
mutate(SourceMatch = ifelse(SeeloadSource == FranklinSource, yes = TRUE, no = "In Both FE and SL")) %>%
  
select(DRUID, SeeLoadSAID, FranklinSAID, SeeLoadSPID, FranklinSPID, SeeLoadAccID, FranklinAccID)

DRUIDdataset2Notdataset1 %>%
  write_csv(file = paste0("./Results/", "FranklinDRUIDs", " ", Sys.Date(), ".csv")) 


# SPID Comparison --------------------------------------------------------
SPIDMatch <- dataset2 %>%
  filter(!SPID %in% MatchingUniqueID$SPID)%>%
  filter(!SPID %in% completeMatch$SPID) %>%
  mutate(dataSource = "dataset2") %>%
  mutate(city = str_trim(city)) %>%
  select(SPID, SeeLoadSAID = SAID, SeeLoadDRUID = DRUID, SeeLoadAccID = AccountID, SeeloadSource = dataSource,
         SeeLoadEnrollStatus = EnrollmentStatus, city, SeeLoadInstall = DeviceInstallDate, SeeLoadSerial = SerialNumber, LastName) %>%
  left_join(dataset1 %>%
              mutate(dataSource = "dataset1") %>%
              mutate(PremiseCity = str_trim(PremiseCity)) %>%
              select(SPID, FranklinSAID = SAID, FranklinDRUID = DRUID, FranklinAccID = AccountID, FranklinSource = dataSource,
                     FranklinEnrollStatus = EnrollmentStatus, PremiseCity, FranklinInstall = DeviceInstallDate, FranklinSerial = SerialNumber, CustomerLastName),
            by = "SPID") %>%
 
  mutate(SAIDMatch = ifelse(SeeLoadSAID == FranklinSAID, yes = TRUE, no = FALSE)) %>%
  mutate(DRUIDMatch = ifelse(SeeLoadDRUID == FranklinDRUID, yes = TRUE, no = FALSE)) %>%
  mutate(AccIDMatch = ifelse(SeeLoadAccID == FranklinAccID, yes = TRUE, no = FALSE)) %>%
  mutate(SourceMatch = ifelse(SeeloadSource == FranklinSource, yes = TRUE, no = "In Both dataset1 and dataset2")) %>%
  
  select(SPID, SeeLoadSAID, FranklinSAID, SAIDMatch, SeeLoadDRUID, FranklinDRUID,  DRUIDMatch, SeeLoadAccID, FranklinAccID, AccIDMatch, SourceMatch, SeeLoadEnrollStatus, FranklinEnrollStatus, city, 
         PremiseCity, SeeLoadInstall, FranklinInstall, SeeLoadSerial, FranklinSerial, LastName, CustomerLastName) 

# DUPLICATE SPIDs PRINT OUT
SPIDDuplicates <- SPIDMatch %>%
  filter(SAIDMatch == "TRUE" & DRUIDMatch == "TRUE" & AccIDMatch == "TRUE") %>%
  select(SPID, SeeLoadSAID, FranklinSAID, SeeLoadDRUID, FranklinDRUID, SeeLoadAccID, FranklinAccID)

SPIDDuplicates %>%
  write_csv(file = paste0("./Results/", "DuplicateSPIDs", " ", Sys.Date(), ".csv"))  


# SEPARATES IN ORDER TO SORT OUT THE SPIDs
SPIDSeparation<- SPIDMatch %>%
  filter(SAIDMatch == "FALSE" & DRUIDMatch == "FALSE" & AccIDMatch == "FALSE")


# DRUID ONLY IN dataset2 AND NOT dataset1
SPIDdataset2Notdataset1 <- SPIDMatch %>%
  filter(!SPID %in% SPIDDuplicates$SPID) %>%
  filter(!SPID %in% SPIDSeparation$SPID) %>%
  select(SPID, SeeLoadSAID, FranklinSAID, SeeLoadDRUID, FranklinDRUID, SeeLoadAccID, FranklinAccID)

SPIDSeeLoadNotFranklin %>%
  write_csv(file = paste0("./Results/", "SeeLoadSPIDs", " ", Sys.Date(), ".csv")) 

#SPID is in dataset1 but does not match or exist in dataset2
SPIDdataset2Notdataset1 <- dataset1 %>%
  filter(!SPID %in% MatchingUniqueID$SPID) %>%
  filter(!SPID %in% SPIDMatch$SPID) %>%
  mutate(dataSource = "dataset1") %>%
  select(SPID, FranklinSAID = SAID, FranklinDRUID = DRUID, FranklinAccID = AccountID, FranklinSource = dataSource) %>%
  left_join(dataset2 %>%
              mutate(dataSource = "dataset2") %>%
              select(SPID, SeeLoadSAID = SAID, SeeLoadDRUID = DRUID, SeeLoadAccID = AccountID, SeeloadSource = dataSource),
            by = "SPID") %>%
  
select(SPID, SeeLoadSAID, FranklinSAID, SeeLoadDRUID, FranklinDRUID, SeeLoadAccID, FranklinAccID)

SPIDdataset2Notdataset1 %>%
  write_csv(file = paste0("./Results/", "FranklinSPIDs", " ", Sys.Date(), ".csv")) 



# ACCOUNT ID Comparison--------------------------------------------------------
AccIDMatch <- dataset2 %>%
  filter(!AccountID %in% MatchingUniqueID$AccountID)%>%
  filter(!AccountID %in% completeMatch$AccountID) %>%
  mutate(dataSource = "dataset2") %>%
  mutate(city = str_trim(city)) %>%
  select(SeeLoadSAID = SAID, SeeLoadSPID = SPID, SeeLoadDRUID = DRUID, AccountID, SeeloadSource = dataSource,
         SeeLoadEnrollStatus = EnrollmentStatus, city, SeeLoadInstall = DeviceInstallDate, SeeLoadSerial = SerialNumber, LastName) %>%
  left_join(dataset1 %>%
              mutate(dataSource = "dataset1") %>%
              mutate(PremiseCity = str_trim(PremiseCity)) %>%
              select(FranklinSAID = SAID, FranklinSPID = SPID, FranklinDRUID = DRUID, AccountID, FranklinSource = dataSource,
                     FranklinEnrollStatus = EnrollmentStatus, PremiseCity, FranklinInstall = DeviceInstallDate, FranklinSerial = SerialNumber, CustomerLastName),
            by = "AccountID") %>%
  
  mutate(SAIDMatch = ifelse(SeeLoadSAID == FranklinSAID, yes = TRUE, no = FALSE)) %>%
  mutate(DRUIDMatch = ifelse(SeeLoadDRUID == FranklinDRUID, yes = TRUE, no = FALSE)) %>%
  mutate(SPIDMatch = ifelse(SeeLoadSPID == FranklinSPID, yes = TRUE, no = FALSE)) %>%
  mutate(SourceMatch = ifelse(SeeloadSource == FranklinSource, yes = TRUE, no = "In Both FE and SL")) %>%
  
  select(SeeLoadSAID, FranklinSAID, SAIDMatch, SeeLoadSPID, FranklinSPID, SPIDMatch, SeeLoadDRUID, FranklinDRUID, DRUIDMatch, AccountID, SourceMatch, SeeLoadEnrollStatus, FranklinEnrollStatus, city, 
         PremiseCity, SeeLoadInstall, FranklinInstall, SeeLoadSerial, FranklinSerial, LastName, CustomerLastName) 

# DUPLICATE SPIDs PRINT OUT
AccIDDuplicates <- AccIDMatch %>%
  filter(SAIDMatch == "TRUE" & DRUIDMatch == "TRUE" & SPIDMatch == "TRUE") %>%
  select(AccountID, SeeLoadSAID, FranklinSAID, SeeLoadDRUID, FranklinDRUID, SeeLoadSPID, FranklinSPID)

AccIDDuplicates %>%
  write_csv(file = paste0("./Results/", "DuplicateAccIDs", " ", Sys.Date(), ".csv"))  


# SEPARATES IN ORDER TO SORT OUT THE SPIDs
AccIDSeparation<- AccIDMatch %>%
  filter(SAIDMatch == "FALSE" & DRUIDMatch == "FALSE" & SPIDMatch == "FALSE")


# DRUID ONLY IN dataset2 AND NOT dataset1
AccIDSeeLoadNotFranklin <- AccIDMatch %>%
  filter(!AccountID %in% AccIDDuplicates$AccountID) %>%
  filter(!AccountID %in% AccIDSeparation$AccountID) %>%
  select(AccountID, SeeLoadSAID, FranklinSAID, SeeLoadDRUID, FranklinDRUID, SeeLoadSPID, FranklinSPID)

AccIDSeeLoadNotFranklin %>%
  write_csv(file = paste0("./Results/", "SeeLoadAccIDs", " ", Sys.Date(), ".csv")) 

#SPID is in dataset1 but does not match or exist in dataset2
AccIDdataset1Notdataset2 <- dataset1 %>%
  filter(!AccountID %in% MatchingUniqueID$AccountID) %>%
  filter(!AccountID %in% AccIDMatch$AccountID) %>%
  mutate(dataSource = "dataset1") %>%
  select(FranklinSAID = SAID, FranklinSPID = SPID, FranklinDRUID = DRUID, AccountID, FranklinSource = dataSource) %>%
  left_join(dataset2 %>%
              mutate(dataSource = "dataset2") %>%
              select(SeeLoadSAID = SAID, SeeLoadSPID = SPID, SeeLoadDRUID = DRUID, AccountID, SeeloadSource = dataSource),
            by = "AccountID") %>%
  
  select(AccountID, SeeLoadSAID, FranklinSAID, SeeLoadDRUID, FranklinDRUID, SeeLoadSPID, FranklinSPID)

AccIDdataset1Notdataset2 %>%
  write_csv(file = paste0("./Results/", "FranklinAccIDs", " ", Sys.Date(), ".csv")) 


# dataset2 Address Clean Up File --------------------------------------------------------
# Use case - if state is not CA, then address input is wrong, print output to file to fix
dataset2AddressCleanUp <- dataset2 %>%
  filter(state != "CA")
# Saves dataset2 address clean up file as csv into results folder
dataset2AddressCleanUp %>%
  write_csv(file = paste0("./Results/", "SeeloadAddressCleanUp", " ", Sys.Date(), ".csv"))










