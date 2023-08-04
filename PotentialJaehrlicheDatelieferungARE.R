# PotentialJaehrlicheDatelieferungARE

# importiere Bibliotheken ----
library(data.table)
library(dbplyr)
library(dplyr)
library(tidyr)
library(sf)

# setze DB-Verbindung ----
con <- DBI::dbConnect(
  odbc::odbc(),
  "STATOPPR",
  UID = Sys.getenv("UID"),
  PWD = Sys.getenv("PWD"))

# definiere Format ----
options(scipen = 1000000)
options(digits = 6)

# definiere Stichtage ----
gde_stichtag = '01.01.2023'
geb_stichtag = '31.12.2022'
einw_stichtag = '31.12.2022'
besch_stichtag = '31.12.2020'
oereb_stichtag = "2023_01_18"
gfb_jahr = '2022'

# import ----

## gde ----
gde <- tbl(con, "GEMEINDE") %>%
  # lese nur Reihen mit STICHTAG = Stichtag ein.
  filter(to_date(!!gde_stichtag,'dd.mm.yyyy') >= VON & to_date(!!gde_stichtag,'dd.mm.yyyy') <= BIS) %>%
  select(GDE_BFSNR, HANDLRAUM_NAME) %>%
  collect() %>%
  arrange(GDE_BFSNR)


## gemeindefus ----
gemeindefus <- tbl(con, "GEMEINDEFUS") %>%
  collect()

## geb ----
geb <- tbl(con, in_schema("GEBAEUDE", "GWR_GEB")) %>%
  # lese nur Reihen mit STICHTAG = Stichtag ein.
  filter(STICHTAG == to_date(geb_stichtag,'dd.mm.yyyy')) %>%
  select(EGID, GKODE, GKODN, GGDENR) %>%
  collect()

## einw ----
einw <- tbl(con, in_schema("EINWOHNER","EINW_BASIS")) %>%
  #lese nur Reihen mit STICHTAG = Stichtag ein.
  filter(STICHTAG == to_date(einw_stichtag,'dd.mm.yyyy')) %>%
  filter(
    # per Stichtg nicht tot und nicht weggezogen
    AKTIV == 1 &
      # EGID vorhanden
      !is.na(DW_ADDR_EGID) & DW_ADDR_EGID != 999999999 &
      # EWID vorhanden
      !is.na(DW_ADDR_EWID) & DW_ADDR_EGID != 999
  ) %>%
  #group_by(DW_ADDR_EGID, DW_ADDR_EWID, STICHTAG) %>%
  #summarize(einw_anz = n()) %>%
  ungroup() %>%
  collect() %>%
  {. ->>  einw_temp1} %>%
  left_join(geb, by = c("DW_ADDR_EGID" = "EGID")) %>%
  {. ->>  einw_temp2} %>%
  filter(!is.na(GKODE) | !is.na(GKODN)) %>%
  group_by(GKODE, GKODN) %>%
  summarize(einw_anz = n()) %>%
  ungroup() %>%
  st_as_sf(coords=c("GKODE", "GKODN"), crs = 2056)

# check
## should be == TRUE
nrow(einw_temp1) == nrow(einw_temp2)
## Anteil einw mit geok
sum(einw$einw_anz)/nrow(einw_temp2)*100

rm(geb)

## besch ----
besch <- tbl(con, in_schema("OEC", "STATENT_LOC_MV")) %>%
  #lese nur Reihen mit STICHTAG = Stichtag ein.
  filter(STICHTAG == to_date(besch_stichtag,'dd.mm.yyyy')) %>%
  # lese nur Reihen im Kanton Zürich ein.
  filter(CANTON_CD == '1') %>%
  filter(!is.na(GKODE) | !is.na(GKODN)) %>%
  select(EMPTOT, MUNICIPALITY_CD, GKODE, GKODN) %>%
  collect() %>%
  {. ->>  besch_temp1} %>%
  group_by(GKODE, GKODN) %>%
  summarize(besch_anz = sum(EMPTOT)) %>%
  ungroup() %>%
  st_as_sf(coords=c("GKODE", "GKODN"), crs = 2056)

# check
## Anteil besch mit geok
sum(besch$besch_anz) == sum(besch_temp1$EMPTOT)

rm(besch_temp1)

## oereb ----
oereb <- st_read(paste0("~/file-server/file-server/08_DS/03_GIS/Geodaten/NP_GN_ZONENFLAECHE_F_KTZH/NP_GN_ZONENFLAECHE_F_KTZH_", oereb_stichtag, ".shp"), stringsAsFactors = FALSE, crs=2056) %>%
  select(TYP_ZH_COD, TYP_BFSNR) %>%
  {. ->>  oereb_temp1} %>%
  left_join(gde, by = c("TYP_BFSNR" = "GDE_BFSNR"))

# check
## should be == TRUE
nrow(oereb) == nrow(oereb_temp1)
oereb_check_hr <- oereb %>%
  st_drop_geometry() %>%
  distinct(HANDLRAUM_NAME, TYP_BFSNR)
length(oereb_check_hr) == length(gde)

## gfb ----
gfb_path <- paste0("~/file-server/file-server/03_AS/02_Datengrundlagen/GFL/PotentialJaehrlicheDatelieferungARE/", gfb_jahr, "/")
gfb <- data.table::fread(paste0(gfb_path, "Input/GFB_InputPotential_", gfb_jahr, ".csv")) %>%
  rename(BFSNR = 1, TYP_GDE_CODE = 2, XXX = 3, GFB = 4) %>%
  select(BFSNR, TYP_GDE_CODE, GFB) %>%
  mutate(TYP_ZH_COD = substr(TYP_GDE_CODE, start = 1, stop = 5)) %>%
  select(-TYP_GDE_CODE) %>%
  {. ->>  gfb_temp1} %>%
  left_join(gemeindefus, by=c('BFSNR' = 'GEMEINDEFUS_ALT'), suffix = c("", ".y")) %>%
  {. ->>  gfb_temp2} %>%
  select_at(vars(-ends_with(".y"))) %>%
  # korrigiere  Spalte 'BFSNR' für Fusionen
  mutate(BFSNR = case_when(
    !is.na(GEMEINDEFUS_NEU) ~ GEMEINDEFUS_NEU,
    TRUE ~ BFSNR)) %>%
  left_join(gde, by = c("BFSNR" = "GDE_BFSNR"))

#check
## should be == TRUE
nrow(gfb) == nrow(gfb_temp1)
# should NOT be TRUE # xxx
table(is.na(gfb$HANDLRAUM_NAME) , useNA = c("always"))

# aggregate by TYP_ZH_COD ----

## einw_oereb ----
einw_oereb <- einw %>%
  st_join(oereb) %>%
  st_drop_geometry() %>%
  group_by(TYP_ZH_COD, HANDLRAUM_NAME, TYP_BFSNR) %>%
  summarize(einw_anz = sum(einw_anz, na.rm = TRUE)) %>%
  ungroup()

# check
## should be == TRUE
sum(einw_oereb$einw_anz) == sum(einw$einw_anz)
rm(einw)

## besch_oereb ----
besch_oereb <- besch %>%
  st_join(oereb) %>%
  st_drop_geometry() %>%
  group_by(TYP_ZH_COD, HANDLRAUM_NAME, TYP_BFSNR) %>%
  summarize(besch_anz = sum(besch_anz, na.rm = TRUE)) %>%
  ungroup()

# check
## should be == TRUE
sum(besch_oereb$besch_anz) == sum(besch$besch_anz)
rm(besch)

## gfb_oereb ----
gfb_oereb <- gfb  %>%
  group_by(TYP_ZH_COD, HANDLRAUM_NAME, BFSNR) %>%
  summarize(gfb_m2 = sum(GFB, na.rm = TRUE)) %>%
  ungroup()

# check
## should be == TRUE
sum(gfb_oereb$gfb_m2) == sum(gfb$GFB)
rm(gfb)

# output: prepare ----
## prep_oereb ----
prep_oereb <- oereb %>%
  st_drop_geometry() %>%
  distinct(TYP_ZH_COD, HANDLRAUM_NAME, TYP_BFSNR)

# check
prop.table(table(prep_oereb$HANDLRAUM_NAME, useNA = c("always")))*100
table(is.na(prep_oereb$HANDLRAUM_NAME) , useNA = c("always"))

## gfb_oereb_antijoin ----
# Nicht alle Attributkombinationen von gfb::'TYP_ZH_COD', 'BFSNR', 'HANDLRAUM_NAME' (gfb_distinct; siehe unten) kommen im oereb (prep_oereb) vor
# Für die finale Auswertung (TYP_ZH_COD_df) ist es aber sinnvoll, dass alle Attributkombinationen berücksichtigt werden.
gfb_distinct <- gfb_oereb %>% select(-gfb_m2) %>% distinct()
## Attributkombinationen die im oereb nicht vorkommen
gfb_oereb_antijoin <- anti_join(gfb_distinct, prep_oereb, by = c("TYP_ZH_COD", "HANDLRAUM_NAME", "BFSNR" = "TYP_BFSNR")) %>%
  rename(TYP_BFSNR = BFSNR) %>%
  filter()

# output final: TYP_ZH_COD_df ----
# join df to prep_oereb
TYP_ZH_COD_df <- prep_oereb %>%
  bind_rows(gfb_oereb_antijoin) %>%
  {. ->>  TYP_ZH_COD_temp1} %>%
  #add_row(TYP_ZH_COD = "C1304", HANDLRAUM_NAME = "Landschaft unter Druck", TYP_BFSNR = 83) %>% # fehlt, check sum(gfb_m2) unten stimmt sonst nicht
  left_join(einw_oereb, by = c("TYP_ZH_COD", "HANDLRAUM_NAME","TYP_BFSNR")) %>%
  {. ->>  TYP_ZH_COD_temp2} %>%
  left_join(besch_oereb, by = c("TYP_ZH_COD", "HANDLRAUM_NAME", "TYP_BFSNR")) %>%
  {. ->>  TYP_ZH_COD_temp3} %>%
  left_join(gfb_oereb, by = c("TYP_ZH_COD", "HANDLRAUM_NAME", "TYP_BFSNR" = "BFSNR")) %>%
  {. ->>  TYP_ZH_COD_temp4} %>%
  mutate(TYP_ZH_COD_aggr = case_when(
    TYP_ZH_COD %in% c("C1101", "C1102", "C1103", "C1104", "C1105") ~ "Wohnzone",
    TYP_ZH_COD %in% c("C1201", "C1202", "C1203") ~ "Arbeitszone",
    TYP_ZH_COD %in% c("C1301", "C1302", "C1303", "C1304", "C1305", "C1306", "C1307") ~ "Mischzone",
    TRUE ~ "Andere"
  )) %>%
  mutate(HANDLRAUM_NAME_sel = case_when(
    HANDLRAUM_NAME %in% c("Naturlandschaft") ~ "Natur- und Kulturlandschaft",
    HANDLRAUM_NAME %in% c("Kulturlandschaft") ~ "Natur- und Kulturlandschaft",
    TRUE   ~ HANDLRAUM_NAME
  )) %>%
  ungroup() %>%
  group_by(TYP_ZH_COD_aggr, HANDLRAUM_NAME_sel) %>%
  summarize(
    einw_anz = sum(einw_anz, na.rm = TRUE),
    besch_anz = sum(besch_anz, na.rm = TRUE),
    gfb_m2 = sum(gfb_m2, na.rm = TRUE),
    einw_gfb_m2_prov = ifelse(einw_anz == 0, 0, gfb_m2/einw_anz),
    besch_gfb_m2_prov = ifelse(besch_anz == 0, 0, gfb_m2/besch_anz),
    einw_ant = einw_anz/(einw_anz+besch_anz)*100,
    besch_ant = besch_anz/(einw_anz+besch_anz)*100,
  ) %>%
  mutate(
    gfb_m2_final = case_when(
      TYP_ZH_COD_aggr %in% c("Arbeitszone") ~ besch_gfb_m2_prov*besch_ant/100,
      TYP_ZH_COD_aggr %in% c("Wohnzone") ~ einw_gfb_m2_prov*einw_ant/100
    )) %>%
  {. ->>  TYP_ZH_COD_df_temp} %>%
  filter(TYP_ZH_COD_aggr != "Andere")

# check
## should be == TRUE
nrow(TYP_ZH_COD_temp1) == nrow(TYP_ZH_COD_temp4)
sum(TYP_ZH_COD_df_temp$einw_anz) == sum(einw_oereb$einw_anz)
sum(TYP_ZH_COD_df_temp$besch_anz) == sum(besch_oereb$besch_anz)
sum(TYP_ZH_COD_df_temp$gfb_m2) == sum(gfb_oereb$gfb_m2)

# export ----

## xlsx ----
statR::quickXLSX(data = TYP_ZH_COD_df,
                 title = paste0("GFB_AuswertungPotential_", gfb_jahr),
                 file = paste0(gfb_path, "GFB_AuswertungPotential_", gfb_jahr), # '.xlsx' wird automatisch hinzugefügt
                 #source = "Source: Henderson and Velleman (1981). Building multiple regression models interactively. Biometrics, 37, 391–411.",
                 #metadata = c("The data was extracted from the 1974 Motor Trend US magazine and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models)."),
                 contactdetails = "statzh",
                 logo = "statzh",
                 grouplines = FALSE,
                 author = "KK")

##  csv ----
write.table(TYP_ZH_COD_df, paste0(gfb_path, "GFB_AuswertungPotential_", gfb_jahr, ".csv"), row.names=FALSE, sep = ",", fileEncoding ="UTF-8")

# weiteres ----
# 7% der Einwohner leben in TYP_ZH_COD_aggr == "Andere"
sum(TYP_ZH_COD_df_temp %>% filter(TYP_ZH_COD_aggr == "Andere") %>% pull(einw_anz))/
  sum(TYP_ZH_COD_df_temp$einw_anz)*100

# 32% der Beschäftigten arbeiten in TYP_ZH_COD_aggr == "Andere"
sum(TYP_ZH_COD_df_temp %>% filter(TYP_ZH_COD_aggr == "Andere") %>% pull(besch_anz))/
  sum(TYP_ZH_COD_df_temp$besch_anz)*100

