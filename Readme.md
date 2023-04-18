# README.md

Wir liefern dem ARE jährlich Daten zu den Potentialen.

## PotentialJaehrlicheDatelieferungARE.R
Jährliche Datenlieferung an ARE. 

## Metadaten

| Name| Systematik                                                                                    
| ---- | ---- | 
| TYP_ZH_COD_aggr | Wohnzone/ Arbeitszone / Mischzone. Klassiert wurde wie folgt: <br> TYP_ZH_COD %in% c("C1101", "C1102", "C1103", "C1104", "C1105") ~ "Wohnzone", <br/> TYP_ZH_COD %in% c("C1201", "C1202", "C1203") ~ "Arbeitszone", <br/> TYP_ZH_COD %in% c("C1301", "C1302", "C1303", "C1304", "C1305", "C1306", "C1307") ~ "Mischzone" |
| HANDLRAUM_NAME | Name des Handlungsraumes |
| einw_anz | Anzahl Einwohner (wirtschafliche Bevölkerung): Die wirtschaftliche Bevölkerung umfasst diejenigen Personen, die ihren Haupt- oder Nebenwohnsitz in der Meldegemeinde gemeldet haben|
| besch_anz | Anzahl Beschäftigte |
| gfb_m2 | Geschossflächenbestand in m2 |
| einw_gfb_m2_prov | Geschossflächenbestand  pro Einwohner in m2 (mit Anteilen Beschäftigte)  |
| besch_gfb_m2_prov | Geschossflächenbestand  pro Beschäftigte in m2 (mit Anteilen Einwohner)|
| einw_ant | Anteil Einwohner / (Anzahl Einwohner + Anzahl Beschäftigte) |
| besch_ant | Anteil Anzahl Beschäftigte / (Anzahl Einwohner + Anzahl Beschäftigte) |
| einw_gfb_m2 | Geschossflächenbestand  pro Einwohner in m2 |
| besch_gfb_m2 | Geschossflächenbestand  pro Beschäftigte in m2 |

