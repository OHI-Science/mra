# README

This folder contain information about data processing, in which R scripts, metadata and data are available. 

# SENSE OF PLACE GOAL
## Iconic Species subgoal

A full description about Iconic Species subgoal can be found [here](http://ohi-science.org/goals/#sense-of-place).

**Processing plan**

French Polynesia iconic species are subtracted from the original list used in the [2018 global analysis](https://github.com/OHI-Science/ohiprep_v2018/blob/gh-pages/globalprep/ico/v2018/int/ico_spp_cat.csv), which includes flagship and priority species defined by the World Wildlife Fund for Nature (*ico_spp_rgn_cat*).

This dataset is completed with local information of iconic spcies in French Polynesia, specifically Moorea (*ico_spp_mra_cat*). 

In French Polynesia, sharks and cetacean are classified within B category, which includes species known as *patrimonial*, having a cultural value. Check [*Le Code de l'environnement de la Polynésie française*](http://www.2dattitude.org/ressources/k2d/pdf/1/1D/1D05/1D05-01/1D05-01-01.pdf) and *Profil d'écosystèmes de la Polynésie Française – Région Pacifique, 2016* (see reference below). Therefore, sharks and cetacean encountered from Moorea are checked in [Moorea BIOCODE Species Database](http://biocode.berkeley.edu/cgi/biocode_species_query_form) (*ico_spp_biocode_cat*). 

The last step is to check their IUCN status and the historical changes in the [IUCN Red List of Threatened Species](https://www.iucnredlist.org/). 


# BIODIVERSITY GOAL
## Species subgoal

A full description about Iconic Species subgoal can be found [here](http://ohi-science.org/goals/#biodiversity).

**Processing plan**

The dataset about French Polynesia (FP) speceis *rgn_spp_gl* are joined with the marine species dataset from API *spp_marine_from_api_2018-1*, thus marine species from FP are obtained *rgn_spp_mar*. 

Then, for time-series data, *rgn_spp_mar* was joint with *iucn_risk_timeseries*, to get the change in IUCN status of FP species *rgn_spp_time*.

*rgn_spp_gl*, *spp_marine_from_api_2018-1* and *iucn_risk_timeseries* are original datasets from the [2018 global analysis](https://github.com/OHI-Science/ohiprep_v2018/tree/gh-pages/globalprep/spp/v2018/_data).
 

**Note**
To keep in mind: the spatial scale of our study is "lagoon of Moorea", thus we may be overestimating the biodiversity. The question is if should we compare with Moorea Biocode species database? One option is to inner-joining FP marine species with Moorea datasets and thus, we would obtain Moorea species hat have been assessed by IUCN.

It is still ongoing...


**Some references**

Ocean Health Index. 2018. ohiprep version 2018: Preparation of data for global scenarios of the Ocean Health Index, [04/02/2019]. National Center for Ecological Analysis and Synthesis, University of California, Santa Barbara. Available at: https://github.com/OHI-Science/ohiprep/releases.

Profil d'écosystèmes de la Polynésie Française – Région Pacifique. 2016. Union
européennes Régions Ultra-pèriphériques et Pays et Territoires d’Outre-mer, Eleonora Avagliano &
Flora Artzner ; BEST, contract de service 07.0307.2013/666363/SER/B2, Commission
Européenne, 2016, 224 p + annexes.


The [OHI Manual R tutorials for OHI](http://ohi-science.org/manual/#appendix-5-r-tutorials-for-ohi) has instruction and resources about how to work with data in R. 
