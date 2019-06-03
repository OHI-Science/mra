
# README


Habitats:

1. Mangrove
Description: Data on the extent of mangrove in Moorea came from Jost (2015), being the most recent available extent data in 2015.  To calculate condition of the habitat, percentage of coastline cover was used, as there were more available measurements: 1987, 1993, 2001, 2003, 2009 and 2014. Temporal gap filling was applied using polynomial regression. Current condition is based on 2014. A temporal reference point is considered, based on 1987. Trend is calculated as the rest, 5 years slope.

Habitat condition of mangrove (*hab_mangrove_health*): Current condition of mangrove habitat relative to historical condition

Habitat condition trend of mangrove: 5 years
Habitat mangrove extent

2.Coral

Coral condition
Where is used: coastal protection and habitat goal.
Description: coral condition was estimated dividing the current condition by the reference condition.This was based on: 1) the proportion of live coral cover from 2004 till 2018, which is derived from SO CORAIL dataset (ref).  117 stations were surveyed using coral transect method, data from same site, habitat and year were averaged and also different site and habitat to calculate a per region and per year average. Within a same transect, the proportion of live coral from different coral genus was summed.2) The reference condition was the proportion of live coral in 1979(Bouchon, 1985). The percent cover was evaluated in 4 zones at Tiahura Reef- reef flat, reef crest, reef slope and reef base and included Acropora, Pocillopora, Montipora, Porites and others.The four zones were pooled to obtain an average live coral cover (%). It was converted to proportion.

Trend:: 14 years

Bouchon C., 1985- Quantitative study of scleractinian coral communities of Tiahura reef (Moorea island, French Polynesia). Proc. 5th Internat. Coral Reefs Congres Tahiti 6 : 279-284.

For example, you could include: 

- data source
- data url or website
- date accessed, contact information
- processing plan

It is best to script (for example, in the R programming language) as much as you can so that it is transparent and reproducible. 

The [OHI Manual R tutorials for OHI](http://ohi-science.org/manual/#appendix-5-r-tutorials-for-ohi) has instruction and resources about how to work with data in R. 
