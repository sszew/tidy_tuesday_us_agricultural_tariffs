# Tidy Tuesday: US Agricultural Tariffs.

**The file tariff_rates_comparison.R contains the R script to generate the NAFTA/USMCA tariff rate plots referred to in the LinkedIn post (text of post printed below): https://www.linkedin.com/feed/update/urn:li:ugcPost:7457677916102115328/**

# The R script was created with the assistance of Claude Sonnet 4.6 through Posit Assistant in RStudio.

# NOTE: The data preparation steps were created using similar code to the "Code for the plot"
# section in the Tidy Tuesday GitHub page for April 28, 2026: https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-04-28

----------

When working with data, “zero” doesn’t always equal “zero.” 

Sound strange? I’ll explain. 

For this #TidyTuesday, we are looking at U.S. tariff data on agricultural products under the NAFTA and USMCA trade agreements from 1997-2024.

These agreements aimed, in part, to eliminate duties between the U.S., Canada, and Mexico. And these plots show exactly that: NAFTA tariff rates went to zero for agricultural products by the early 2000s, continuing through the start of USMCA. 

But there is a problem. The plots also show that the tariff rates were zero before peaking in 1997. If NAFTA was meant to eliminate tariffs, why would tariffs start at zero and then increase? 

The most likely explanation is that the pre-1997 “zeros” reflect missing or incomplete data, not true tariff levels. The data source itself is labelled as beginning in the year 1997, suggesting that the pre-1997 data may be unreliable. In reality, agricultural tariffs were not uniformly zero before 1997.

In other words, not all zeros are equal. The pre-1997 zeros are likely artifacts of data collection, while the post-1997 zeros represent true zero tariffs. 

Examples like this can reminder us that data science is only useful if it can connect data to real world events. And sometimes, this means that we need to question the data itself. 

----------


<img width="800" height="800" alt="mexico_tariffs" src="https://github.com/user-attachments/assets/58e6c5cc-9004-4a27-9d5b-95bb94f40ae9" />

<img width="800" height="800" alt="canada_tariffs" src="https://github.com/user-attachments/assets/336a2d14-7c87-43af-90d6-90ef90e08e98" />

----------

For more information on NAFTA and USMCA:
- https://www.cbp.gov/trade/nafta/guide-customs-procedures/description-nafta
- https://www.investopedia.com/terms/n/nafta.asp
- https://ustr.gov/trade-agreements/free-trade-agreements/united-states-mexico-canada-agreement
- https://www.investopedia.com/usmca-4582387
