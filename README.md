# How Mobile Money Interoperability in Ghana affects fees Mobile Money Users Pay: Telco Price Analysis

I took a deep dive into Telco Mobile Money prices in Ghana to determine what interoperability means for fees mobile money users pay, specifically low-income people.

For reproducibility, I outline how Telco mobile money fees data was collected, tools used in analysis and the insights below.

## Data Collection Details

Mobile Money transaction fees data was sourced from [AirtelTigo Ghana](http://airteltigo.com.gh), [MTN Ghana](http://mtn.com.gh/) and [Vodafone Ghana](https://vodafone.com.gh/) and can be found below. 
A `.csv` file of the table below can be found [here](https://github.com/DavidQuartey/Mobile-Money-Price-Value-For-Money/blob/master/Sources/Data.csv).

| Price Ranges                                                                      | MTN            |              | Vodafone       |              | AirtelTigo     |              |
|-----------------------------------------------------------------------------------|----------------|--------------|----------------|--------------|----------------|--------------|
|                                                                                   | Across Network | Same Network | Across Network | Same Network | Across Network | Same Network |
| GHS 1 - GHS 50                                                                    | 0.75           | 0.5          | 0.5            | 0.5          | 0.75           | 0.5          |
| GHS 51 - GHS 75                                                                   | 1.50%          | 1%           | 1.50%          | 0.5          | 1.50%          | 1            |
| GHS 76 - GHS 100*                                                                 | 1.50%          | 1%           | 1.50%          | 0.75         | 1.50%          | 1            |
| GHS 101 - GHS 250                                                                 | 1.50%          | 1%           | 1.50%          | 1            | 1.50%          | 1.5          |
| GHS 251 - GHS 500                                                                 | 1.50%          | 1%           | 1.50%          | 2            | 1.50%          | 2            |
| GHS 501 - GHS 1000                                                                | 1.50%          | 1%           | 1.50%          | 3            | 1.50%          | 2.5          |
| GHS 1 000 - GHS 5 000                                                           |                |              | 15             |              | 15             |              |
|                                                                                   |                |              |                |              |                |              |
| * **Range included to accommodate Vodafone's unique 50 -75/76 - 100 fee point split** |                |              |                |              |                |              |

For each Network, I collected fees data on same network (on-net) and cross network (off-net) mobile money transfers.                 Data was collected from GHS 1 - GHS 1000, however for networks which had price points beyond GHS 1 000 available, it was collected.

## Tools
* [R Programming Language](https://www.r-project.org/about.html)
* [ggplot2](https://ggplot2.tidyverse.org/)
* [magrittr](https://magrittr.tidyverse.org/)
* [purrr](https://purrr.tidyverse.org/)
* [dplyr](https://dplyr.tidyverse.org/)


## Insights
![alt text](https://github.com/DavidQuartey/Mobile-Money-Price-Value-For-Money/blob/master/Visualizations/Viz-mobile%20money%20fee%20structure-1.png)

I found that, users who send high amounts have ralatively good options (further expanded by interoperability), but if it's less it doesn't matter what network they use.

If Government of Ghana has plans to tax mobile money transactions as being reported, Telco's could consider looking at higher amount ranges (500 - 1000, 1000+) since these amounts generally pay lower fees in percentage terms, although it feels like the platform is still in its infancy.

Find the full analysis [here](https://github.com/DavidQuartey/Mobile-Money-Price-Value-For-Money/blob/master/value_for_money.md)
