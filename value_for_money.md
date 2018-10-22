# Mobile Money Value for Money
David Quartey  
September 28, 2018  



# How Mobile Money Interoperability in Ghana affects fees Mobile Money Users Pay: Telco Price Analysis

On 10^th^ May 2018, Mobile Money Interoperability was officially [launched](https://www.myjoyonline.com/business/2018/May-10th/bawumia-launches-mobile-money-payment-interoperability-system.php) in Ghana. This means that you can now transfer mobile money across networks at a fee.

I wrote part of this piece a few days after the launch because interoperability brought up some interesting questions for me. Since then, significant things have happened within the space which meant I've had to re-analyse some portions. About [3000](https://citinewsroom.com/2018/05/15/mobile-money-interoperability-scored-3000-transactions-on-first-day-ghipss/) transactions were made on the first day. By end of August it had reached [800, 000 transactions](https://www.modernghana.com/news/882509/mobile-money-interoperability-hits-800000-transactions-re.html). Most importantly though, since then [AirtelTigo merged](http://www.airteltigo.com.gh/airteltigo-outlines-plans-for-mobile-financial-service-sector/) their mobile money platform.

Some interesting questions this brought up for me at the time of the launch include, say a mobile money user wants to make a transaction, after factoring in cash in fees (if any), transfer fees, and the cash out fees, what is the final amount withdrawn? Or put another way, what 2 combination of networks allows the receiver to cash out at least cost in terms of fees? 

Given that mobile money users can switch to any network since the market is not fragmented anymore, how competitive are Interoperable transaction options to same network transactions? 

What does interoperability mean for low-income people who typically send small amounts?

I find out this and more in this article.


<img src="Visualizations/Viz-10 cedis transfer-1.png" style="display: block; margin: auto;" />

Let's start off with this chart.

To read the chart above, Vodafone Cash to MTN Momo, for example, combines the transfer fee of making a transfer from Vodafone Cash to MTN Momo and the cash out fee of MTN Momo at a particular amount. This gives a sense of how much it costs for that particular network combination. 

This is then extended to all possible network combinations to compare amounts to see what it means for users in fees terms.

At GHc10 for instance, interoperability has provided a comparatively cheap alternative (Vodafone Cash to either AirtelTigo Money or MTN Momo) of sending such a low amount at GHC1 considering the fact that previously, exactly the same amount through the voucher system would have likely cost more.

Either way, one thing that strikes me is how fees alternate. Either you pay GHc1 or GHc1.25.

Is it the same at larger amounts?

<img src="Visualizations/Viz-1000 cedis transfer-1.png" style="display: block; margin: auto;" />

Suddenly, things look a little... different. An incredible 0.5% of GHc 1000 is paid in fees when using AirtelTigo to AirtelTigo (seems like Tigo's Cash rates are being used after the Airtel / Tigo merger for same network transfers).

Just so we see how over a period these fees divergence, lets looks at this scenario: Imagine a mobile money user has to transfer GHC1000 to a friend ones every month for the next 12months. Imagine again whatever combination she uses is consistent across the 12months, how much different will the total amount of fees be by the 12th month?
<img src="Visualizations/Viz-cummulative 1000 cedis transfer-1.png" style="display: block; margin: auto;" />

It's incredible, isn't it? 

She'd have paid between GHc60 to GHc300 in fees after sending GHc12, 000 to her friend over the 12 months period using mobile money on her phone. What's interesting is the variety of options available at a higher amount. Indeed I found that, as the amount transfered increased, so did the options in terms of fees.

Previously, no token or voucher transfer would have been more cost-effective than a same network transfer. This shows that, this is not true anymore.

Even users of the current market leader (MTN) are now better-off interoperating to AirtelTigo than on same network transfer basis for larger amounts.

However, based purely on other possible network combinations a mobile money user may consider, interoperability has also provided relatively costly alternatives too. As high as GHc25 (2.5% paid in fees in percentage terms). 

Still, it's likely that the pre-interoperability rate for say MTN Momo to an unregistered user would've been 5% (GHc50), compared to the interoperable rate averaging at about 1.9% (GHc19) with the added benefit and convenience of direct transfer into a cross-network wallet.

### Are Mobile Money fees retrogressive in structure?

[Innovations for Poverty Action](https://www.poverty-action.org) (IPA) recently came out with a [study](http://www.cgap.org/blog/how-do-mobile-money-fee-structures-impact-poor) which highlighted the fees paid (cash in and cash out) on 21 mobile money service across 7 countries (Kenya, Uganda, Tanzania, Pakistan, Nigeria, Bangladesh and India). Even though Ghana was not included in their work, they concluded that most had prices which were regressive in **structure** because "the larger a consumers transaction, the less they pay in **percentage terms**" (emphasis mine).

I was curious to know how this translates to Ghana's Telco mobile money prices in terms of structure. 
<img src="Visualizations/Viz-average prices-1.png" style="display: block; margin: auto;" />

Considering each network combination and their transfer + cash out fees, mobile money products in Ghana are regressive in structure too. This shows with the average fees paid downward trend as the amount increases. This means that at lower amounts, users are paying more in fees in percentage terms compared to higher amounts. This is important because, low-income people, the unbanked, and people new to the platform will most likely send low amounts and so, they will most likely be paying more in percentage terms compared to people who send higher amounts.

Looking at how Telco's in Ghana structure their mobile money prices, the higher fees could partly be as a result of how amounts within the 1 - 50 range are charged flat rates. Currently, all 3 Telco's in Ghana who provide this service charge flat rates for the GHc1-GHc50 band. As [noted](http://www.cgap.org/blog/how-do-mobile-money-fee-structures-impact-poor), this way of pricing has its benefits because a flat rate probably takes consideration for people with low numeracy skills to understand how much they are charged. However, it ends up being a lot in percentage terms at such low amounts.

### What does this all mean?
<img src="Visualizations/Viz-mobile money fee structure-1.png" style="display: block; margin: auto;" />

Simply, users who send high amounts have good options (further expanded by interoperability), but if it's less it doesn't matter what network they use.

If Government of Ghana has plans to tax mobile money transactions as being [reported](https://thebftonline.com/2018/business/companies/mtn-ghana-holds-2018-mobile-money-stakeholder-conference/), Telco's could consider looking at higher amount ranges (500 - 1000, 1000+) since these amounts generally pay lower fees in percentage terms, although it feels like the platform is still in its infancy.

Hope you found this as interesting as I did putting it together. Thanks for reading.

***

Find the code and data used in this analysis [here](https://github.com/DavidQuartey/Mobile-Money-Price-Value-For-Money) and more of my content on my blog, [SimpleEconomics](https://medium.com/@DaveQuartey) where I interrogate topics I find interesting from a data perspective.

I am always interested in getting involved in new projects or just connecting with others. Feel free to get in touch!
