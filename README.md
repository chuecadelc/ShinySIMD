# ShinySIMD App

Hello (again) world,

This was my first repository back in 2020 and now the time has come for an update (2025). I built an RShiny app using the SIMD 2016 dataset as part of my Q-Step internship at University of Glasgow in 2017 as I was finishing my undergraduate degree. For those who are not familiar with the acronym, it means Scottish Index of Multiple Deprivation. You can find all the details about this dataset here: https://www2.gov.scot/Topics/Statistics/SIMD.

This RShiny app comprises of four tabs. The first one contains information about the dataset and the app's features. The second tab is a one-variable description including summary statistics and various visualisations (histogram, density plot and boxplot). The third tab offers two-variable comparisons in the form of a scatterplot and hexbin chart. There is also the option to run a simple linear regression model. Additionally, the data can be subset based on location (e.g Greater Glasgow Area, Edinburgh, Aberdeen, etc). The last tab is an interactive map visualisation. There's an option to create a dedicated map of Glasgow City Council area in which all variables can be visualised and the colors and cartography can be manipulated too. Note that for the Glasgow map I used QGIS to create my own geojson file whereas the Scotland map uses the Local Authority Districts (LADs) of 2016 and 2020 respectively. 

So overall, this was the first Shiny App I built in order to help others better understand statistics and visualisations. This app was used in public lectures promoting the Q-Step Programme at Glasgow University where the speakers guided the public along. 

Among the updates are a more stylised UI with improved CSS theme (Bootswatch/Minty), better variable descriptions, more visualisation options for one and two-variable comparisons. Additionally, I have included the SIMD2020 dataset which is the most recent release so people can compare. Lastly, while the previous version interactive map was limited to Glasgow, this one includes the whole of Scotland too. Note that due to the low population density of some areas it may appear sparse but when you zoom in you'll see more defined areas.

If you have any questions or suggestions please do not hesitate to contact me.
