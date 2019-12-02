# ShinySIMD

Hello world,

This is my first repository so please be kind. So, I built a shiny app using the SIMD dataset. For those who are not familiar with the acronym it means Scottish Index of Multiple Deprivation. You can find all the details about this dataset here: https://www2.gov.scot/Topics/Statistics/SIMD

The SIMD has countless Scottish constituencies so for the purpose of my app I reduced these to the Glasgow City Council area. This data was collected on 2016 and the datazones (the geographical boundaries) are from 2011 which meant that I had to do some adjustments to fit the data from SIMD to the datazones. I used QGIS to do so and with the resulting shapefiles I built my own map, that I later integrated onto the app.


This Shiny App comprises of four tabs. The first one being an information tab so you can learn a bit more about why I built my app and the purpose. The second tab is summary statistics which includes a histogram. The third tab offers a comparison between each of the variables. Also, it shows a scatterplot to visualise the relationship between variable X and Y. Additionally, the mean line can be added and a sub sample of just Glasgow can be selected. The last tab is a dedicated map of Glasgow City Council area in which all variables can be visualised and the colors and cartography can be manipulated too.


So overall, this was the first Shiny App I built in order to help others better understand statistics and visualisations. This app was used in public lectures where the speakers guided the public along. If you have any questions or suggestions please do not hesitate to contact me.
