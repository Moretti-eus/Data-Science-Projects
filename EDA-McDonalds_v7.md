---
title: "Beverages at McDonalds are the worst you can get - EDA"
author: Matheus Moretti
output:
  html_document:
    keep_md: true
    theme: journal
    highlight: tango
    number_sections: yes
    toc: true
    toc_depth: 4
    df_print: kable
---
<center><img
src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTm4jB6fBdRex8K2GXb7rWoFIoQX54bIWdHQg&usqp=CAU">
</center>



#  Introduction

The main objective of this kernel is to find out which items listed on the menu have the most empty calories within their compositions. To understand which menu items are the least healthy according to this metric, and also identify the average % of empty calories within McDonald's food and beverages.

"Empty" literally means "that contains nothing". This concept applied to food composition represents those calories that have little or no nutritional value within their composition. On the other hand, non-empty calories are those that carry essential nutrients for the correct functioning of the body, such as vitamins, minerals, antioxidants, etc.¹

Following the FDA (U.S Food and Drug Administration) classification, which considers "empty calories" to come from solid fats and added sugars², I will take an approximation considering "empty calories" to come from sugar, fats, and cholesterol.

The results show that sugar is the most relevant element of all, more than fat and cholesterol. All sugar-rich items and categories have a higher proportion of empty calories.

#  Preparation{.tabset .tabset-fade .tabset-pills}

## Loading libraries and theme


```r
library(ggplot2) # plot library
library(tidyverse) # for data manipulation
library(gridExtra) # multiple plots in 1
library(scales) # show the colors
library(ggrepel) # for graph repel (labels)
library(scales) # for % in density plots
library(skimr)
library(eeptools)
library(pubh)
library(RColorBrewer)
library(highcharter)


my_theme <- theme(plot.background = element_rect(fill = "grey97", color = "grey25"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major = element_line(colour = "grey87"),
        text = element_text(color = "grey25"),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 11),
        legend.box.background = element_rect(color = "grey25", fill = "grey97", size = 0.5),
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5))    
```

## Loading data


```r
data <- read_csv("C:/Users/mathe/Downloads/menu.csv")
```


## Missing Values


```r
test <- skim(data)

test$n_missing
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```
There's no missing values


#  Data Transformation



##  Creating beverages and food groups

Operations to change the format of "serving size", which mixed oz with grams and drinks with food. Also, to create categories for foods and beverages.




```r
#Select only fields that contain "fl oz" string and sperately 'carton' string

drinks.oz <- data[str_detect(data$`Serving Size`, " fl oz.*"),] 
drinks.ml <- data[str_detect(data$`Serving Size`, 'carton'),]

#Convert ounces to mililiters (1 oz = 29.5735 ml) and round
 
drinks.oz$Serving.Size <- 
  round(as.numeric(gsub(" fl oz.*", "", drinks.oz$`Serving Size`))*29.5735,0)
drinks.ml$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ ml).*", "\\1", drinks.ml$`Serving Size`)),0)

#Select only fields that contain "g" string

food.g <- data[str_detect(data$`Serving Size`, 'g'),] 
food.g$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ g).*", "\\1", food.g$`Serving Size`)),0)

#combine data frames
#create new column with Type as being 'drink' or 'food'
datav1 <- rbind(drinks.oz,drinks.ml)
datav1$Type <- rep("beverage(ml)", nrow(datav1))
food.g$Type <- rep("food(g)", nrow(food.g))
datav1 <- rbind(datav1,food.g)

datav1 <-datav1 %>% 
  select(- `Serving Size` )
```


##  Transform unit from weight to calories 

To understand the percentage of empty calories within the calories of each food/beverage, it's necessary to convert the unit to calories.




```r
# Isolate sugars and fiber from carbohydrates:
datav1 <- datav1 %>% 
  mutate(carbs_isolated = Carbohydrates - Sugars - `Dietary Fiber`)

# Isolando as composições de fat. 
datav1 <- datav1 %>% 
  mutate(fat_isolated = `Total Fat` - `Saturated Fat` - `Trans Fat`)

# Convert from grams to calories:
  
  #Calorie support table:
calories_intake <- tibble("fat" = 9, "cholesterol" = 9/1000, "sodium" = 0, "carbs" = 4, "fiber" = 4, "sugar" = 4, "protein" = 4)

# Conversion:

  # Isolated Fat:
datav2 <- datav1 %>% 
  mutate(Fat_isolated_cal = fat_isolated * calories_intake$fat)

   # Saturated Fat:
datav2 <- datav2 %>% 
  mutate(saturated_fat_cal = `Saturated Fat` * calories_intake$fat)
  
  # Trans Fat:
datav2 <- datav2 %>% 
  mutate(trans_fat_cal = `Trans Fat` * calories_intake$fat)

  # Cholesterol:
datav2 <- datav2 %>% 
  mutate(cholesterol_cal = Cholesterol * calories_intake$cholesterol)

  # Sugar:
datav2 <- datav2 %>% 
  mutate(sugar_cal = Sugars * calories_intake$sugar)

  # Sodium:
datav2 <- datav2 %>% 
  mutate(sodium_cal = Sodium * calories_intake$sodium)

  # Isolated Carbs:
datav2 <- datav2 %>% 
  mutate(isolated_carbs_cal = carbs_isolated * calories_intake$carbs)

  # Fiber:
datav2 <- datav2 %>% 
  mutate(fiber_cal = `Dietary Fiber` * calories_intake$fiber)

  # Protein:
datav2 <- datav2 %>% 
  mutate(protein_cal = Protein * calories_intake$protein)
```




## Creat proportion of empty calories

Having the data in the correct unit, time to make the proportions.


```r
# Create variables for empty and full calories:

datav2 <- datav2 %>% 
  mutate(empty_calories =saturated_fat_cal + trans_fat_cal + cholesterol_cal + sugar_cal + Fat_isolated_cal )

datav2 <- datav2 %>% 
  mutate(maior_que_total = ifelse(empty_calories > Calories,1,0 ))

datav2 <- datav2 %>%
  mutate(prop_empty_calories = round(empty_calories/Calories, 2))

datav2 <- datav2 %>% 
  mutate(full_calories = protein_cal + fiber_cal + isolated_carbs_cal + sodium_cal)

datav2 <- datav2 %>% 
  mutate(prop_full_calories = round(1 - prop_empty_calories, 2))


# Removing columns that will not be needed to analyze calories.
datav2 <- datav2 %>%
  select(- `Vitamin A (% Daily Value)`, -`Vitamin C (% Daily Value)`, -`Calcium (% Daily Value)` , -`Iron (% Daily Value)`, - `Calories from Fat` , - `Total Fat` , -`Saturated Fat`, -`Trans Fat` , -Cholesterol, -Sodium, -Carbohydrates, -`Dietary Fiber`, -Sugars , -Protein , -carbs_isolated , -fat_isolated, -maior_que_total)

#  Removing problematic observations:

datav2 <- datav2 %>%
  filter(!prop_full_calories == Inf)

# Must also deal with beverages that have an empty calorie ratio above 100%. My hypothesis is that there is a typo or some approximation that I don't know about, regarding the amount of calories per gram of sugar. So, for values greater than 100%, I will limit it to 100%.

  # Limiting these values to 100%:


datav2[datav2$prop_empty_calories>=1, 22] <- 1
```

## Data Summary


```r
skim(datav2)
```


Table: Data summary

|                         |       |
|:------------------------|:------|
|Name                     |datav2 |
|Number of rows           |244    |
|Number of columns        |24     |
|_______________________  |       |
|Column type frequency:   |       |
|character                |3      |
|numeric                  |21     |
|________________________ |       |
|Group variables          |None   |


**Variable type: character**

|skim_variable | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:-------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|Category      |         0|             1|   6|  18|     0|        9|          0|
|Item          |         0|             1|   5|  61|     0|      244|          0|
|Type          |         0|             1|   7|  12|     0|        2|          0|


**Variable type: numeric**

|skim_variable                 | n_missing| complete_rate|   mean|     sd|    p0|    p25|    p50|    p75|    p100|hist                                     |
|:-----------------------------|---------:|-------------:|------:|------:|-----:|------:|------:|------:|-------:|:----------------------------------------|
|Calories                      |         0|             1| 392.42| 228.07| 15.00| 240.00| 350.00| 510.00| 1880.00|▇▆▁▁▁ |
|Total Fat (% Daily Value)     |         0|             1|  23.25|  21.84|  0.00|   6.75|  20.00|  35.00|  182.00|▇▂▁▁▁ |
|Saturated Fat (% Daily Value) |         0|             1|  31.93|  26.33|  0.00|  11.00|  29.00|  49.00|  102.00|▇▅▃▂▁ |
|Cholesterol (% Daily Value)   |         0|             1|  19.60|  29.64|  0.00|   4.50|  11.50|  22.00|  192.00|▇▁▁▁▁ |
|Sodium (% Daily Value)        |         0|             1|  21.95|  24.27|  0.00|   5.00|   9.00|  39.25|  150.00|▇▃▁▁▁ |
|Carbohydrates (% Daily Value) |         0|             1|  16.82|   8.78|  1.00|  11.00|  15.00|  20.00|   47.00|▃▇▃▁▁ |
|Dietary Fiber (% Daily Value) |         0|             1|   6.96|   6.28|  0.00|   3.00|   6.00|  11.00|   28.00|▇▅▂▁▁ |
|Serving.Size                  |         0|             1| 364.70| 205.39| 29.00| 188.75| 355.00| 473.00|  946.00|▇▇▅▅▁ |
|Fat_isolated_cal              |         0|             1|  76.28|  90.90|  0.00|  13.50|  51.75| 117.00|  873.00|▇▁▁▁▁ |
|saturated_fat_cal             |         0|             1|  57.61|  47.33|  0.00|  18.00|  54.00|  90.00|  180.00|▇▆▃▂▁ |
|trans_fat_cal                 |         0|             1|   1.95|   3.96|  0.00|   0.00|   0.00|   0.00|   22.50|▇▁▁▁▁ |
|cholesterol_cal               |         0|             1|   0.53|   0.80|  0.00|   0.12|   0.32|   0.58|    5.18|▇▁▁▁▁ |
|sugar_cal                     |         0|             1| 125.41| 114.26|  0.00|  28.00|  84.00| 204.00|  512.00|▇▃▂▁▁ |
|sodium_cal                    |         0|             1|   0.00|   0.00|  0.00|   0.00|   0.00|   0.00|    0.00|▁▁▇▁▁ |
|isolated_carbs_cal            |         0|             1|  69.44|  74.31|  0.00|  12.00|  36.00| 120.00|  444.00|▇▃▁▁▁ |
|fiber_cal                     |         0|             1|   6.95|   6.24|  0.00|   4.00|   4.00|  12.00|   28.00|▇▂▃▁▁ |
|protein_cal                   |         0|             1|  56.69|  45.20|  0.00|  27.00|  48.00|  77.00|  348.00|▇▃▁▁▁ |
|empty_calories                |         0|             1| 261.79| 154.03|  8.00| 160.78| 233.22| 308.13| 1068.38|▇▇▂▁▁ |
|prop_empty_calories           |         0|             1|   0.69|   0.19|  0.32|   0.52|   0.74|   0.84|    1.00|▃▇▂▇▅ |
|full_calories                 |         0|             1| 133.08| 116.75|  0.00|  52.00|  88.00| 208.00|  816.00|▇▃▁▁▁ |
|prop_full_calories            |         0|             1|   0.31|   0.20| -0.12|   0.16|   0.26|   0.48|    0.68|▃▇▅▇▃ |


# Plots

## Distribution of empty calories proportion:


```r
datav2 %>% 
  
  ggplot(aes(x = prop_empty_calories,group = Type, fill=Type, color=Type)) +
  geom_density(alpha = 0.4, size = 1) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Proportion of empty calories", y = "Frequency", title = "Empty Calories Distribution", subtitle = "32% of empty calories is the minimum") +
  my_theme +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

It is clear that beverages have a higher proportion of empty calories.

<center><img
src="https://i.imgur.com/kTmdqd3.png">
</center>


## Correlation:


```r
coor_df <- datav2 %>%
  select(Fat_isolated_cal, saturated_fat_cal, trans_fat_cal, cholesterol_cal, sugar_cal, isolated_carbs_cal, fiber_cal, protein_cal, prop_empty_calories)



corr0 <- cor( method = "pearson", coor_df[sapply(coor_df, function(x) !is.factor(x))])


hchart(corr0)
```

```{=html}
<div id="htmlwidget-5a22777e3fa7ed82f35b" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-5a22777e3fa7ed82f35b">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":null},"yAxis":{"title":{"text":""},"categories":["Fat_isolated_cal","saturated_fat_cal","trans_fat_cal","cholesterol_cal","sugar_cal","isolated_carbs_cal","fiber_cal","protein_cal","prop_empty_calories"],"reversed":true},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false,"boderWidth":0,"dataLabels":{"enabled":false}},"treemap":{"layoutAlgorithm":"squarified"}},"series":[{"data":[{"x":0,"y":0,"value":1,"name":"Fat_isolated_cal ~ Fat_isolated_cal"},{"x":0,"y":1,"value":0.624677630511396,"name":"Fat_isolated_cal ~ saturated_fat_cal"},{"x":0,"y":2,"value":0.221279844891134,"name":"Fat_isolated_cal ~ trans_fat_cal"},{"x":0,"y":3,"value":0.608029754336921,"name":"Fat_isolated_cal ~ cholesterol_cal"},{"x":0,"y":4,"value":-0.357411652526478,"name":"Fat_isolated_cal ~ sugar_cal"},{"x":0,"y":5,"value":0.871582899679232,"name":"Fat_isolated_cal ~ isolated_carbs_cal"},{"x":0,"y":6,"value":0.616350940377046,"name":"Fat_isolated_cal ~ fiber_cal"},{"x":0,"y":7,"value":0.802051897583177,"name":"Fat_isolated_cal ~ protein_cal"},{"x":0,"y":8,"value":-0.444024280295358,"name":"Fat_isolated_cal ~ prop_empty_calories"},{"x":1,"y":0,"value":0.624677630511396,"name":"saturated_fat_cal ~ Fat_isolated_cal"},{"x":1,"y":1,"value":1,"name":"saturated_fat_cal ~ saturated_fat_cal"},{"x":1,"y":2,"value":0.616115677156916,"name":"saturated_fat_cal ~ trans_fat_cal"},{"x":1,"y":3,"value":0.618723193965806,"name":"saturated_fat_cal ~ cholesterol_cal"},{"x":1,"y":4,"value":0.131578523384229,"name":"saturated_fat_cal ~ sugar_cal"},{"x":1,"y":5,"value":0.538274548002583,"name":"saturated_fat_cal ~ isolated_carbs_cal"},{"x":1,"y":6,"value":0.297607296073095,"name":"saturated_fat_cal ~ fiber_cal"},{"x":1,"y":7,"value":0.567469467998866,"name":"saturated_fat_cal ~ protein_cal"},{"x":1,"y":8,"value":-0.130007733563937,"name":"saturated_fat_cal ~ prop_empty_calories"},{"x":2,"y":0,"value":0.221279844891134,"name":"trans_fat_cal ~ Fat_isolated_cal"},{"x":2,"y":1,"value":0.616115677156916,"name":"trans_fat_cal ~ saturated_fat_cal"},{"x":2,"y":2,"value":1,"name":"trans_fat_cal ~ trans_fat_cal"},{"x":2,"y":3,"value":0.239148172534272,"name":"trans_fat_cal ~ cholesterol_cal"},{"x":2,"y":4,"value":0.316096218398369,"name":"trans_fat_cal ~ sugar_cal"},{"x":2,"y":5,"value":0.161891721148688,"name":"trans_fat_cal ~ isolated_carbs_cal"},{"x":2,"y":6,"value":0.0234090061646455,"name":"trans_fat_cal ~ fiber_cal"},{"x":2,"y":7,"value":0.371715794806196,"name":"trans_fat_cal ~ protein_cal"},{"x":2,"y":8,"value":0.0151105360501593,"name":"trans_fat_cal ~ prop_empty_calories"},{"x":3,"y":0,"value":0.608029754336921,"name":"cholesterol_cal ~ Fat_isolated_cal"},{"x":3,"y":1,"value":0.618723193965806,"name":"cholesterol_cal ~ saturated_fat_cal"},{"x":3,"y":2,"value":0.239148172534272,"name":"cholesterol_cal ~ trans_fat_cal"},{"x":3,"y":3,"value":1,"name":"cholesterol_cal ~ cholesterol_cal"},{"x":3,"y":4,"value":-0.186998425333408,"name":"cholesterol_cal ~ sugar_cal"},{"x":3,"y":5,"value":0.573472636923275,"name":"cholesterol_cal ~ isolated_carbs_cal"},{"x":3,"y":6,"value":0.412655412503544,"name":"cholesterol_cal ~ fiber_cal"},{"x":3,"y":7,"value":0.54519561533714,"name":"cholesterol_cal ~ protein_cal"},{"x":3,"y":8,"value":-0.269409319866678,"name":"cholesterol_cal ~ prop_empty_calories"},{"x":4,"y":0,"value":-0.357411652526478,"name":"sugar_cal ~ Fat_isolated_cal"},{"x":4,"y":1,"value":0.131578523384229,"name":"sugar_cal ~ saturated_fat_cal"},{"x":4,"y":2,"value":0.316096218398369,"name":"sugar_cal ~ trans_fat_cal"},{"x":4,"y":3,"value":-0.186998425333408,"name":"sugar_cal ~ cholesterol_cal"},{"x":4,"y":4,"value":1,"name":"sugar_cal ~ sugar_cal"},{"x":4,"y":5,"value":-0.448073006325663,"name":"sugar_cal ~ isolated_carbs_cal"},{"x":4,"y":6,"value":-0.393041108697263,"name":"sugar_cal ~ fiber_cal"},{"x":4,"y":7,"value":-0.275991020381243,"name":"sugar_cal ~ protein_cal"},{"x":4,"y":8,"value":0.689730347973038,"name":"sugar_cal ~ prop_empty_calories"},{"x":5,"y":0,"value":0.871582899679232,"name":"isolated_carbs_cal ~ Fat_isolated_cal"},{"x":5,"y":1,"value":0.538274548002583,"name":"isolated_carbs_cal ~ saturated_fat_cal"},{"x":5,"y":2,"value":0.161891721148688,"name":"isolated_carbs_cal ~ trans_fat_cal"},{"x":5,"y":3,"value":0.573472636923275,"name":"isolated_carbs_cal ~ cholesterol_cal"},{"x":5,"y":4,"value":-0.448073006325663,"name":"isolated_carbs_cal ~ sugar_cal"},{"x":5,"y":5,"value":1,"name":"isolated_carbs_cal ~ isolated_carbs_cal"},{"x":5,"y":6,"value":0.699268661469137,"name":"isolated_carbs_cal ~ fiber_cal"},{"x":5,"y":7,"value":0.748852780167058,"name":"isolated_carbs_cal ~ protein_cal"},{"x":5,"y":8,"value":-0.658823858699031,"name":"isolated_carbs_cal ~ prop_empty_calories"},{"x":6,"y":0,"value":0.616350940377046,"name":"fiber_cal ~ Fat_isolated_cal"},{"x":6,"y":1,"value":0.297607296073095,"name":"fiber_cal ~ saturated_fat_cal"},{"x":6,"y":2,"value":0.0234090061646455,"name":"fiber_cal ~ trans_fat_cal"},{"x":6,"y":3,"value":0.412655412503544,"name":"fiber_cal ~ cholesterol_cal"},{"x":6,"y":4,"value":-0.393041108697263,"name":"fiber_cal ~ sugar_cal"},{"x":6,"y":5,"value":0.699268661469137,"name":"fiber_cal ~ isolated_carbs_cal"},{"x":6,"y":6,"value":1,"name":"fiber_cal ~ fiber_cal"},{"x":6,"y":7,"value":0.612123572132773,"name":"fiber_cal ~ protein_cal"},{"x":6,"y":8,"value":-0.603900006507787,"name":"fiber_cal ~ prop_empty_calories"},{"x":7,"y":0,"value":0.802051897583177,"name":"protein_cal ~ Fat_isolated_cal"},{"x":7,"y":1,"value":0.567469467998866,"name":"protein_cal ~ saturated_fat_cal"},{"x":7,"y":2,"value":0.371715794806196,"name":"protein_cal ~ trans_fat_cal"},{"x":7,"y":3,"value":0.54519561533714,"name":"protein_cal ~ cholesterol_cal"},{"x":7,"y":4,"value":-0.275991020381243,"name":"protein_cal ~ sugar_cal"},{"x":7,"y":5,"value":0.748852780167058,"name":"protein_cal ~ isolated_carbs_cal"},{"x":7,"y":6,"value":0.612123572132773,"name":"protein_cal ~ fiber_cal"},{"x":7,"y":7,"value":1,"name":"protein_cal ~ protein_cal"},{"x":7,"y":8,"value":-0.5788198469998,"name":"protein_cal ~ prop_empty_calories"},{"x":8,"y":0,"value":-0.444024280295358,"name":"prop_empty_calories ~ Fat_isolated_cal"},{"x":8,"y":1,"value":-0.130007733563937,"name":"prop_empty_calories ~ saturated_fat_cal"},{"x":8,"y":2,"value":0.0151105360501593,"name":"prop_empty_calories ~ trans_fat_cal"},{"x":8,"y":3,"value":-0.269409319866678,"name":"prop_empty_calories ~ cholesterol_cal"},{"x":8,"y":4,"value":0.689730347973038,"name":"prop_empty_calories ~ sugar_cal"},{"x":8,"y":5,"value":-0.658823858699031,"name":"prop_empty_calories ~ isolated_carbs_cal"},{"x":8,"y":6,"value":-0.603900006507787,"name":"prop_empty_calories ~ fiber_cal"},{"x":8,"y":7,"value":-0.5788198469998,"name":"prop_empty_calories ~ protein_cal"},{"x":8,"y":8,"value":1,"name":"prop_empty_calories ~ prop_empty_calories"}],"type":"heatmap"}],"tooltip":{"formatter":"function(){\n                 return this.point.name + ': ' +\n                   Highcharts.numberFormat(this.point.value, 2)\n               }"},"legend":{"enabled":true},"colorAxis":{"auxarg":true,"stops":[[0,"#FF5733"],[0.5,"#F8F5F5"],[1,"#2E86C1"]],"min":-1,"max":1},"xAxis":{"categories":["Fat_isolated_cal","saturated_fat_cal","trans_fat_cal","cholesterol_cal","sugar_cal","isolated_carbs_cal","fiber_cal","protein_cal","prop_empty_calories"],"title":{"text":""},"opposite":true}},"theme":{"chart":{"backgroundColor":"transparent"},"colors":["#7cb5ec","#434348","#90ed7d","#f7a35c","#8085e9","#f15c80","#e4d354","#2b908f","#f45b5b","#91e8e1"]},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>
```

<center><img
src="https://i.imgur.com/LZnOmpW.png">
</center>

It's clear from the analysis that for now the data is making sense.

* Healthier nutrients (fiber, protein, etc.) have a negative correlation with the proportion of empty calories;
* Positive correlations are with unhealthier nutrients (trans fat and sugar)
   + The highest correlation within the empty calorie ratio is with the amount of sugar.
   + Which indicates that drinks with more sugar tend to have a higher proportion of empty calories, as is the case with soft drinks and/or sweetened drinks. This can also be the case with desserts.

To understand more deeply it is necessary to deepen the analysis.


## Distribution of nutrients with highest correlation:




```r
# Negative correlation:


datav2 %>% 
  ggplot(aes(x = sugar_cal,group = Type, fill=Type, color=Type)) +
  geom_density(alpha = 0.4, size = 1) +
  #coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Amount of sugar (cal)", y = "Frequency", title = "Distribution of sugar (calories)", subtitle = "Dotted line reppresents suggested daily intake³") +
  my_theme +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  geom_vline(xintercept = 200, size=1, color="tomato2", linetype="dotted")
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# Positive correlation:  

  datav2 %>% 
  ggplot(aes(x = fiber_cal,group = Type, fill=Type, color=Type)) +
  geom_density(alpha = 0.4, size = 1) +
  #coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Amount of dietary fiber (cal)", y = "Frequency", title = "Distribution of dietary fiber (calories)", subtitle = "Dotted line reppresents suggested daily intake³") +
  my_theme +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  geom_vline(xintercept = 112, size=1, color="tomato2", linetype="dotted")
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

 Drinks have more sugar and, it seems, a higher proportion of empty calories.

Foods, on the other hand, because they have more fiber and protein, seem to have proportionally less empty calories.


<center><img
src="https://i.imgur.com/gEbuX77.png">
</center>

<center><img
src="https://i.imgur.com/K8SjwMc.png">
</center>



## Category Analysis:


```r
datav2 %>%
  ggplot(aes(x = reorder(Category, prop_empty_calories), y = prop_empty_calories, fill = reorder(Category, prop_empty_calories))) +
  geom_boxplot(alpha = 0.4) +
  labs(x = "", y = "Proportion of empty calories", title = "Empty calories per category", subtitle = "Beverages have median of 1") +
  my_theme +
  coord_flip()+
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  guides(fill=guide_legend(title="Categories"))+
  geom_jitter(color="grey", size=0.4, alpha=0.9)
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

<center><img
src="https://i.imgur.com/ydY8Dp3.png">
</center>

Note that the top 3 is made up of drinks. The first food-related category to appear is desets, the highest sugar category.

So what is the role of sugar in empty calories? Does it make up most of the calories?



## Sugar analysis within categories:


```r
category_df <- datav2 %>% 
  mutate(prop_cal_sug = sugar_cal/Calories) %>%
  group_by(Category) %>% 
  summarise(avg_cal = mean(prop_empty_calories), avg_sug = mean(prop_cal_sug))

options(repr.plot.width=18, repr.plot.height=7) 



category_df %>% 
  mutate(Category = factor(Category)) %>%
  
  ggplot(aes(x = reorder(Category, avg_sug), y = avg_sug)) +
  geom_bar(stat = "identity", aes(fill = reorder(Category, avg_sug))) +
  coord_flip() +
  geom_label(aes(label = formatC(avg_sug, format="f", big.mark=",", digits=2)), size = 3) +
  my_theme +
  labs(x = "Category", y = "Average proportion of calories that come from sugar", title = "Largest proportion of sugar", subtitle = "")+
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  guides(fill=guide_legend(title="Categories"))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
datav2 %>% 
  ggplot(aes(x = sugar_cal, y= reorder(Category, sugar_cal), fill=reorder(Category, sugar_cal))) +
  geom_density_ridges(alpha=1.5, bandwidth=5) +
  #coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Amount of sugar (cal)", y = "Frequency", title = "Distribution of sugar (calories)", subtitle = "Dotted line reppresents suggested daily intake³", fill="Categories") +
  my_theme +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  geom_vline(xintercept = 200, size=1, color="tomato2", linetype="dotted")
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

<center><img
src="https://i.imgur.com/nBH5LKQ.png">
</center>


<center><img
src="https://i.imgur.com/CGYUkH0.png">
</center>


It seems so, the four categories with the highest proportion of empty calories are the four categories that contain the most sugar.

To deepen the presence of sugar on the menu, it is worth noting its distribution.

https://www.hsph.harvard.edu/nutritionsource/carbohydrates/added-sugar-in-the-diet/


## Emptiest food by category: {.tabset .tabset-fade .tabset-pills}




### Beverages


```r
#Select beverages 

beverages <- list_datav2$Beverages

#Remove duplicates due to size: small coke and a large one have the same proportion of empty calories. 

beverages <- beverages[-grep("Small" , beverages$Item),]
beverages <- beverages[-grep("Medium" , beverages$Item),]
beverages <- beverages[-grep("Child" , beverages$Item),]

# Remove size:

beverages$Item <- str_replace(beverages$Item, coll("(Large)"), "")


 beverages%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Beverages'", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

<center><img
src="https://i.imgur.com/DLQVtj6.png">
</center>


### Coffee & Tea


```r
coff_tea <- list_datav2$`Coffee & Tea`

coff_tea <- coff_tea[-grep("Small" , coff_tea$Item),]
coff_tea <- coff_tea[-grep("Medium" , coff_tea$Item),]
coff_tea <- coff_tea[-grep("Child" , coff_tea$Item),]

coff_tea$Item <- str_replace(coff_tea$Item, coll("(Large)"), "")

 coff_tea%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Coffe & tea'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

<center><img
src="https://i.imgur.com/0MseT1V.png">
</center>

### Smoothies & Shakes



```r
smoothies_shakes <- list_datav2$`Smoothies & Shakes`

smoothies_shakes <- smoothies_shakes[-grep("Small" , smoothies_shakes$Item),]
smoothies_shakes <- smoothies_shakes[-grep("Medium" , smoothies_shakes$Item),]

smoothies_shakes$Item <- str_replace(smoothies_shakes$Item, coll("(Large)"), "")


 smoothies_shakes%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Smoothies & Shakes'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 2))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

<center><img
src="https://i.imgur.com/f6gLEoB.png">
</center>

### Breakfast



```r
breakfast <- list_datav2$Breakfast


# Limiting to 6 items per chart:

breakfast <- breakfast[order(breakfast$prop_empty_calories, decreasing = TRUE), ]
breakfast <- head(breakfast, n=6)




 breakfast%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Breakfast'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

<center><img
src="https://i.imgur.com/rVqy0in.png">
</center>

### Salads



```r
salads <- list_datav2$Salads


 salads%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Salads'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

<center><img
src="https://i.imgur.com/VT4jaoJ.png">
</center>

### Beef & Pork



```r
beef_pork <- list_datav2$`Beef & Pork`


# Limiting to 6 items per chart:

beef_pork <- beef_pork[order(beef_pork$prop_empty_calories, decreasing = TRUE), ]
beef_pork <- head(beef_pork, n=6)



 beef_pork%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Beef & Pork'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

<center><img
src="https://i.imgur.com/MBZ3kDd.png">
</center>

### Chicken & Fish



```r
chicken_fish <- list_datav2$`Chicken & Fish`

#Remove duplicates:

chicken_fish <- chicken_fish[-grep("(6 piece)" , chicken_fish$Item),]
chicken_fish <- chicken_fish[-grep("(10 piece)" , chicken_fish$Item),]
chicken_fish <- chicken_fish[-grep("(4 piece)" , chicken_fish$Item),]
chicken_fish <- chicken_fish[-grep("(20 piece)" , chicken_fish$Item),]

chicken_fish$Item <- str_replace(chicken_fish$Item, coll("(40 piece)"), "")


# Limiting to 6 items per chart:

chicken_fish <- chicken_fish[order(chicken_fish$prop_empty_calories, decreasing = TRUE), ]
chicken_fish <- head(chicken_fish, n=6)


 chicken_fish%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Chicken & Fish'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 2))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

<center><img
src="https://i.imgur.com/XwLd9l8.png">
</center>


### Snacks & Sides



```r
snacks_sides <- list_datav2$`Snacks & Sides`


# Limiting to 6 items per chart:

snacks_sides <- snacks_sides[order(snacks_sides$prop_empty_calories, decreasing = TRUE), ]
snacks_sides <- head(snacks_sides, n=6)


 snacks_sides%>%
  ggplot(aes(x = reorder(Item, prop_empty_calories), y = prop_empty_calories)) +
  geom_bar(stat = "identity", aes(fill = as.factor(prop_empty_calories))) +
  coord_flip() +
  geom_label(aes(label = formatC(prop_empty_calories, format="f", big.mark=",", digits=2)), size = 3) +
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds")+
  my_theme +
  labs(x = "", y = "Proportion of empty Calories", title = "Empitiest calories in 'Snacks & Sides'  ", subtitle = "")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
```

![](EDA-McDonalds_v7_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

<center><img
src="https://i.imgur.com/24hHPMa.png">
</center>

# Conclusion

* In McDonald's the worst you can get in terms of nutritional value is related to it's beverages (sodas, coffee & tea and smoothies & shakes)

* Fat isn't the villain of the empty calories, but sugar is.

* Beverages are poor nutritionally because are made mostly of sugar.

* According to a 15 year long study, people who got 17% - 21% of its calories from sugar, had a 38% higher risk of dying from heart disease⁴ . 



# Reference

[1](https://www.health.harvard.edu/staying-healthy/understanding-empty-calories), [2](https://health.gov/sites/default/files/2019-09/Scientific-Report-of-the-2015-Dietary-Guidelines-Advisory-Committee.pdf), [3](https://www.fda.gov/media/135301/download), [4](https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/1819573) , 
[theme by Andrada Olteanu](https://www.kaggle.com/andradaolteanu/i-fraud-detection-eda-and-understanding), 
[Conversion of units by Lukasz Piwek](https://www.kaggle.com/lukaszpiwek/mcdonald-s-menu-exploratory-data-analysis)
