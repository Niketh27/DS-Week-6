---
title: 'Weekly Exercises #6'
author: "Niketh Gamage"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---

Link to github(App is included there) - https://github.com/Niketh27/DS-Week-6



```r
library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
theme_set(theme_minimal())
```


```r
# Lisa's garden data
data("garden_harvest")

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```

## Put your homework on GitHub!

Go [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md) or to previous homework to remind yourself how to get set up. 

Once your repository is created, you should always open your **project** rather than just opening an .Rmd file. You can do that by either clicking on the .Rproj file in your repository folder on your computer. Or, by going to the upper right hand corner in R Studio and clicking the arrow next to where it says Project: (None). You should see your project come up in that list if you've used it recently. You could also go to File --> Open Project and navigate to your .Rproj file. 

## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* **NEW!!** With animated graphs, add `eval=FALSE` to the code chunk that creates the animation and saves it using `anim_save()`. Add another code chunk to reread the gif back into the file. See the [tutorial](https://animation-and-interactivity-in-r.netlify.app/) for help. 

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.

## Your first `shiny` app 

  1. This app will also use the COVID data. Make sure you load that data and all the libraries you need in the `app.R` file you create. Below, you will post a link to the app that you publish on shinyapps.io. You will create an app to compare states' cumulative number of COVID cases over time. The x-axis will be number of days since 20+ cases and the y-axis will be cumulative cases on the log scale (`scale_y_log10()`). We use number of days since 20+ cases on the x-axis so we can make better comparisons of the curve trajectories. You will have an input box where the user can choose which states to compare (`selectInput()`) and have a submit button to click once the user has chosen all states they're interested in comparing. The graph should display a different line for each state, with labels either on the graph or in a legend. Color can be used if needed. 
 NOTE: App is on github(https://github.com/Niketh27/DS-Week-6)
  

```r
 # picked_states = c("Alabama","Minnesota", "Michigan", "Arizona")
 # picked_states
```
#  
# ```{r}
# covid19 %>% 
#   group_by(state,date) %>% 
#   filter(total_cases >= 20) %>% 
#   group_by(state) %>% 
#   mutate(twenty_day = min(date)) %>% 
#   mutate(days_since_20 = date - twenty_day) %>% 
#   filter( state %in% picked_states) %>% 
#   ggplot()+
#   geom_line(aes(y = log10(total_cases), x= days_since_20, color = state))
# ```

```r
# covid19 %>% 
#   filter(cases >= 20) %>%
#   group_by(state) %>%
#   mutate(twenty_day = min(date)) %>%
#   mutate(days_since_20 = date - twenty_day) %>%
#   filter( state %in% picked_states) %>%
#   ggplot()+
#   geom_line(aes(y = log10(cases), x= days_since_20, color = state))
```

  
## Warm-up exercises from tutorial

  2. Read in the fake garden harvest data. Find the data [here](https://github.com/llendway/scraping_etc/blob/main/2020_harvest.csv) and click on the `Raw` button to get a direct link to the data. 
  

```r
X2020_harvest <- read_csv("https://raw.githubusercontent.com/llendway/scraping_etc/main/2020_harvest.csv", 
    col_types = cols(weight = col_number()), 
    na = "null", skip = 2) %>% 
  select(-X1)

head(X2020_harvest)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["vegetable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["variety"],"name":[2],"type":["chr"],"align":["left"]},{"label":["date"],"name":[3],"type":["chr"],"align":["left"]},{"label":["weight"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["units"],"name":[5],"type":["chr"],"align":["left"]}],"data":[{"1":"lettuce","2":"reseed","3":"6/6/20","4":"20","5":"grams"},{"1":"radish","2":"Garden Party Mix","3":"6/6/20","4":"36","5":"grams"},{"1":"lettuce","2":"reseed","3":"6/8/20","4":"15","5":"grams"},{"1":"lettuce","2":"reseed","3":"6/9/20","4":"10","5":"grams"},{"1":"radish","2":"Garden Party Mix","3":"6/11/20","4":"67","5":"grams"},{"1":"lettuce","2":"Farmer's Market Blend","3":"6/11/20","4":"12","5":"grams"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
  
  3. Read in this [data](https://www.kaggle.com/heeraldedhia/groceries-dataset) from the kaggle website. You will need to download the data first. Save it to your project/repo folder. Do some quick checks of the data to assure it has been read in appropriately.

```r
Groceries_dataset <- read_csv("~/Macalester/Courses/Spring 21/DS/week 6 excercises/Groceries_dataset.csv/Groceries_dataset.csv", 
    na = "null")
head(Groceries_dataset)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Member_number"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Date"],"name":[2],"type":["chr"],"align":["left"]},{"label":["itemDescription"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"1808","2":"21-07-2015","3":"tropical fruit"},{"1":"2552","2":"05-01-2015","3":"whole milk"},{"1":"2300","2":"19-09-2015","3":"pip fruit"},{"1":"1187","2":"12-12-2015","3":"other vegetables"},{"1":"3037","2":"01-02-2015","3":"whole milk"},{"1":"4941","2":"14-02-2015","3":"rolls/buns"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

  4. CHALLENGE(not graded): Write code to replicate the table shown below (open the .html file to see it) created from the `garden_harvest` data as best as you can. When you get to coloring the cells, I used the following line of code for the `colors` argument:
  

```r
colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character()
```




  5. Create a table using `gt` with data from your project or from the `garden_harvest` data if your project data aren't ready.

```r
gt_garden_harvest<-gt(garden_harvest)
```


```r
gt_garden_harvest<- gt_garden_harvest %>% 
  tab_header(title = md("**Garden Harvest Data From Prof. Lendway's Garden**")) %>% 
  tab_source_note("Source: Prof. Lendway - https://github.com/llendway/gardenR")

gt_garden_harvest
```

```{=html}
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gwdutayfow .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gwdutayfow .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwdutayfow .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gwdutayfow .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gwdutayfow .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwdutayfow .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwdutayfow .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gwdutayfow .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gwdutayfow .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gwdutayfow .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gwdutayfow .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gwdutayfow .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#gwdutayfow .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gwdutayfow .gt_from_md > :first-child {
  margin-top: 0;
}

#gwdutayfow .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gwdutayfow .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gwdutayfow .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#gwdutayfow .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwdutayfow .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gwdutayfow .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwdutayfow .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gwdutayfow .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gwdutayfow .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwdutayfow .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwdutayfow .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gwdutayfow .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwdutayfow .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gwdutayfow .gt_left {
  text-align: left;
}

#gwdutayfow .gt_center {
  text-align: center;
}

#gwdutayfow .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gwdutayfow .gt_font_normal {
  font-weight: normal;
}

#gwdutayfow .gt_font_bold {
  font-weight: bold;
}

#gwdutayfow .gt_font_italic {
  font-style: italic;
}

#gwdutayfow .gt_super {
  font-size: 65%;
}

#gwdutayfow .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="gwdutayfow" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style><strong>Garden Harvest Data From Prof. Lendway's Garden</strong></th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">vegetable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">variety</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">date</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">weight</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">units</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">reseed</td>
      <td class="gt_row gt_left">2020-06-06</td>
      <td class="gt_row gt_right">20</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-06-06</td>
      <td class="gt_row gt_right">36</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">reseed</td>
      <td class="gt_row gt_left">2020-06-08</td>
      <td class="gt_row gt_right">15</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">reseed</td>
      <td class="gt_row gt_left">2020-06-09</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-06-11</td>
      <td class="gt_row gt_right">67</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-11</td>
      <td class="gt_row gt_right">12</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-11</td>
      <td class="gt_row gt_right">9</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">leaves</td>
      <td class="gt_row gt_left">2020-06-11</td>
      <td class="gt_row gt_right">8</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-06-13</td>
      <td class="gt_row gt_right">53</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-13</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-13</td>
      <td class="gt_row gt_right">14</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-06-13</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-17</td>
      <td class="gt_row gt_right">48</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-17</td>
      <td class="gt_row gt_right">58</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-06-17</td>
      <td class="gt_row gt_right">8</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-17</td>
      <td class="gt_row gt_right">121</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">chives</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-06-17</td>
      <td class="gt_row gt_right">8</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-06-18</td>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-18</td>
      <td class="gt_row gt_right">47</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-18</td>
      <td class="gt_row gt_right">59</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">leaves</td>
      <td class="gt_row gt_left">2020-06-18</td>
      <td class="gt_row gt_right">25</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-19</td>
      <td class="gt_row gt_right">58</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-19</td>
      <td class="gt_row gt_right">39</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">leaves</td>
      <td class="gt_row gt_left">2020-06-19</td>
      <td class="gt_row gt_right">11</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-19</td>
      <td class="gt_row gt_right">38</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">22</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">25</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">16</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">71</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">148</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">asparagus</td>
      <td class="gt_row gt_left">asparagus</td>
      <td class="gt_row gt_left">2020-06-20</td>
      <td class="gt_row gt_right">20</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">37</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">71</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">95</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">51</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">13</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">leaves</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">57</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-06-21</td>
      <td class="gt_row gt_right">60</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-22</td>
      <td class="gt_row gt_right">37</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-22</td>
      <td class="gt_row gt_right">52</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-22</td>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-06-22</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-06-22</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-22</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-06-23</td>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-23</td>
      <td class="gt_row gt_right">165</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-23</td>
      <td class="gt_row gt_right">41</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">2020-06-23</td>
      <td class="gt_row gt_right">2</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-06-23</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-24</td>
      <td class="gt_row gt_right">34</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-24</td>
      <td class="gt_row gt_right">122</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-25</td>
      <td class="gt_row gt_right">22</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-25</td>
      <td class="gt_row gt_right">30</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-06-26</td>
      <td class="gt_row gt_right">17</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-26</td>
      <td class="gt_row gt_right">425</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-27</td>
      <td class="gt_row gt_right">52</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-06-27</td>
      <td class="gt_row gt_right">89</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-27</td>
      <td class="gt_row gt_right">60</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-06-27</td>
      <td class="gt_row gt_right">333</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-28</td>
      <td class="gt_row gt_right">793</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-28</td>
      <td class="gt_row gt_right">99</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-28</td>
      <td class="gt_row gt_right">111</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-29</td>
      <td class="gt_row gt_right">58</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">mustard greens</td>
      <td class="gt_row gt_left">2020-06-29</td>
      <td class="gt_row gt_right">23</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-06-29</td>
      <td class="gt_row gt_right">625</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-06-29</td>
      <td class="gt_row gt_right">561</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-06-29</td>
      <td class="gt_row gt_right">30</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-06-29</td>
      <td class="gt_row gt_right">82</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-06-30</td>
      <td class="gt_row gt_right">32</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-06-30</td>
      <td class="gt_row gt_right">80</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-01</td>
      <td class="gt_row gt_right">60</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-07-02</td>
      <td class="gt_row gt_right">144</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-07-02</td>
      <td class="gt_row gt_right">16</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-07-02</td>
      <td class="gt_row gt_right">798</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-02</td>
      <td class="gt_row gt_right">743</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-03</td>
      <td class="gt_row gt_right">217</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-07-03</td>
      <td class="gt_row gt_right">216</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-07-03</td>
      <td class="gt_row gt_right">88</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-03</td>
      <td class="gt_row gt_right">9</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-04</td>
      <td class="gt_row gt_right">285</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-07-04</td>
      <td class="gt_row gt_right">457</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-04</td>
      <td class="gt_row gt_right">147</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-06</td>
      <td class="gt_row gt_right">17</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-06</td>
      <td class="gt_row gt_right">175</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-06</td>
      <td class="gt_row gt_right">235</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-07-06</td>
      <td class="gt_row gt_right">189</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-07-06</td>
      <td class="gt_row gt_right">433</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-06</td>
      <td class="gt_row gt_right">48</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-07</td>
      <td class="gt_row gt_right">67</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_left">2020-07-07</td>
      <td class="gt_row gt_right">62</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-07-07</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-07-07</td>
      <td class="gt_row gt_right">43</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-07</td>
      <td class="gt_row gt_right">11</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-07</td>
      <td class="gt_row gt_right">13</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">75</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">252</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">178</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">39</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">181</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">83</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">96</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-07-08</td>
      <td class="gt_row gt_right">75</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-09</td>
      <td class="gt_row gt_right">61</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-09</td>
      <td class="gt_row gt_right">131</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-09</td>
      <td class="gt_row gt_right">140</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-07-09</td>
      <td class="gt_row gt_right">69</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-09</td>
      <td class="gt_row gt_right">78</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-10</td>
      <td class="gt_row gt_right">61</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-10</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">60</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">77</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">79</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">105</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">701</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-11</td>
      <td class="gt_row gt_right">24</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-12</td>
      <td class="gt_row gt_right">130</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-07-12</td>
      <td class="gt_row gt_right">89</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-12</td>
      <td class="gt_row gt_right">492</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-12</td>
      <td class="gt_row gt_right">83</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">47</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">145</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">85</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">53</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">137</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-13</td>
      <td class="gt_row gt_right">443</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-07-14</td>
      <td class="gt_row gt_right">128</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-14</td>
      <td class="gt_row gt_right">152</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-07-14</td>
      <td class="gt_row gt_right">207</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-14</td>
      <td class="gt_row gt_right">526</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-14</td>
      <td class="gt_row gt_right">152</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-15</td>
      <td class="gt_row gt_right">393</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-15</td>
      <td class="gt_row gt_right">743</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-15</td>
      <td class="gt_row gt_right">1057</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-07-15</td>
      <td class="gt_row gt_right">39</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-07-16</td>
      <td class="gt_row gt_right">29</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_left">2020-07-16</td>
      <td class="gt_row gt_right">61</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Delicious Duo</td>
      <td class="gt_row gt_left">2020-07-16</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-17</td>
      <td class="gt_row gt_right">88</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">2020-07-17</td>
      <td class="gt_row gt_right">33</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-17</td>
      <td class="gt_row gt_right">16</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-07-17</td>
      <td class="gt_row gt_right">20</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-17</td>
      <td class="gt_row gt_right">347</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-18</td>
      <td class="gt_row gt_right">77</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-07-18</td>
      <td class="gt_row gt_right">172</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-07-18</td>
      <td class="gt_row gt_right">61</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-18</td>
      <td class="gt_row gt_right">81</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-18</td>
      <td class="gt_row gt_right">294</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-18</td>
      <td class="gt_row gt_right">660</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-07-19</td>
      <td class="gt_row gt_right">113</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-19</td>
      <td class="gt_row gt_right">531</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-19</td>
      <td class="gt_row gt_right">344</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-19</td>
      <td class="gt_row gt_right">37</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_left">2020-07-19</td>
      <td class="gt_row gt_right">140</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">134</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">179</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">336</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">107</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">128</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">thai</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">12</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">519</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">559</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">197</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">123</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">178</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-07-20</td>
      <td class="gt_row gt_right">102</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">110</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">86</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">137</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">339</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">21</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">21</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-21</td>
      <td class="gt_row gt_right">7</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-22</td>
      <td class="gt_row gt_right">76</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-22</td>
      <td class="gt_row gt_right">351</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-22</td>
      <td class="gt_row gt_right">655</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-22</td>
      <td class="gt_row gt_right">23</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-23</td>
      <td class="gt_row gt_right">129</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-07-23</td>
      <td class="gt_row gt_right">56</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-07-23</td>
      <td class="gt_row gt_right">466</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-07-23</td>
      <td class="gt_row gt_right">91</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-23</td>
      <td class="gt_row gt_right">130</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">525</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">31</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">140</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">247</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">220</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">1321</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">32</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">93</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">16</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">3</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">68</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">178</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Dragon</td>
      <td class="gt_row gt_left">2020-07-24</td>
      <td class="gt_row gt_right">80</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-07-25</td>
      <td class="gt_row gt_right">463</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-25</td>
      <td class="gt_row gt_right">106</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-07-25</td>
      <td class="gt_row gt_right">121</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-25</td>
      <td class="gt_row gt_right">901</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-26</td>
      <td class="gt_row gt_right">81</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-07-26</td>
      <td class="gt_row gt_right">148</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">1542</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">728</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">785</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">113</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">29</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">801</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">99</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">49</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">149</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">39</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">174</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">129</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Yod Fah</td>
      <td class="gt_row gt_left">2020-07-27</td>
      <td class="gt_row gt_right">372</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">160</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">611</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">203</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">312</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">315</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">131</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">91</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-28</td>
      <td class="gt_row gt_right">76</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">153</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">442</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">240</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">209</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">73</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">457</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">514</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">305</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-07-29</td>
      <td class="gt_row gt_right">280</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">91</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">101</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">94</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">116</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">107</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-30</td>
      <td class="gt_row gt_right">626</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">307</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">197</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">633</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">290</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">1215</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">592</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">23</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">31</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">107</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-07-31</td>
      <td class="gt_row gt_right">174</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">435</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">320</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">619</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">97</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">436</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">168</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">164</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">1130</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">137</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">74</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">17</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Delicious Duo</td>
      <td class="gt_row gt_left">2020-08-01</td>
      <td class="gt_row gt_right">182</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">1175</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">509</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">857</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">336</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">156</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">211</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-02</td>
      <td class="gt_row gt_right">102</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-03</td>
      <td class="gt_row gt_right">308</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-03</td>
      <td class="gt_row gt_right">252</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-03</td>
      <td class="gt_row gt_right">1155</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-03</td>
      <td class="gt_row gt_right">572</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-03</td>
      <td class="gt_row gt_right">65</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-08-03</td>
      <td class="gt_row gt_right">383</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">387</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">231</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">73</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">339</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">118</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">270</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">162</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">56</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">192</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">195</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">81</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">87</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">thai</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">24</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">44</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-04</td>
      <td class="gt_row gt_right">427</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">563</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">290</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">781</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">223</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">382</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">217</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">67</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">41</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-05</td>
      <td class="gt_row gt_right">234</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">393</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">307</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">175</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">303</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">127</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">98</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">164</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Dragon</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">442</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">purple</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">317</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-08-06</td>
      <td class="gt_row gt_right">439</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">359</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">356</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">233</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">364</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">1045</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">562</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">292</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">1219</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">1327</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">255</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-07</td>
      <td class="gt_row gt_right">19</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">162</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">81</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">564</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">184</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Chinese Red Noodle</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">108</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">122</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">1697</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">545</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">445</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-08-08</td>
      <td class="gt_row gt_right">305</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">179</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">591</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">1102</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">308</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">54</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">64</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">443</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">118</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-08-09</td>
      <td class="gt_row gt_right">302</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">13</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">272</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">purple</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">168</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">216</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">241</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">309</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-08-10</td>
      <td class="gt_row gt_right">221</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">731</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">302</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">307</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">160</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">755</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">1029</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Chinese Red Noodle</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">78</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">245</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">218</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">802</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">354</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">359</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">506</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">92</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">109</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_left">2020-08-11</td>
      <td class="gt_row gt_right">330</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-12</td>
      <td class="gt_row gt_right">73</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">1774</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">468</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">122</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">421</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">332</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">727</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">642</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">413</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Chinese Red Noodle</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">65</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">599</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">12</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">198</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">308</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">517</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">2209</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_left">2020-08-13</td>
      <td class="gt_row gt_right">2476</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">1564</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">80</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">711</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">238</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">525</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">181</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">266</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">490</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">126</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-14</td>
      <td class="gt_row gt_right">371</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Golden Bantam</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">383</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">351</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">859</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">25</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">137</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">71</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-15</td>
      <td class="gt_row gt_right">56</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">477</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">328</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">45</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">543</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">599</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">560</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">291</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">238</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">397</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">660</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-16</td>
      <td class="gt_row gt_right">693</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">364</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">305</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">588</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">764</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">436</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">306</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">350</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Chinese Red Noodle</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">105</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">30</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">67</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Golden Bantam</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">344</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-08-17</td>
      <td class="gt_row gt_right">173</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">27</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">126</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">112</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">1151</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">225</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">2888</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">608</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">136</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">148</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">317</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">105</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">271</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">39</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">87</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">233</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">2020-08-18</td>
      <td class="gt_row gt_right">527</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">purple</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">323</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">278</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">thai</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">31</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">872</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">579</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">615</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">997</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">335</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">264</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">451</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-19</td>
      <td class="gt_row gt_right">306</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">99</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">70</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">333</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">483</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">632</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">360</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">230</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">344</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">1010</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">328</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">287</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">322</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">493</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">252</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">70</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">834</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-08-20</td>
      <td class="gt_row gt_right">113</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">1122</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">34</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">509</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">1601</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">842</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">1538</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">428</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">243</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">330</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">997</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">265</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">562</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Dragon</td>
      <td class="gt_row gt_left">2020-08-21</td>
      <td class="gt_row gt_right">457</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">1542</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">801</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">436</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">747</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">1573</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">704</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">446</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">269</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">661</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">2436</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-23</td>
      <td class="gt_row gt_right">111</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-24</td>
      <td class="gt_row gt_right">134</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-08-24</td>
      <td class="gt_row gt_right">115</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-24</td>
      <td class="gt_row gt_right">75</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-08-24</td>
      <td class="gt_row gt_right">117</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">578</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">871</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">115</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">629</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">186</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">320</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">488</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">506</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">920</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">179</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">1400</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">993</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-25</td>
      <td class="gt_row gt_right">1026</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">1886</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">666</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">1042</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">593</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">216</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">309</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">497</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">261</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">819</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_left">2020-08-26</td>
      <td class="gt_row gt_right">1607</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-27</td>
      <td class="gt_row gt_right">14</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-08-28</td>
      <td class="gt_row gt_right">29</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-08-28</td>
      <td class="gt_row gt_right">3244</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-08-28</td>
      <td class="gt_row gt_right">85</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">24</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">289</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">380</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">737</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">1033</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">1097</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">483</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">627</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">352</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">purple</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">262</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">716</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">888</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">566</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">greens</td>
      <td class="gt_row gt_left">2020-08-29</td>
      <td class="gt_row gt_right">169</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">861</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">460</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">2934</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">599</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">155</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">822</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">589</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">393</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">752</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-08-30</td>
      <td class="gt_row gt_right">833</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">2831</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">1953</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">160</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">4758</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">2342</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">3227</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">5150</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">Cinderella's Carraige</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">7350</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">805</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">178</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">201</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">1537</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">773</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-09-01</td>
      <td class="gt_row gt_right">1202</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_left">2020-09-02</td>
      <td class="gt_row gt_right">798</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-09-02</td>
      <td class="gt_row gt_right">370</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-09-02</td>
      <td class="gt_row gt_right">43</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-09-02</td>
      <td class="gt_row gt_right">60</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-03</td>
      <td class="gt_row gt_right">1131</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-03</td>
      <td class="gt_row gt_right">610</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-09-03</td>
      <td class="gt_row gt_right">1265</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-09-03</td>
      <td class="gt_row gt_right">102</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">2160</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">2899</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">442</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">1234</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">1178</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">255</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">430</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Delicious Duo</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">33</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">256</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-09-04</td>
      <td class="gt_row gt_right">58</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_left">2020-09-05</td>
      <td class="gt_row gt_right">214</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">2020-09-05</td>
      <td class="gt_row gt_right">1644</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-06</td>
      <td class="gt_row gt_right">2377</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-09-06</td>
      <td class="gt_row gt_right">710</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-06</td>
      <td class="gt_row gt_right">1317</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-09-06</td>
      <td class="gt_row gt_right">1649</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-06</td>
      <td class="gt_row gt_right">615</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-09-07</td>
      <td class="gt_row gt_right">3284</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-09-08</td>
      <td class="gt_row gt_right">1300</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-09-09</td>
      <td class="gt_row gt_right">843</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Main Crop Bravado</td>
      <td class="gt_row gt_left">2020-09-09</td>
      <td class="gt_row gt_right">102</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-09-09</td>
      <td class="gt_row gt_right">228</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">692</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">674</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">1392</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">316</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">754</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">413</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-10</td>
      <td class="gt_row gt_right">509</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-09-12</td>
      <td class="gt_row gt_right">108</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-15</td>
      <td class="gt_row gt_right">258</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-15</td>
      <td class="gt_row gt_right">725</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">Russet</td>
      <td class="gt_row gt_left">2020-09-16</td>
      <td class="gt_row gt_right">629</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Main Crop Bravado</td>
      <td class="gt_row gt_left">2020-09-16</td>
      <td class="gt_row gt_right">219</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-09-16</td>
      <td class="gt_row gt_right">8</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-09-17</td>
      <td class="gt_row gt_right">160</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-09-17</td>
      <td class="gt_row gt_right">168</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kohlrabi</td>
      <td class="gt_row gt_left">Crispy Colors Duo</td>
      <td class="gt_row gt_left">2020-09-17</td>
      <td class="gt_row gt_right">191</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-17</td>
      <td class="gt_row gt_right">212</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-09-18</td>
      <td class="gt_row gt_right">714</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-18</td>
      <td class="gt_row gt_right">228</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-09-18</td>
      <td class="gt_row gt_right">670</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-09-18</td>
      <td class="gt_row gt_right">1052</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-09-18</td>
      <td class="gt_row gt_right">1631</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_left">2020-09-18</td>
      <td class="gt_row gt_right">137</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2934</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">304</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1058</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">307</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">397</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">537</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">314</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">494</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">484</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">454</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">480</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">252</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">294</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">437</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1834</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1655</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1927</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1558</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1183</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1178</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">706</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1686</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1785</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1923</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2120</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2325</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1172</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">Cinderella's Carraige</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1311</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">Cinderella's Carraige</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">6250</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1154</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1208</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2882</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2689</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">3441</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">7050</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1109</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1028</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1131</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1302</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1570</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1359</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1608</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2277</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">1743</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-09-19</td>
      <td class="gt_row gt_right">2931</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-09-20</td>
      <td class="gt_row gt_right">163</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-09-21</td>
      <td class="gt_row gt_right">714</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-21</td>
      <td class="gt_row gt_right">95</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">477</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">2738</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">236</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">1823</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">819</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">2006</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">659</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">1239</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">1978</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">28</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">24</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Main Crop Bravado</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">75</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-09-25</td>
      <td class="gt_row gt_right">84</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">apple</td>
      <td class="gt_row gt_left">unknown</td>
      <td class="gt_row gt_left">2020-09-26</td>
      <td class="gt_row gt_right">156</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-09-26</td>
      <td class="gt_row gt_right">95</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">94</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">81</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">139</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Main Crop Bravado</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">134</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Dragon</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">883</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">449</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-09-27</td>
      <td class="gt_row gt_right">232</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">88</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">92</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">1447</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">494</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">678</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">70</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-09-30</td>
      <td class="gt_row gt_right">327</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-10-01</td>
      <td class="gt_row gt_right">127</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">Russet</td>
      <td class="gt_row gt_left">2020-10-02</td>
      <td class="gt_row gt_right">1596</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-10-02</td>
      <td class="gt_row gt_right">101</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-10-02</td>
      <td class="gt_row gt_right">145</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-10-03</td>
      <td class="gt_row gt_right">252</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-10-03</td>
      <td class="gt_row gt_right">213</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_left">2020-10-03</td>
      <td class="gt_row gt_right">346</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-10-04</td>
      <td class="gt_row gt_right">39</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">254</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">363</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">715</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">272</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">64</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">17</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-10-07</td>
      <td class="gt_row gt_right">169</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">Russet</td>
      <td class="gt_row gt_left">2020-10-08</td>
      <td class="gt_row gt_right">372</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-10-08</td>
      <td class="gt_row gt_right">436</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_left">2020-10-10</td>
      <td class="gt_row gt_right">1377</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-10-10</td>
      <td class="gt_row gt_right">1977</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-10-10</td>
      <td class="gt_row gt_right">258</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-10-10</td>
      <td class="gt_row gt_right">23</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">2478</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">375</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">316</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">898</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">526</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">386</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">230</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">84</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">119</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">144</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Main Crop Bravado</td>
      <td class="gt_row gt_left">2020-10-11</td>
      <td class="gt_row gt_right">437</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">1031</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">2322</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">296</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">312</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">709</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">2143</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">1950</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">1291</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">1627</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">4372</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">5000</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">2990</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">1300</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">2710</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_left">2020-10-12</td>
      <td class="gt_row gt_right">137</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">859</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">791</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">1175</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">418</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">484</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">219</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">646</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">2838</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">1500</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">1023</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">328</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">175</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_left">2020-10-14</td>
      <td class="gt_row gt_right">89</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">5700</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">3600</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">Russet</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">1527</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">272</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">red</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">1718</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">purple</td>
      <td class="gt_row gt_left">2020-10-15</td>
      <td class="gt_row gt_right">295</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-16</td>
      <td class="gt_row gt_right">883</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-16</td>
      <td class="gt_row gt_right">740</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">310</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">932</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">1096</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">1101</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">red</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">293</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">183</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Delicious Duo</td>
      <td class="gt_row gt_left">2020-10-17</td>
      <td class="gt_row gt_right">77</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">2001</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">673</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">144</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">366</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">1393</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">903</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">419</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">1026</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">1350</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">297</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">52</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_left">2020-10-18</td>
      <td class="gt_row gt_right">114</td>
      <td class="gt_row gt_left">grams</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="5">Source: Prof. Lendway - https://github.com/llendway/gardenR</td>
    </tr>
  </tfoot>
  
</table></div>
```
  
  6. Use `patchwork` operators and functions to combine at least two graphs using your project data or `garden_harvest` data if your project data aren't read.
  

```r
garden_cum_harvest <- garden_harvest %>% 
  group_by(vegetable) %>% 
  summarise(cum_weight = sum(weight))

g1<- garden_planting %>% 
  filter(!(is.na(number_seeds_planted))) %>% 
  group_by(vegetable) %>% 
  summarise(tot_seeds = sum(number_seeds_planted)) %>% 
  right_join(garden_cum_harvest, by = c("vegetable" = "vegetable")) %>%  
  ggplot(aes(x = cum_weight * 0.0022, y = fct_reorder(str_to_title(vegetable), cum_weight), fill = tot_seeds), na.rm = TRUE)+
  geom_col()+
  scale_color_viridis_c() +
  labs(y="", title = "Total weight of harvest", x = " Cumulative harvest (lb)", caption = " Seed planting data was not collected for rows that are not colored, Source: Lisa Lendway", fill = "Total Seeds Planted") + 
  theme(legend.position = c(0.85,0.3))

g2<- garden_planting %>% 
  #filter(vegetable == c(top_10_veg)) %>% 

  group_by(vegetable, date) %>% 
  summarise(tot_seeds2 = sum(number_seeds_planted)) %>% 
  mutate(cum_seeds = cumsum(tot_seeds2)) %>% 
  ggplot(aes(x = date, y = cum_seeds, color = vegetable))+
  geom_line() +
  labs(title = " seed planting pattern", y = " seeds planted", x = "date", caption = "Source: Lisa Lendway")

g1 | g2
```

![](06_exercises_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

**DID YOU REMEMBER TO UNCOMMENT THE OPTIONS AT THE TOP?**
