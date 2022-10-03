library(jsonstat)
library(rjstat)
library(rjson)
library(tidyjson)
library(httr)
library(tidyverse)
library(corr)
library(ggplot2)
library(janitor)

data <- "https://data.ssb.no/api/v0/no/table/11155/"

data_frame <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "20-64",
          "15-24"
        ]
      }
    },
    {
      "code": "UtdNivaa",
      "selection": {
        "filter": "item",
        "values": [
          "TOT"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "ArbLedigProsent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2020"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

ssb_df <- POST(data, body = data_frame, encode = "json", verbose())

ssb_df <- fromJSONstat(content(ssb_df, "text")) %>%
  as_tibble()




ggplot(ssb_df, aes(år, value, group = alder))+
  geom_col(position = "identity", aes(fill=alder), alpha =.5)+
  scale_fill_manual(values = c("lightpink1", "hotpink3"))+
labs(title = "Arbeidsledighet i %", subtitle  = "blant ungdommer og voksne", x = "År" , y ="Arbeidsledighet i %")+
  theme(panel.background = element_rect(fill = "white", color = "white"), plot.title = element_text(size = 10), plot.subtitle = element_text(size =8), axis.title = element_text(size = 8))
