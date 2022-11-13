library(PxWebApiData)
library(dplyr)
library(tidyverse)
library(jsonstat)
library(rjstat)
library(httr)
library(janitor)

url <- "https://data.ssb.no/api/v0/no/table/12441/"

data <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99"
        ]
      }
    },
    {
      "code": "Sykefraver2",
      "selection": {
        "filter": "item",
        "values": [
          "Alt"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

ssb_data <- POST(url, body = data, encode = "json", verbose())

syke_data <- fromJSONstat(content(ssb_data, "text"))
as_tibble(syke_data)


url1 <- "https://data.ssb.no/api/v0/no/table/05111/"

arb_data <- '{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

ssb_data1 <- POST(url1, body = arb_data, encode = "json", verbose())


arbei_data <- fromJSONstat(content(ssb_data1, "text"))
as_tibble(arbei_data)

data <- cbind(syke_data,arbei_data)

data_kvinner <- data %>% 
  clean_names("upper_camel")%>%
  filter(Kjonn == "Kvinner" & Kjonn_2 == "Kvinner")

data_menn <- data %>%
  clean_names("upper_camel") %>%
  filter(Kjonn == "Menn", Kjonn_2 == "Menn")


farger <- c("Arbeidsledig" = "plum", "Sykefravær" = "mediumaquamarine")

ggplot(data_kvinner, aes(x = Ar, group = Kjonn)) + 
  geom_line(aes(y= Value, color = "Sykefravær")) + 
  geom_line(aes(y= Value_2, color = "Arbeidsledig"))+
  scale_y_continuous(name = "Sykefraværprosent", 
    sec.axis = sec_axis(~ . * 1, name = "Arbeidsledighet"))+
  scale_color_manual(values = farger)+
  labs(title="Kvinner",
       color="Status")+
  theme_minimal()
  


ggplot(data_menn, aes(Ar, group = Kjonn)) +
  geom_line(aes(y = Value, colour = "Sykefravær")) +
  geom_line(aes(y = Value_2, colour = "Arbeidsledig")) +
  scale_y_continuous(name = "Sykefraværprosent", 
                     sec.axis = sec_axis(~ . * 1, name = "Arbeidsledighet"))+
  scale_color_manual(values = farger)+
  labs(title = "Menn",
       color = "Status")+
  theme_minimal()

