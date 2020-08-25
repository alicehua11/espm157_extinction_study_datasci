The Sixth Mass Extinctions
================
Alice Hua, Chancey Quam

## Mass Extinctions Module

*Are we experiencing the sixth great extinction?*

What is the current pace of vetebrates extinction rates? Is it
accelerating? How does it compare to background extinction rates? How do
we account for NA values in the data?

## Background

  - [Section Intro Video](https://youtu.be/QsH6ytm89GI)
  - [Ceballos et al (2015)](http://doi.org/10.1126/sciadv.1400253)

Our focal task will be to reproduce the result from Ceballos and
colleagues showing the recent increase in extinction rates relative to
the background rate, as well as looking at different results of
extinction rates from including and excluding NA values.

![](https://espm-157.carlboettiger.info/img/extinctions.jpg)

## Computational Topics

  - Accessing data from a RESTful API
  - Error handling
  - JSON data format
  - Regular expressions
  - Working with missing values

## Additional references:

  - <http://www.hhmi.org/biointeractive/biodiversity-age-humans> (Video)
  - [Barnosky et al. (2011)](http://doi.org/10.1038/nature09678)
  - [Pimm et al (2014)](http://doi.org/10.1126/science.1246752)
  - [Sandom et al (2014)](http://dx.doi.org/10.1098/rspb.2013.3254)

### Data about the species:

Here we use RESTful API to read in data about all species evaluated by
the IUCN Red List.

``` r
species_ap <- "https://apiv3.iucnredlist.org/api/v3/species"
page <- "/page/"
page_number <- 0:10
query <- "?token="
token <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"

#write a function to get http, clue everything together to make a url that iterates over the vector of page_number
all_pages <- paste0(species_ap, page, page_number, query, token)
```

We will get the results for all 10 pages, each has 10,000 records of all
species published.

``` r
#apply map to all_pages a vector
all_results <- map(all_pages, GET)
```

Let’s look at the names of the data.

``` r
row <- all_results[[1]] %>% content() %>% getElement("result") %>% getElement(1)
names(row)
```

``` 
 [1] "taxonid"         "kingdom_name"    "phylum_name"    
 [4] "class_name"      "order_name"      "family_name"    
 [7] "genus_name"      "scientific_name" "infra_rank"     
[10] "infra_name"      "population"      "category"       
```

Using the names we got above, here we turn the data into “rectangular”
dataframe. According to the paper by Ceballos on species extinctions,
both extinct species and extinct in the wild species were selected to
make their final graph. To reproduce their result, we are filtering to
have only species under these two categories.

``` r
row_to_tibble <- function(row){
  tibble(scientific_name = row$scientific_name, 
         category = row$category, 
         phylum = row$phylum_name,
         class = row$class_name)
}

get_result <- function(x){
  x %>% content() %>% getElement("result") %>% 
  map_dfr(row_to_tibble)
}
```

``` r
all_species <- all_results  %>% map_dfr(get_result) %>% filter(category=="EX"| category=="EW")
all_species
```

    # A tibble: 989 x 4
       scientific_name            category phylum     class         
       <chr>                      <chr>    <chr>      <chr>         
     1 Mirogrex hulensis          EX       CHORDATA   ACTINOPTERYGII
     2 Acanthametropus pecatonica EX       ARTHROPODA INSECTA       
     3 Achatinella abbreviata     EX       MOLLUSCA   GASTROPODA    
     4 Achatinella buddii         EX       MOLLUSCA   GASTROPODA    
     5 Achatinella caesia         EX       MOLLUSCA   GASTROPODA    
     6 Achatinella casta          EX       MOLLUSCA   GASTROPODA    
     7 Achatinella decora         EX       MOLLUSCA   GASTROPODA    
     8 Achatinella dimorpha       EX       MOLLUSCA   GASTROPODA    
     9 Achatinella elegans        EX       MOLLUSCA   GASTROPODA    
    10 Achatinella juddii         EX       MOLLUSCA   GASTROPODA    
    # ... with 979 more rows

We can see that 989 species went extinct or extinct in the wild. Let’s
see the unique phylums of those species. For the purpose of
reproducibility, we will select only the species of Chordata phylum
because they are vertebrates.

``` r
unique(all_species$phylum)
```

``` 
[1] "CHORDATA"        "ARTHROPODA"      "MOLLUSCA"        "NEMERTINA"      
[5] "PLATYHELMINTHES" "TRACHEOPHYTA"    "BRYOPHYTA"       "ANNELIDA"       
[9] "RHODOPHYTA"     
```

### Data about the species extinction dates:

Lets get the extinction dates for these species. The data is store in
the narrative text for each species. Again, we are setting up the urls
to be called in the RESTful API.

``` r
all_extinct <- all_species %>% pull(scientific_name)

base <- "https://apiv3.iucnredlist.org"
narrative <- "/api/v3/species/narrative/"
name_group1 <- all_extinct
query <- "?token="
token <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
url <- paste0(base, narrative, name_group1, query, token)
```

We have already called the urls for all the extinct species (this will
take awhile) and saved it as an R object RDS. The code chunk below will
load the object if the file exists or it will download the data if the
file doesn’t exist. Then this object consists narratives of all extinct
and extinct in the wild species is read into all\_data.

``` r
if (!file.exists("massive_resp.rds")){
  resp <- map(url, GET)
  saveRDS(resp, "massive_resp.rds")
} else {
  all_data <- readRDS("massive_resp.rds")
}
```

We will need to get the rationale section for each species in order to
find out what dates they went extinct. Here are the narratives of the
species, the narratives that are NULL are stored as NA.

``` r
map_chr_na <- possibly(map_chr, as.character(NA))

narrative <- all_data %>% map(content) %>% map("result") %>% 
  map(map_chr_na, "rationale")
```

Lets extract the extinction date for species that have dates in their
narrative. We created a custom function to extract any 4 digit number
from the rationales. We then unlist them as they are nested in lists.

``` r
extinc_year <- map(narrative, function(x){
  str_extract_all(x, "\\d{4}") %>% 
  unlist()
}) 
```

We will bin the years for all species into the specific bins for our
final graph. Since we are using bins of an 100-year interval. It is
reasonable to put them in these bins. For rationales of species that
contain more than one year, if the years are within the 100-year
interval, we will collapse them into the same bin. If they are not
within the 100-year interval however, we will select the latest year and
put it in the corresponding bin. We chose to do so because we observed
that the narratives have a tendency to list the last seen date as the
date of approximate extinction. Lastly, if there is no narrative, or
Null values, we will give them NA as values.

``` r
bin_year <- function(x) {
  x = as.integer(x)
    case_when(
      between(x, 1500, 1600) ~ "1600",
      between(x, 1600, 1700) ~ "1700",
      between(x, 1700, 1800) ~ "1800",
      between(x, 1800, 1900) ~ "1900",
      between(x, 1900, 2020) ~ "2000",
      !between(x, 1500, 2020) ~ as.character("not a year"),
      length(x) < 1 ~ as.character(NA)) %>%
      unique() %>%
      max()
}
```

### Method 1: Highest possible extinction rates by including dates with NA values into the first bin:

Here we apply our binning function to all the years that we extracted
from the species rationale to come up an extinction date for each
species. It is important to note that we assigned all null narratives
which were replaced to NAs by our binning function earlier to have 1600
as its values. This is because we will calculate the cumulative sum for
each group of species, the cumulative sum would start at the beginning
and continue to the latest year bin, which means that all the species of
null dates will get counted from the beginning of our time series and
continued to be counted to the end. This would be our maximum possible
species that have gone extinct.

``` r
all_year <- 
  extinc_year %>% 
  map(possibly(bin_year, NA_real_)) %>%
  as.integer() %>% 
  replace_na(1600)
```

Here we are adding the extinction dates (including dates of NA values
that are binned into 1600) for 989 species calculated above to our table
of 989 species consists of EX - Extinct and EW - Extinct in the Wild
species.

``` r
all_species_with_year <- all_species %>%
  mutate(extinct_year = all_year)
all_species_with_year
```

    # A tibble: 989 x 5
       scientific_name            category phylum    class         extinct_year
       <chr>                      <chr>    <chr>     <chr>                <dbl>
     1 Mirogrex hulensis          EX       CHORDATA  ACTINOPTERYG~         2000
     2 Acanthametropus pecatonica EX       ARTHROPO~ INSECTA               1600
     3 Achatinella abbreviata     EX       MOLLUSCA  GASTROPODA            1600
     4 Achatinella buddii         EX       MOLLUSCA  GASTROPODA            1600
     5 Achatinella caesia         EX       MOLLUSCA  GASTROPODA            1600
     6 Achatinella casta          EX       MOLLUSCA  GASTROPODA            1600
     7 Achatinella decora         EX       MOLLUSCA  GASTROPODA            1600
     8 Achatinella dimorpha       EX       MOLLUSCA  GASTROPODA            1600
     9 Achatinella elegans        EX       MOLLUSCA  GASTROPODA            1600
    10 Achatinella juddii         EX       MOLLUSCA  GASTROPODA            1600
    # ... with 979 more rows

To reproduce a similar result to Ceballos’s final graph, we did the
following - We sorted the species with extinction dates into 4 different
groups: Mammals, Birds, Vertebrates and Other Vertebrates. We included
Mammals, Birds and Amphibians in the Vertebrates group. For the Other
Vertebrates group, we excluded Mammals and Birds and included all
species in the Chordata phylum, which includes all other vertebrates.

``` r
verte <- c("MAMMALIA","AVES","AMPHIBIA")

birds <- all_species_with_year %>% 
  filter(class == "AVES") %>% mutate(group = "Birds")

mammals <- all_species_with_year %>% 
  filter(class == "MAMMALIA") %>% mutate(group = "Mammals")

vertebrate <- all_species_with_year %>% 
  filter(class %in% verte) %>% mutate(group = "Vertebrates")

all_others <- all_species_with_year %>% 
  filter(phylum == "CHORDATA") %>% 
  filter(!class %in% c("MAMMALIA", "AVES")) %>% 
  mutate(group = "Other Vertebrates")
```

Here, we calculate the cumulative sums for each group and create a new
table with all groups combined.

``` r
count_sum <- function(table) {
  new_table <- table %>%
  group_by(extinct_year, group)%>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(cum_sum = cumsum(count))
  return(new_table)
}

birds_sum <- count_sum(birds)
mam_sum <- count_sum(mammals)
vert_sum <- count_sum(vertebrate)
other_sum <- count_sum(all_others)

all_types <- rbind(birds_sum, mam_sum, vert_sum, other_sum)
all_types
```

    # A tibble: 19 x 4
       extinct_year group             count cum_sum
              <dbl> <chr>             <int>   <int>
     1         1600 Birds                19      19
     2         1700 Birds                15      34
     3         1800 Birds                22      56
     4         1900 Birds                43      99
     5         2000 Birds                62     161
     6         1600 Mammals              26      26
     7         1700 Mammals               3      29
     8         1800 Mammals               2      31
     9         1900 Mammals              14      45
    10         2000 Mammals              50      95
    11         1600 Vertebrates          69      69
    12         1700 Vertebrates          18      87
    13         1800 Vertebrates          24     111
    14         1900 Vertebrates          57     168
    15         2000 Vertebrates         122     290
    16         1600 Other Vertebrates    60      60
    17         1800 Other Vertebrates     3      63
    18         1900 Other Vertebrates     2      65
    19         2000 Other Vertebrates    82     147

Now, we also need to get the total of all species (with and without
being classified as extinct or extinct in the wild) for each group, this
will be our denominator for the cumulative percentage calculation at the
end.

``` r
all_species_eval <-all_results  %>% map_dfr(get_result)
```

``` r
birds_total  <- birds <- all_species_eval %>% 
  filter(class == "AVES") %>% mutate(group = "Birds")

mammal_total <- mammals <- all_species_eval %>% 
  filter(class == "MAMMALIA") %>% mutate(group = "Mammals")

vert_total <- vertebrate <- all_species_eval %>% 
  filter(class %in% verte) %>% mutate(group = "Vertebrates")

other_total <- all_others <- all_species_eval %>% 
  filter(phylum == "CHORDATA") %>% 
  filter(!class %in% c("MAMMALIA", "AVES")) %>% 
  mutate(group = "Other Vertebrates")
```

In order to arrive at the fractions of cumulative extinctions as
percentages of IUCN-evaluated species, we need to divide the total of a
species of each group in each bin to the total number of that species.
To get each group total count, we made a new table to have only total
counts for 4 groups (birds, mammals, vertebrate, other vertebrates). We
joined this table to our current master extict table to create another
more comprehensive table where the total counts and species counts per
bin year, so that we can derive the cumulative percentages.

``` r
group_total <- data.frame(group = 
                                  c("Birds", "Mammals", "Vertebrates", "Other Vertebrates"),                                    
                          sum_species = 
                                  c(nrow(birds_total), nrow(mammal_total),                                                      nrow(vert_total),nrow(other_total)))

master_ex_table <- all_types %>%
  left_join(group_total) %>%
  mutate(perc_ex_withNA = 100*(cum_sum/ sum_species))
```

    Joining, by = "group"

    Warning: Column `group` joining character vector and factor, coercing into
    character vector

``` r
master_ex_table
```

    # A tibble: 19 x 6
       extinct_year group             count cum_sum sum_species perc_ex_withNA
              <dbl> <chr>             <int>   <int>       <int>          <dbl>
     1         1600 Birds                19      19       11126          0.171
     2         1700 Birds                15      34       11126          0.306
     3         1800 Birds                22      56       11126          0.503
     4         1900 Birds                43      99       11126          0.890
     5         2000 Birds                62     161       11126          1.45 
     6         1600 Mammals              26      26        6237          0.417
     7         1700 Mammals               3      29        6237          0.465
     8         1800 Mammals               2      31        6237          0.497
     9         1900 Mammals              14      45        6237          0.722
    10         2000 Mammals              50      95        6237          1.52 
    11         1600 Vertebrates          69      69       24135          0.286
    12         1700 Vertebrates          18      87       24135          0.360
    13         1800 Vertebrates          24     111       24135          0.460
    14         1900 Vertebrates          57     168       24135          0.696
    15         2000 Vertebrates         122     290       24135          1.20 
    16         1600 Other Vertebrates    60      60       33056          0.182
    17         1800 Other Vertebrates     3      63       33056          0.191
    18         1900 Other Vertebrates     2      65       33056          0.197
    19         2000 Other Vertebrates    82     147       33056          0.445

Lets’ visualize these maximum possible extinction rates that we
calculated by including NA dates in the year bin of 1600.

``` r
master_ex_table %>% ggplot(aes(x = factor(extinct_year), group = group, color = group)) +
  geom_line(aes(y = perc_ex_withNA), lwd = 1.5) +
  labs(title = "Cumulative Vertebrate Species as EX or EW by the IUCN 2019",
       y = 'Cumulative extinctions as % of UCN-evaluated species',
       x = "Time interval") + 
  scale_x_discrete(labels=c("1500-1600", "1600-1700", "1700-1800", "1800-1900", "1900-2000")) +
  scale_y_continuous(breaks = seq(0,2, by=0.2)) +
  theme_classic() +
  theme(text = element_text(size=10))
```

![](extinction-assignment_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Method 2: Lowest possible extinction dates by dropping for dates with NA values:

Comparing to the Fig. 1 graph in Ceballos’s paper, we see that the graph
was made using IUCN data as of 2012. It has been almost 8 years since
then. We therefore suspect that our graph is reflecting the changes in
the extinction rates. Particularly, we see an elevated rate of
extinction for the vertebrates group. However, we can only say that this
result shows the maximum possible veterbrate extinctions with N.A values
for extinction dates accounted for. Now, We want to reproduce the data
without the NA values to see the difference.

``` r
all_year_noNA <- 
  extinc_year %>% 
  map(possibly(bin_year, NA_real_)) %>%
  as.integer()

all_species_noNA <- all_species %>%
  mutate(extinct_year = all_year_noNA) %>%
  drop_na()
all_species_noNA
```

    # A tibble: 548 x 5
       scientific_name               category phylum    class      extinct_year
       <chr>                         <chr>    <chr>     <chr>             <int>
     1 Mirogrex hulensis             EX       CHORDATA  ACTINOPTE~         2000
     2 Acipenser nudiventris         EX       CHORDATA  ACTINOPTE~         2000
     3 Afrocyclops pauliani          EX       ARTHROPO~ MAXILLOPO~         2000
     4 Alasmidonta robusta           EX       MOLLUSCA  BIVALVIA           2000
     5 Alasmidonta wrightiana        EX       MOLLUSCA  BIVALVIA           2000
     6 Alasmidonta mccordi           EX       MOLLUSCA  BIVALVIA           2000
     7 Alburnus akili                EX       CHORDATA  ACTINOPTE~         2000
     8 Alcelaphus buselaphus ssp. b~ EX       CHORDATA  MAMMALIA           2000
     9 Pholidoscelis cineraceus      EX       CHORDATA  REPTILIA           2000
    10 Aphanius splendens            EX       CHORDATA  ACTINOPTE~         2000
    # ... with 538 more rows

Our table without the rows of NA values for extinct year now has only
548 species. This number is only around 58% of all extinct species.
Lets’ similarly filter this no NA dataframe for the same vertebrate
groups of species that we are interested in.

``` r
birds_noNa <- all_species_noNA %>% 
  filter(class == "AVES") %>% mutate(group = "Birds")

mammals_noNa <- all_species_noNA %>% 
  filter(class == "MAMMALIA") %>% mutate(group = "Mammals")

vertebrate_noNa <- all_species_noNA %>% 
  filter(class %in% verte) %>% mutate(group = "Vertebrates")

all_others_noNa <- all_species_noNA %>% 
  filter(phylum == "CHORDATA") %>% 
  filter(!class %in% c("MAMMALIA", "AVES")) %>% 
  mutate(group = "Other Vertebrates")

cat("The total species of all groups without NA for extinction dates  combined is:", as.character(sum(nrow(birds_noNa) + nrow(mammals_noNa) + nrow(vertebrate_noNa) + nrow(all_others_noNa))))
```

    The total species of all groups without NA for extinction dates  combined is: 536

We calculate the cumulative sums for each group as we did for the
earlier data with NA values accounted for. We will add our results into
the master table earlier where extinction percentages included NA values
in the cummulative sum.

``` r
birds_sum_noNa <- count_sum(birds_noNa)
mam_sum_noNa <- count_sum(mammals_noNa)
vert_sum_noNa <- count_sum(vertebrate_noNa)
other_sum_noNa <- count_sum(all_others_noNa)

all_types_noNa <- rbind(birds_sum_noNa, mam_sum_noNa, vert_sum_noNa, other_sum_noNa)

master_noNA <- all_types_noNa %>%
  left_join(group_total) %>%
  mutate(perc_extinct_noNA = 100*(cum_sum/ sum_species))
```

    Joining, by = "group"

    Warning: Column `group` joining character vector and factor, coercing into
    character vector

``` r
master_ex_table <- master_ex_table %>%
  select(extinct_year, group, sum_species, perc_ex_withNA) %>%
  mutate(perc_ex_noNA = master_noNA$perc_extinct_noNA)
  
master_ex_table
```

``` 
# A tibble: 19 x 5
   extinct_year group             sum_species perc_ex_withNA perc_ex_noNA
          <dbl> <chr>                   <int>          <dbl>        <dbl>
 1         1600 Birds                   11126          0.171      0.0539 
 2         1700 Birds                   11126          0.306      0.189  
 3         1800 Birds                   11126          0.503      0.386  
 4         1900 Birds                   11126          0.890      0.773  
 5         2000 Birds                   11126          1.45       1.33   
 6         1600 Mammals                  6237          0.417      0.0160 
 7         1700 Mammals                  6237          0.465      0.0641 
 8         1800 Mammals                  6237          0.497      0.0962 
 9         1900 Mammals                  6237          0.722      0.321  
10         2000 Mammals                  6237          1.52       1.12   
11         1600 Vertebrates             24135          0.286      0.0290 
12         1700 Vertebrates             24135          0.360      0.104  
13         1800 Vertebrates             24135          0.460      0.203  
14         1900 Vertebrates             24135          0.696      0.439  
15         2000 Vertebrates             24135          1.20       0.945  
16         1600 Other Vertebrates       33056          0.182      0.00908
17         1800 Other Vertebrates       33056          0.191      0.0182 
18         1900 Other Vertebrates       33056          0.197      0.0242 
19         2000 Other Vertebrates       33056          0.445      0.272  
```

Lets’ visualize the extinction percentages that we calculated excluding
extinction records that had NA values for extinction dates.

``` r
master_ex_table %>% ggplot(aes(x = factor(extinct_year), group = group, color = group)) +
  geom_line(aes(y = perc_ex_withNA), lwd = 1.5) +
  geom_line(aes(y = perc_ex_noNA), linetype = "dashed") +
  labs(title = "Cumulative Vertebrate Species as EX or EW by the IUCN 2019",
       y = 'Cumulative extinctions as % of UCN-evaluated species',
       x = "Time interval") + 
  scale_x_discrete(labels=c("1500-1600", "1600-1700", "1700-1800", "1800-1900", "1900-2000")) +
  scale_y_continuous(breaks = seq(0,2, by=0.2)) +
  theme_classic() +
  theme(text = element_text(size=10))
```

![](extinction-assignment_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

The graph above shows the highest and lowest possible extinction
percentages for all veterbrates. The solid lines represent the highest
values which were generated by including NAs in our initial year bin.
The dashed lines represent the lowest values which were generated by
excluding species with NAs as extinction dates. We think that the real
percentages of vertebrates extinction are somewhere in between these two
lines for each group of species. To come up with a line that is in
between the highest and lowest lines, we will interpolate the
percentages for each bin by projecting the same proportions of species
in each bin in our previous lowest possible data (without NA values).
Then, we will add the counts derived from the projected proportions of
NAs to the lowest possible species count, then find the cumulative
percentages by dividing by the common denominator we have been using,
total species evaluated.

### Method 3: The uncertainty line between highest and lowest extinction rates by interpolating NA values:

First, we need to know how many NA values there are total. We are also
interested in how many there are per group of species of interest.
Notice here that we are double counting the NA values for birds and
mammals because there were included in the vertebrates group as well.

``` r
species_test <- all_species %>%
  mutate(extinct_year = all_year_noNA) %>%
  select(phylum, class, extinct_year) %>%
  mutate(NA_year = is.na(extinct_year))

birds_test <- species_test %>% 
  filter(class == "AVES") %>% mutate(group = "Birds")

mammals_test <- species_test %>% 
  filter(class == "MAMMALIA") %>% mutate(group = "Mammals")

vertebrate_test <- species_test %>% 
  filter(class %in% verte) %>% mutate(group = "Vertebrates")

all_others_test <- species_test %>% 
  filter(phylum == "CHORDATA") %>% 
  filter(!class %in% c("MAMMALIA", "AVES")) %>% 
  mutate(group = "Other Vertebrates")

cat("The total NA values for birds are:", sum(birds_test$NA_year),"\n",
    "The total NA values for mammals are:", sum(mammals_test$NA_year),"\n",
    "The total NA values for vertebrates are:", sum(vertebrate_test$NA_year), "\n",
    "The total NA values for all other vertebrates are:", sum(all_others_test$NA_year))
```

    The total NA values for birds are: 13 
     The total NA values for mammals are: 25 
     The total NA values for vertebrates are: 62 
     The total NA values for all other vertebrates are: 57

``` r
barplot(c(13, 25, 62, 57), 
        main = "Total NA values",
        xlab = "Number of NAs",
        ylab = "Species Groups",
        col = "darkred",
        names.arg = c("Birds", "Mammals", "Verts", "Other"),
        horiz = TRUE)
```

![](extinction-assignment_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

We can see that the NA values are highest at Vertebrates (Birds,
Mammals, Amphibians), the second highest would be Other Vertebrates
(exlcuding Birds and Mammals). The bird group have the least NA values.
If we were to leave the species with NA dates out of our final
conclusion, that would tell an unaccurate story of the verterbrates
extinction overall. Here we derive a count for each group of NA dates by
projecting the same proportions of species per bin in our lowest
possible extinction data.

``` r
#first, we grab the proportions of extinctions in each bin for the data that has no NAs, 
#the count per species group over the total counts for data without na values
#note, the dataframe is already alphabetically sorted by group names
proportions <- master_noNA$count/536

#next, we take those proportions multiply it by the total number of NAs for each bin year, this number is 157 as we have seen above 
NAcount <- 157*proportions

#now add those NA counts per bin + the count for data with no NAs together into the dataframe
all_types_NA <- master_noNA %>%
  select(extinct_year, count, group) %>%
  mutate(NA_derived_count = NAcount)

#lets write a function to calculate cumulative sum for the uncertain counts per species group 
count_cumsum_NA <- function(table) {
  table %>%
  plyr::mutate(cumsum_lowNA = cumsum(NA_derived_count)) 
}

birds_NA <- count_cumsum_NA(all_types_NA %>% filter(group == "Birds"))
mammals_NA <- count_cumsum_NA(all_types_NA %>% filter(group == "Mammals"))
vert_NA <- count_cumsum_NA(all_types_NA %>% filter(group == "Vertebrates"))
other_NA <- count_cumsum_NA(all_types_NA %>% filter(group == "Other Vertebrates"))

#lets bind them back into the dataframe
bind_NAs <- rbind(birds_NA, mammals_NA, vert_NA, other_NA)
bind_NAs
```

``` 
# A tibble: 19 x 5
   extinct_year count group             NA_derived_count cumsum_lowNA
          <int> <int> <chr>                        <dbl>        <dbl>
 1         1600     6 Birds                        1.76         1.76 
 2         1700    15 Birds                        4.39         6.15 
 3         1800    22 Birds                        6.44        12.6  
 4         1900    43 Birds                       12.6         25.2  
 5         2000    62 Birds                       18.2         43.4  
 6         1600     1 Mammals                      0.293        0.293
 7         1700     3 Mammals                      0.879        1.17 
 8         1800     2 Mammals                      0.586        1.76 
 9         1900    14 Mammals                      4.10         5.86 
10         2000    50 Mammals                     14.6         20.5  
11         1600     7 Vertebrates                  2.05         2.05 
12         1700    18 Vertebrates                  5.27         7.32 
13         1800    24 Vertebrates                  7.03        14.4  
14         1900    57 Vertebrates                 16.7         31.0  
15         2000   122 Vertebrates                 35.7         66.8  
16         1600     3 Other Vertebrates            0.879        0.879
17         1800     3 Other Vertebrates            0.879        1.76 
18         1900     2 Other Vertebrates            0.586        2.34 
19         2000    82 Other Vertebrates           24.0         26.4  
```

We then use the cumulative some of interpolated NA values and divide by
the total of all evaluated species to find the cumulative percentages,
add this to the cumulative sum percentages of the lowest line rates to
find the line in between the maximum and minimum vertebrates extinction.

``` r
master_ex_table <- master_ex_table %>%
  left_join(bind_NAs) %>%
  mutate(perc_ex_uncertain = (100*(cumsum_lowNA/sum_species)) + perc_ex_noNA) %>%
  select(extinct_year, group, sum_species, perc_ex_withNA, perc_ex_noNA, perc_ex_uncertain)
```

    Joining, by = c("extinct_year", "group")

``` r
master_ex_table
```

    # A tibble: 19 x 6
       extinct_year group sum_species perc_ex_withNA perc_ex_noNA
              <dbl> <chr>       <int>          <dbl>        <dbl>
     1         1600 Birds       11126          0.171      0.0539 
     2         1700 Birds       11126          0.306      0.189  
     3         1800 Birds       11126          0.503      0.386  
     4         1900 Birds       11126          0.890      0.773  
     5         2000 Birds       11126          1.45       1.33   
     6         1600 Mamm~        6237          0.417      0.0160 
     7         1700 Mamm~        6237          0.465      0.0641 
     8         1800 Mamm~        6237          0.497      0.0962 
     9         1900 Mamm~        6237          0.722      0.321  
    10         2000 Mamm~        6237          1.52       1.12   
    11         1600 Vert~       24135          0.286      0.0290 
    12         1700 Vert~       24135          0.360      0.104  
    13         1800 Vert~       24135          0.460      0.203  
    14         1900 Vert~       24135          0.696      0.439  
    15         2000 Vert~       24135          1.20       0.945  
    16         1600 Othe~       33056          0.182      0.00908
    17         1800 Othe~       33056          0.191      0.0182 
    18         1900 Othe~       33056          0.197      0.0242 
    19         2000 Othe~       33056          0.445      0.272  
    # ... with 1 more variable: perc_ex_uncertain <dbl>

Lets’ put these results on multiple graphs to observe the differences,
we have delved deeper into the data and explored different permutations
of dealing with NA values. The graph below has a similar order of
species groups as Cebellos, from high to low: Mammals, Birds,
Vertebrates and Other vertebrates.

``` r
master_ex_table %>% 
  ggplot(aes(x = factor(extinct_year), group = group, color = group)) +
  geom_line(aes(y = perc_ex_withNA), linetype = "dotdash", lwd=1, alpha =0.5) +
  geom_line(aes(y = perc_ex_noNA), linetype = "longdash", lwd =1, alpha = 0.5) +
  geom_line(aes(y = perc_ex_uncertain), linetype = "solid", lwd = 1.5, alpha=0.7) +
  labs(title = "Cumulative Vertebrate Species as EX or EW by the IUCN 2019",
       y = 'Cumulative extinctions as % of UCN-evaluated species',
       x = "Time interval") + 
  scale_x_discrete(labels=c("1500-1600", "1600-1700", "1700-1800", "1800-1900", "1900-2000")) +
  scale_y_continuous(breaks = seq(0,2, by=0.2)) +
  theme_classic() +
  theme(text = element_text(size=10))
```

![](extinction-assignment_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Perhaps the dashed lines above are hard to decipher. The dotted dash
lines represent the highest possible extinction rates and the long dash
lines represent the lowest, the solid lines are our interpolated
uncertainty lines where NA data is projected to have the same
proportions as the data without NAs(the lowest possible rates). Lets’
look individual species group and their different extinction rates.

``` r
plotting_func <- function(table, name) {
  table %>% 
  ggplot(aes_string(x = "extinct_year")) +
  geom_line(aes_string(y = "perc_ex_withNA"), linetype = "dotdash", lwd=1, alpha =0.5, color="blue") +
  geom_line(aes_string(y = "perc_ex_noNA"), linetype = "longdash", lwd =1, alpha = 0.5, color="red") +
  geom_line(aes_string(y = "perc_ex_uncertain"), linetype = "solid", lwd = 1.5, alpha=0.7) +
  labs(title = name,
       y = 'Cumulative extinctions % ',
       x = "Time interval") + 
  scale_y_continuous(breaks = seq(0,2, by=0.2)) +
  theme_classic() +
  theme(text = element_text(size=10))
}


plot1 <- plotting_func(master_ex_table %>% filter(group == "Birds"), "Birds")
plot2 <- plotting_func(master_ex_table %>% filter(group == "Mammals"), "Mammals")
plot3 <- plotting_func(master_ex_table %>% filter(group == "Vertebrates"), "Vertebrates")
plot4 <- plotting_func(master_ex_table %>% filter(group == "Other Vertebrates"), "Other Vertebrates")


grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
```

![](extinction-assignment_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

The uncertainly line for birds is unusally higher in 1900 and 2000 year
bins than the lowest possible extinction line. Due to time constraint,
we have not been able to discover why this is such. However, the discuss
on dealing with NAs is one to be continued throughout all of our future
research. In order to interpolate NA values more accurately, we would
need to look into using statistical techniques that have not been taught
in the classroom. Perhaps, one way to start is to find the pattern of
NAs distribution to all species classes to see if biases exist within
how the data is collected.\!

In all, we can agree that the modern rates of vertebrate extinction are
much higher than the background rate mentioned in Ceballos’s paper at 2
mammal extinctions per 10,000 species per 100 years (2E/MSY). Instead of
using the extinction categories: “EX” = “extinction” category and “EW” =
“extinction in the wild” and “PE” = “possibly extinct” to estimate a
reasonable conservative extinction rates, we looked at the different
ways NA values can affect the final findings. We decided to look the
highest and lowest possible extinction rates by including and excluding
NAs. We ventured into interpolating an uncertainty line between the two
using the excluded NA data proportions for each group of vertebrates. We
find that there is no best answer to the exact rates of extinction but
rather, we can only confirm Ceballos’s statement that the current
extinction rate is much higher than the background rate.
