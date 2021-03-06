"Word" and Sentence-number Detection
================
Suhas Hegde
October 19, 2017

The following program reads the text from the file and saves them into a tabular data structure(data-frame). Each new sentence is saved in a new row, thus the data-frame will have 332938 rows. Using 'regex' pattern matching, a new column is created (containing a logical vector/array with True/False values) to indicate whether or not the given word is present in that particular sentence. The 'regex' pattern uses the 'OR' operator for the plural forms of the word. For example, if a sentence contains both "expert" and "experts", it will only count one of them and thus the sentence number is only displayed once in the final result. Finally sentence numbers are displayed in an interactive point-range plot where if we hover over a certain point, it displays the sentence number corresponding to the given word.

``` r
library(tidyverse) # library for data manipulation
library(tidytext) # text manipulation
library(stringr) # regex and pattern matching in strings
library(tibble) # library that holds the special datastructures



# read in the data from the text file
raw_text <- read_delim("sents_BNCbaby.txt", col_names = FALSE, delim = "\n")

# the raw data 
raw_text
```

    ## # A tibble: 332,938 x 1
    ##                                                                             X1
    ##                                                                          <chr>
    ##  1 "1\tLatest corporate unbundler reveals laid-back approach : Roland Franklin
    ##  2                                                          "2\tBy FRANK KANE"
    ##  3 "3\tIT SEEMS that Roland Franklin , the latest unbundler to appear in the U
    ##  4     "4\tHe has not properly investigated the target 's dining facilities ."
    ##  5 "5\tThe 63-year-old head of Pembridge Investments , through which the bid i
    ##  6 "6\tIf he had taken his own rule seriously , he would have found out that D
    ##  7 "7\tThere are other things he has , on his own admission , not fully invest
    ##  8 "8\tWhen the bid was launched last week , Mr Franklin faced some criticism 
    ##  9                                  "9\tHe regards the charges as unfounded ."
    ## 10                                           "10\tOn property , he is blunt ."
    ## # ... with 332,928 more rows

``` r
# filter out the sentence numbers and create another column
raw_text$X1 %>% str_extract("[0-9]{1,}(?=\t)") -> raw_text$sentence_num

# rename the columns
names(raw_text) <- c("text","sentence_num")

# convert sentence number to numeric type column
as.integer(raw_text$sentence_num) -> raw_text$sentence_num

# build a new dataframe that contains the sentence numbers and the string detection result  for each word
df <- data_frame(
  sentence_num = raw_text$sentence_num,
  priceless = str_detect(raw_text$text, "\\spriceless\\s|\\sPriceless\\s"),
  valuable = str_detect(raw_text$text, "\\svaluable\\s|\\sValuable\\s"),
  absent = str_detect(raw_text$text, "\\sabsent\\s|\\sAbsent\\s"),
  specialist_specialists = str_detect(raw_text$text, "\\sspecialist\\s|\\sspecialists\\s|\\sSpecialist\\s|\\sSpecialists\\s"),
  expert_experts = str_detect(raw_text$text, "\\sexpert\\s|\\sexperts\\s|\\sExpert\\s|\\sExperts\\s"),
  courage = str_detect(raw_text$text, "\\scourage\\s|\\sCourage\\s")
)

# the new table/dataframe with the string setection results
glimpse(df)
```

    ## Observations: 332,938
    ## Variables: 7
    ## $ sentence_num           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, ...
    ## $ priceless              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
    ## $ valuable               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
    ## $ absent                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
    ## $ specialist_specialists <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
    ## $ expert_experts         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
    ## $ courage                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS...

``` r
# convert the data into long form data and filter out only those rows that contain actual detection,
# ie, rows with values that say 'TRUE'
df %>% gather(word, detection, priceless:courage) %>% filter(detection == TRUE) %>%
  select(sentence_num, word) -> df


# plot the sentence numbers for each different word
df %>% ggplot(., aes(word, sentence_num)) +
  geom_point(aes( color=word), size=1.75, alpha = .75) +
  coord_flip()  +
  theme(legend.position = "none") 
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)
