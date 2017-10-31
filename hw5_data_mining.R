library(tidyverse)
library(tidytext)
library(stringr)
library(tibble)
library(ggiraph)


# read in the data from the text file
raw_text <- read_delim("sents_BNCbaby.txt", col_names = FALSE, delim = "\n")

# filter out the sentence numbers and create another column
raw_text$X1 %>% str_extract("[0-9]{1,}(?=\t)") -> raw_text$sentence_num

# rename the columns
names(raw_text) <- c("text","sentence_num")

# convert sentence number to numeric type column
as.integer(raw_text$sentence_num) -> raw_text$sentence_num

# build a new dataframe that contains the sentence numbers and the string detection result 
# for each word

df <- data_frame(
  sentence_num = raw_text$sentence_num,
  priceless = str_detect(raw_text$text, "\\spriceless\\s|\\sPriceless\\s"),
  valuable = str_detect(raw_text$text, "\\svaluable\\s|\\sValuable\\s"),
  absent = str_detect(raw_text$text, "\\sabsent\\s|\\sAbsent\\s"),
  specialist_specialists = str_detect(raw_text$text, "\\sspecialist\\s|\\sspecialists\\s|\\sSpecialist\\s|\\sSpecialists\\s"),
  expert_experts = str_detect(raw_text$text, "\\sexpert\\s|\\sexperts\\s|\\sExpert\\s|\\sExperts\\s"),
  courage = str_detect(raw_text$text, "\\scourage\\s|\\sCourage\\s")
)


# convert the data into long form data and 
# filter out only those rows that contain actual detection, ie, rows with 'TRUE'
df %>% gather(word, detection, priceless:courage) %>% filter(detection == TRUE) %>%
  select(sentence_num, word) -> df

# plot the sentence numbers for each different word
df %>% ggplot(., aes(word, sentence_num)) +
  geom_point_interactive(aes(tooltip = sentence_num, color=word), size=1.75, alpha = .75) +
  coord_flip()  -> gg

ggiraph(code = print(gg))

df %>% ggplot(., aes(sentence_num)) +
  geom_density() +
  facet_wrap(~ word)



# --------------------------------------------------------------------------------------

# not a good approach
# build a vector(array) with the words that needs to be searched
text_to_compare <-   data_frame(word = c("priceless","valuable","absent","specialist",
                                       "specialists","expert","experts","courage"))
  
# convert raw_text to a the required format - each word divided into 
# one token and sentence_num indicating the original line numbers
raw_text %>% unnest_tokens(word, text) -> raw_text

# use "inner_join" to filter out words that are only in the lexicon and 
# their corresponding line numbers  
raw_text %>% inner_join(text_to_compare) %>% arrange() -> line_no_list 


print(line_no_list, n=622)

# too complicated
# better approach
regex_pattern <- "priceless|valuable|absent|specialist|specialists|expert|experts|courage+"

regex_pattern <- "[priceless][valuable][absent][specialist|specialists][expert|experts][courage]"

regex_pattern <- c("priceless+","valuable+","absent+","specialist+","specialists+",
                   "expert+","experts+","courage+")

pattern2 <- "specialists|experts"

# detect the pattern in the text corpus
raw_text$text %>% str_detect(regex_pattern) -> raw_text$string_detection_res

# extract all matches and save them into a wide column table
raw_text$text %>% str_extract_all(regex_pattern, simplify = TRUE) %>% as_data_frame() %>%
  mutate(sentence_num = row_number())-> string_matches



raw_text$text %>% map(str_extract, pattern = regex_pattern ) -> li


li %>% set_names(1:length(raw_text$text)) %>% 
  as_data_frame() -> df

raw_text$text %>% str_extract_all(pattern, simplify = TRUE) %>% 
  as_data_frame() %>% mutate(sentence_num = row_number())->s  

string_matches %>% gather(group, matched_string, -sentence_num) -> s


raw_text %>% filter(string_detection_res==TRUE) %>% select(sentence_num) %>% distinct()




