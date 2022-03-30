
library(tidyverse)
library(rvest)
library(polite)
library(purrr)
library(sf)
# Create a session object using bow()

session = bow("https://www.ncleg.gov/Members/MemberList/S")

# Next, retrieve the page

resp = scrape(session)

# Find class that contains the information about the senators
senator_elements = html_elements(resp, ".col-8")

# Extract the Senator's name, district, and url

senator = map_dfr(senator_elements, function(element) {
  senator_name = html_element(element, "a") %>% html_text2()
  senator_district = html_element(element, "p:contains(District)") %>% html_text2()
  senator_url = html_element(element, "a") %>% html_attr("href")
  
  return(list(
    "Name"=senator_name,
    "DISTRICT"=senator_district,
    "url"=senator_url
  ))
})

# Find district number at the end of the string

senator$DISTRICT = str_extract(senator$DISTRICT, "[0-9]+$")

# Remove Sam Searcy because he resigned
senator = senator[-c(48), ]

# Add website to the senators url in order for the link to work

senator$url = paste0("https://www.ncleg.gov", senator$url)

# Not on the same server so create a new session

terms_sess = bow("https://www.ncleg.gov")

# Extract senators term information

get_terms = function(url) {
  terms_sess <<- nod(terms_sess, url)
  resp = scrape(terms_sess)
  terms = html_elements(resp, ".col-12 p:contains('in House')") %>%
    first() %>%
    html_text2()
  
  return(terms)
}


# Extract number of terms served by each senator

with_terms = rowwise(senator, everything()) %>%
  mutate(terms=get_terms(url))

#Repeat previous step but now extract senators party affiliation
#New session
party_sess = bow("https://www.ncleg.gov")

#Extract party information
get_party = function(url) {
  party_sess <<- nod(party_sess, url)
  resp = scrape(party_sess)
  party = html_elements(resp, "h6:contains('District')") %>%
    first() %>%
    html_text2()
  
  return(party)
}
with_party = rowwise(with_terms, everything()) %>%
  mutate(party=get_party(url))

#Extract number terms served at the beginning of the string

with_party$terms = as.numeric(str_extract(with_party$terms, "^[0-9]+"))

# Sydney Batch doesn't have a number for numbers of terms served so I removed her 

with_party = with_party[-c(4), ]



# Extract only Senators party affiliation

with_party$party = str_extract(with_party$party, "^(Democrat|Republican)")

# Remove URL column
with_party = with_party[-c(3)]

View(with_party)

# Save as csv
write_csv(with_party, "nc_senate_composition_2022.csv")

# Calculate whether Republicans or Democratics have serverd more terms on avg as senators

group_by(with_party, party) %>%
  summarize(mean_terms=mean(terms))

# Democrat: 3 
# Republican: 3.18 
# Republicans have serverd more terms on avg

# Read the district map which was used in the 2020 election

district_map = read_sf("Senate Consensus Nonpartisan Map v3.shp")

# Join district map with dataset

district_map = left_join(district_map, with_party, on="DISTRICT")

# Map showing which seats are occupied by Republicans

Rseats = filter(district_map, party == "Republican")

ggplot() +
  geom_sf(data=Rseats, aes(fill=party))

# Map showing how many terms the senator in each district has served
ggplot() +
  geom_sf(data=district_map, aes(fill=terms), lwd=0) +
  scale_fill_fermenter(palette="Blues", n.breaks=8)
