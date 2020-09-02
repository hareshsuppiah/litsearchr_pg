library(litsearchr)

search_directory <- "./naive_results/"

# Import RIS file
naiveimport <-
  litsearchr::import_results(directory = search_directory, verbose = TRUE)

# Remove repeated
naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")

# Identify potential keywords
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

head(rakedkeywords,10)

all_keywords <-rakedkeywords

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = as.matrix(naivedfm),
    min_studies = 2,
    min_occ = 2
  )

cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .50,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- litsearchr::get_keywords(reducedgraph)

head(searchterms, 20)
write.csv(searchterms, "./search_terms.csv")

grouped_terms <- read.csv("./search_terms_grouped.csv")
# extract the energetics terms from the csv
energetics_terms <- grouped_terms$term[grep("energetics", grouped_terms$group)]
# join together a list of manually generated energetics terms with the ones from the csv
energetics <- unique(append(c("energetics","Energy-system contribution",
                              "Energy system contribution",
                              "Energy contribution","Energy expenditure",
                              "Energy demand","Energy cost",
                              "energy release","Anaerobic capacity",
                              "Anaerobic exercise","Anaerobic threshold",
                              "Anaerobic system contribution", " Anaerobic system",
                              "ENERGY MEASUREMENT","Aerobic system contribution",
                              "Aerobic system","Aerobic capacity ",
                              "Aerobic exercise"), energetics_terms))

# extract the demographic terms from the csv
demo_terms <- grouped_terms$term[grep("demo", grouped_terms$group)]
# join together a list of manually generated demo terms with the ones from the csv
demos <- demo_terms

# extract the method terms from the csv
method_terms <- grouped_terms$term[grep("method", grouped_terms$group)]
# join together a list of manually generated method terms with the ones from the csv
methods <- method_terms
# Merge all grouped terms into a list
mysearchterms <- list(energetics, demos,methods)

# Write boolean search terms

my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "left",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )

my_search
str(my_search)
my_search <- gsub("i* ","* ",my_search, fixed=TRUE) # Address stemming problem
my_search <- gsub("i*","*",my_search, fixed=TRUE) # Address stemming problem
my_search

write.csv(my_search, "./my_search.csv")

# Check search strategy precision and recall

gold_standard <- c("Energy system contribution to 400-metre and 800-metre track running",
                   "Energy system contribution to 1500-and 3000-metre track running",
                   "Anaerobic and aerobic energy system contribution to 400-m flat and 400-m hurdles track running")

title_search <- litsearchr::write_title_search(titles=gold_standard)

library(synthesisr)

results_directory <- "./extdata/"
retrieved_articles <-litsearchr::import_results(directory = results_directory, verbose = TRUE)
articles_found <- litsearchr::check_recall(true_hits = gold_standard,
                                           retrieved = retrieved_articles$title)
articles_found
