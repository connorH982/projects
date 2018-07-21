Exploratory Data Analysis
================

Data comes from here: <https://www.goodreads.com/list/show/50.The_Best_Epic_Fantasy>

Wordclouds
----------

### Wordcloud of the book titles

``` r
############# TITLE WORDCLOUD #############################################
# Create corpus
title.corpus<-corpus(as.character(bestEpicFantasy$title))

# now creating a document-feature matrix using dfm()
T.plot<-dfm(title.corpus, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords(source = "smart"))

# Colors
T.col <- brewer.pal(10, "BrBG")  

# Create Wordcloud
textplot_wordcloud(T.plot, min_count = 16, color = T.col)
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Wordcloud of the book descriptions

``` r
############# DESCRIPTIONS WORDCLOUD #############################################
# Create corpus
descriptions.corpus<-corpus(as.character(bestEpicFantasy$book.descriptions))

# now creating a document-feature matrix using dfm()
D.plot<-dfm(descriptions.corpus, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords(source = "smart"))

# Colors
D.col <- brewer.pal(10, "BrBG")  

# Wordcloud
textplot_wordcloud(D.plot, min_count = 16, color = D.col)  
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-3-1.png)

Numerical Analysis
------------------

### Total Ratings: Histogram and Boxplot

``` r
ggplot(data = bestEpicFantasy, aes(log(bestEpicFantasy$total_ratings)))+
  geom_histogram(bins = 25,fill="dodgerblue4")+
  labs(title="Histogram for Total # of Reviews")+
  labs(x="Log Total Reviews",y="Counts")
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(data = bestEpicFantasy, aes(y=bestEpicFantasy$total_ratings,x=""))+
  geom_boxplot(fill="dodgerblue4",width=0.1)+
  labs(title="Boxplot for Total Ratings")+
  labs(y="Total # of Ratings",x="")
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
#Outlier: It is Harry Potter book 1 (no surprise there)
bestEpicFantasy[which.max(bestEpicFantasy$total_ratings),c("title","total_ratings","book.descriptions")]
```

    ##                                                         title
    ## 2080 Harry Potter and the Sorcerer's Stone (Harry Potter, #1)
    ##      total_ratings
    ## 2080       5374966
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    book.descriptions
    ## 2080 Harry Potter's life is miserable. His parents are dead and he's stuck with his heartless relatives, who force him to live in a tiny closet under the stairs. But his fortune changes when he receives a letter that tells him the truth about himself: he's a wizard. A mysterious visitor rescues him from his relatives and takes him to his new home, Hogwarts School of Witchcraft and Wizardry.After a lifetime of bottling up his magical powers, Harry finally feels like a normal kid. But even within the Wizarding community, he is special. He is the boy who lived: the only person to have ever survived a killing curse inflicted by the evil Lord Voldemort, who launched a brutal takeover of the Wizarding world, only to vanish after failing to kill Harry.Though Harry's first year at Hogwarts is the best of his life, not everything is perfect. There is a dangerous secret object hidden within the castle walls, and Harry believes it's his responsibility to prevent it from falling into evil hands. But doing so will bring him into contact with forces more terrifying than he ever could have imagined.Full of sympathetic characters, wildly imaginative situations, and countless exciting details, the first installment in the series assembles an unforgettable magical world and sets the stage for many high-stakes adventures to come.

### Pages: Histogram and Boxplot

``` r
#################### PLOT Pages #########################################

pageplt<-ggplot(data = bestEpicFantasy, aes(bestEpicFantasy$pageCounts))+
  geom_histogram(bins = 25,fill="dodgerblue4")+
  labs(title="Histogram for Page Lengths")+
  labs(x="Total Pages",y="Counts")
pageplt
```

    ## Warning: Removed 472 rows containing non-finite values (stat_bin).

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggplot(data = bestEpicFantasy, aes(y=bestEpicFantasy$pageCounts,x=""))+
  geom_boxplot(fill="dodgerblue4",width=0.1)+
  labs(title="Boxplot for Page Lengths")+
  labs(y="Page Lengths",x="")
```

    ## Warning: Removed 472 rows containing non-finite values (stat_boxplot).

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-5-2.png)

### Average Ratings: Histogram and Boxplot

``` r
################### PLOT Average Ratings ###################################

avgplt<-ggplot(data = bestEpicFantasy, aes(bestEpicFantasy$average_rating))+
  geom_histogram(bins = 25,fill="dodgerblue4")+
  labs(title="Histogram for Average Ratings")+
  labs(x="Average Rating",y="Counts")
avgplt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot(data = bestEpicFantasy, aes(y=bestEpicFantasy$average_rating,x=""))+
  geom_boxplot(fill="dodgerblue4",width=0.1)+
  labs(title="Boxplot for Average Ratings")+
  labs(y="Average Rating",x="")
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-6-2.png)

Categorical Data
----------------

### Awards/Nominations: Barplot

``` r
#######################PLOT Award ########################################

awardplt<-ggplot(data=bestEpicFantasy,aes(bestEpicFantasy$hasAward))+geom_bar(fill="dodgerblue4")
awardplt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Genres: Barplot

Almost all books in this set are tagged Fantasy, with a large number of other genres with very few titles. Therefore I relabeled these rarer genres as "Other genres" for presentation and testing.

``` r
######################PLOT Genres ###########################################

tp <- bestEpicFantasy
levels(tp$genreVoted)[levels(tp$genreVoted) != "Fantasy"] <- "Other Genres"

genreplt<-ggplot(data = tp,aes(x = genreVoted))+geom_bar(fill="dodgerblue4")+
  labs(title = "Histogram for Genres")+
  labs(x = "Genre 'tagged' by vote on GoodReads.com", y = "Count")
genreplt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-8-1.png)

Crossing Variables and Statistical Testing
------------------------------------------

### Genres and Awards: CHI-SQ

``` r
############# CROSS Genres With Awards###################################
# Outcome: SUCCESSFULLY reject Null Hypothesis
crossplt<-ggplot(data = tp,aes(x = genreVoted, fill = hasAward))+geom_bar(position = "dodge")+
  labs(title = "Genres crossed with Awards")+
  labs(x = "Genre", y = "Count")+
  scale_fill_discrete("Has Award/Nom")
crossplt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# Same plot but if we wanted to exclude NA cases
tpNoNA<-tp[complete.cases(tp),]
tbl<-table(tpNoNA$genreVoted, tpNoNA$hasAward)
chisq.test(tbl)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  tbl
    ## X-squared = 11.002, df = 1, p-value = 0.0009101

``` r
crossplt2<-ggplot(data = tpNoNA,aes(x = genreVoted, fill = hasAward))+geom_bar(position = "dodge")+
  labs(title = "Genres crossed with Awards (excluding NAs)")+
  labs(x = "Genre", y = "Count")+
  scale_fill_discrete("Has Award/Nom")
crossplt2
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-9-2.png)

### Average Rating with Genre: Kruskal-Wallis

``` r
############# CROSS Genres With Average Ratings###################################
# Outcome: Close... can reject the null hypothesis at 0.05 confidence threshold.

avgGenrePlt<-ggplot(data = tp,aes(x = genreVoted, y = average_rating))+geom_boxplot(fill="dodgerblue4", width=0.1)+
  labs(title = "Genres By Ratings")+
  labs(x = "Genre", y = "Ratings")
avgGenrePlt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
kruskal.test(average_rating~genreVoted,data = tp)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  average_rating by genreVoted
    ## Kruskal-Wallis chi-squared = 4.0544, df = 1, p-value = 0.04406

``` r
tblResults<-with(tp,tapply(average_rating,genreVoted,median))
tblResults
```

    ## Other Genres      Fantasy 
    ##         4.05         4.04

### Page Lengths with Genre: Kruskal-Wallis

``` r
############# CROSS Genres With Page Counts###################################
# Outcome: SUCCESSFULLY reject Null hypothesis

pageGenrePlt<-ggplot(data = tp,aes(x = genreVoted, y = pageCounts))+geom_boxplot(fill="dodgerblue4",width=0.1)+
  labs(title = "Genres By Pages")+
  labs(x = "Genre", y = "Page Length")
pageGenrePlt
```

    ## Warning: Removed 472 rows containing non-finite values (stat_boxplot).

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
kruskal.test(pageCounts~genreVoted,data = tp)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  pageCounts by genreVoted
    ## Kruskal-Wallis chi-squared = 33.987, df = 1, p-value = 5.548e-09

### Average Rating with Award/Nomination: Kruskal-Wallis

``` r
############# CROSS Award With Average Ratings###################################
#Outcome: FAIL to reject null

avgGenrePlt<-ggplot(data = tp,aes(x = hasAward, y = average_rating))+geom_boxplot(fill="dodgerblue4", width=0.1)+
  labs(title = "Awards/Nominations By Ratings")+
  labs(x = "Award/Nominations", y = "Ratings")
avgGenrePlt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
kruskal.test(average_rating~hasAward,data = tp)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  average_rating by hasAward
    ## Kruskal-Wallis chi-squared = 0.37614, df = 1, p-value = 0.5397

### Total Ratings with Award/Nomination: Kruskal-Wallis

``` r
############# CROSS Awards With Total Numbers of Reviews ###################################
#Outcome: SUCCESSFULLY reject Null
pageGenrePlt<-ggplot(data = tp,aes(x = hasAward, y = total_ratings))+geom_boxplot(fill="dodgerblue4",wdith=0.1)+
  labs(title = "Award/Nomination By Total Ratings")+
  labs(x = "Award", y = "Total Ratings")
```

    ## Warning: Ignoring unknown parameters: wdith

``` r
pageGenrePlt
```

![](DataVisualization_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
kruskal.test(total_ratings~hasAward,data = tp)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  total_ratings by hasAward
    ## Kruskal-Wallis chi-squared = 269.06, df = 1, p-value < 2.2e-16

``` r
tblResults<-with(tp,tapply(total_ratings,hasAward,median))
tblResults
```

    ## FALSE  TRUE 
    ##  1087 73265
