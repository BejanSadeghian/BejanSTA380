library(arules)
library(arulesViz)


groceries = read.transactions('groceries.txt', format='basket', sep=',', rm.duplicates = TRUE)

#Setup the rules
#I chose support = 0.001 because since we have around 10,000 transactions, if an item is in approx. 10+ of the baskets I 
#wanted to say that is an item worth looking at
#I chose maxlen = 10 to see the impact of a large itemset
#I chose confidence = 0.1 because I wanted the results that showed item sets were in atleast 10% of my LHS itemset
grocery.rules = apriori(groceries, parameter=list(support=0.001, confidence=0.4, maxlen=10, target='rules'))

inspect(head(sort(grocery.rules, by='lift')))

inspect(sort(subset(grocery.rules, subset=lift > 10), by='lift')) #I want to see what itemsets have a high frequency of being purchased with another itemset and primarily with that itemsets (in other words not purchased without the first itemset) 
inspect(sort(subset(grocery.rules, subset=confidence == 1.0), by='lift')) #Want to see what RHS itemsets are purchased every time with a LHS set

grocery.rules = apriori(groceries, parameter=list(support=0.001, confidence=0.1, maxlen=2, target='rules')) # Because I was getting confidence of 1.0 I feel like that the maxlen rule is letting things get too general. In other words, the max rule is just too large and skewing my results
inspect(head(sort(subset(grocery.rules, subset=support > 0.01 & confidence > 0.2), by='lift'),25)) #want to see what 10% of our itemsets involve and what other itemsets have a high frequency in these

plot(head(sort(subset(grocery.rules, subset=support > 0.01 & confidence > 0.2), by='lift'),20),method="graph",interactive=FALSE)
