library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

mushroom <- read.csv('mushrooms.csv')

str(mushroom)

mushroom %>% count(class)
#edible : poison
# 51.8  : 48.2



#=============================================================
# preparing data to be easier to manage
#=============================================================

unique(data_mushroom$ring.number)

data_mushroom <- mushroom %>%
  mutate(class = factor(ifelse(class == "e", "edible", "poisonous")),
         cap.shape = factor(ifelse(cap.shape == "b", "bell",
                            ifelse(cap.shape == "c", "conical",
                            ifelse(cap.shape == "x", "convex",
                            ifelse(cap.shape == "f", "flat",
                            ifelse(cap.shape == "k", "knobbed",
                                  "sunken")))))),
         cap.surface = factor(ifelse(cap.surface == "f", "fibrous",
                              ifelse(cap.surface == "g", "grooves",
                              ifelse(cap.surface == "y", "scaly",
                                     "smooth")))),
         cap.color = factor(ifelse(cap.color == "n", "brown",
                            ifelse(cap.color == "b", "buff",
                            ifelse(cap.color == "c", "cinnamon",
                            ifelse(cap.color == "g", "gray",
                            ifelse(cap.color == "r", "green",
                            ifelse(cap.color == "p", "pink",
                            ifelse(cap.color == "u", "purple",
                            ifelse(cap.color == "e", "red",
                            ifelse(cap.color == "w", "white",
                                   "yellow")))))))))),
         bruises = factor(ifelse(bruises == "t", "bruises", 
                                          "no")),
         odor = factor(ifelse(odor == "a", "almond",
                       ifelse(odor == "l", "anise",
                       ifelse(odor == "c", "creosote",
                       ifelse(odor == "y", "fishy",
                       ifelse(odor == "f", "foul",
                       ifelse(odor == "m", "musty",
                       ifelse(odor == "n", "none",
                       ifelse(odor == "p", "pungent",
                              "spicy"))))))))),
         gill.attachment = factor(ifelse(gill.attachment == "a", "attached",
                                  ifelse(gill.attachment == "d", "descending",
                                  ifelse(gill.attachment == "f", "free",
                                         "notched")))),
         gill.spacing = factor(ifelse(gill.spacing == "c", "close",
                                        ifelse(gill.spacing == "w", "crowded",
                                      "distant"))),
         gill.size = factor(ifelse(gill.size == "b", "broad", 
                                            "narrow")),
         gill.color = factor(ifelse(gill.color == "k", "black", 
                             ifelse(gill.color == "n", "brown",
                             ifelse(gill.color == "b", "buff",
                             ifelse(gill.color == "h", "chocolate",
                             ifelse(gill.color == "g", "gray", 
                             ifelse(gill.color == "r", "green",
                             ifelse(gill.color == "o", "orange",
                             ifelse(gill.color == "u", "purple",
                             ifelse(gill.color == "e", "red",
                             ifelse(gill.color == "w", "white",
                             ifelse(gill.color == "y", "yellow",
                                    "pink")))))))))))),
         stalk.shape = factor(ifelse(stalk.shape == "e", "enlarging", 
                                              "tapering")),
         stalk.root = factor(ifelse(stalk.root == "b", "bulbous",
                                      ifelse(stalk.root == "c", "club",
                                      ifelse(stalk.root == "u", "cup",
                                      ifelse(stalk.root == "e", "equal",
                                      ifelse(stalk.root == "z", "rhizomorphs",
                                      ifelse(stalk.root == "r", "rooted",
                                             "missing"))))))),
         stalk.surface.above.ring = factor(ifelse(stalk.surface.above.ring == "f", "fibrous",
                                                    ifelse(stalk.surface.above.ring == "y", "scaly",
                                                    ifelse(stalk.surface.above.ring == "k", "silky",
                                                           "smooth")))),
         stalk.surface.below.ring = factor(ifelse(stalk.surface.below.ring == "f", "fibrous",
                                                    ifelse(stalk.surface.below.ring == "y", "scaly",
                                                    ifelse(stalk.surface.below.ring == "k", "silky",
                                                           "smooth")))),
         stalk.color.above.ring = factor(ifelse(stalk.color.above.ring == "n", "brown",
                                                  ifelse(stalk.color.above.ring == "b", "buff",
                                                  ifelse(stalk.color.above.ring == "c", "cinnamon",
                                                  ifelse(stalk.color.above.ring == "g", "gray",
                                                  ifelse(stalk.color.above.ring == "o", "orange",
                                                  ifelse(stalk.color.above.ring == "p", "pink",
                                                  ifelse(stalk.color.above.ring == "e", "red",
                                                  ifelse(stalk.color.above.ring == "w", "white",
                                                         "yellow"))))))))),
         stalk.color.below.ring = factor(ifelse(stalk.color.above.ring == "n", "brown",
                                                  ifelse(stalk.color.above.ring == "b", "buff",
                                                  ifelse(stalk.color.above.ring == "c", "cinnamon",
                                                  ifelse(stalk.color.above.ring == "g", "gray",
                                                  ifelse(stalk.color.above.ring == "o", "orange",
                                                  ifelse(stalk.color.above.ring == "p", "pink",
                                                  ifelse(stalk.color.above.ring == "e", "red",
                                                  ifelse(stalk.color.above.ring == "w", "white",
                                                         "yellow"))))))))),
         veil.type = factor(ifelse(veil.type == "p", "partial", 
                                            "universal")),
         veil.color = factor(ifelse(veil.color == "n", "brown", 
                                      ifelse(veil.color == "o", "orange",
                                      ifelse(veil.color == "w", "white",
                                             "yellow")))),
         ring.number = ifelse(ring.number == "n", 0, 
                                       ifelse(ring.number == "o", 1,
                                              2)),
         ring.type = factor(ifelse(ring.type == "c", "cobwebby", 
                                     ifelse(ring.type == "e", "evanescent",
                                     ifelse(ring.type == "f", "flaring",
                                     ifelse(ring.type == "l", "large",
                                     ifelse(ring.type == "n", "none",
                                     ifelse(ring.type == "p", "pendant",
                                     ifelse(ring.type == "s", "sheathing",
                                            "zone")))))))),
         spore.print.color = factor(ifelse(spore.print.color == "k", "black", 
                                             ifelse(spore.print.color == "n", "brown",
                                             ifelse(spore.print.color == "b", "buff",
                                             ifelse(spore.print.color == "h", "chocolate",
                                             ifelse(spore.print.color == "r", "green",
                                             ifelse(spore.print.color == "o", "orange",
                                             ifelse(spore.print.color == "u", "purple",
                                             ifelse(spore.print.color == "w", "white",
                                                    "yellow"))))))))),
         population = factor(ifelse(population == "a", "abundant",
                                      ifelse(population == "c", "clustered",
                                      ifelse(population == "n", "numerous",
                                      ifelse(population == "s", "scattered",
                                      ifelse(population == "v", "several",
                                             "solitary")))))),
         habitat = factor(ifelse(habitat == "g", "grasses",
                                   ifelse(habitat == "l", "leaves",
                                   ifelse(habitat == "m", "meadows",
                                   ifelse(habitat == "p", "paths",
                                   ifelse(habitat == "u", "urban",
                                   ifelse(habitat == "w", "waste",
                                          "woods"))))))))

str(data_mushroom)
summary(data_mushroom)
#=============================================================

data_mushroom <- 
  data_mushroom %>% 
  select(-stalk.color.below.ring, -veil.type)

#=============================================================
# analyze data - relation to class(edible or poison)
#=============================================================

#
table(data_mushroom$class, data_mushroom$cap.shape)
ggplot(data_mushroom, aes(x=class, y=cap.shape)) + geom_jitter()
#bell     -> 
#conical  -> 
#convex   -> 
#flat     -> 
#knobbed  -> 
#sunken   -> 


table(data_mushroom$class, data_mushroom$cap.surface)
ggplot(data_mushroom, aes(x=class, y=cap.surface)) + geom_jitter()

table(data_mushroom$class, data_mushroom$cap.color)
ggplot(data_mushroom, aes(x=class, y=cap.color)) + geom_jitter()

##
table(data_mushroom$class, data_mushroom$bruises)
ggplot(data_mushroom, aes(x=class, y=bruises)) + geom_jitter()

###
table(data_mushroom$class, data_mushroom$odor)
ggplot(data_mushroom, aes(x=class, y=odor)) + geom_jitter()

table(data_mushroom$class, data_mushroom$gill.attachment)
ggplot(data_mushroom, aes(x=class, y=gill.attachment)) + geom_jitter()

table(data_mushroom$class, data_mushroom$gill.spacing)
ggplot(data_mushroom, aes(x=class, y=gill.spacing)) + geom_jitter()


table(data_mushroom$class, data_mushroom$gill.size)
ggplot(data_mushroom, aes(x=class, y=gill.size)) + geom_jitter()

table(data_mushroom$class, data_mushroom$gill.color)
ggplot(data_mushroom, aes(x=class, y=gill.color)) + geom_jitter()

table(data_mushroom$class, data_mushroom$stalk.shape)
ggplot(data_mushroom, aes(x=class, y=stalk.shape)) + geom_jitter()

table(data_mushroom$class, data_mushroom$stalk.root)
ggplot(data_mushroom, aes(x=class, y=stalk.root)) + geom_jitter()

table(data_mushroom$class, data_mushroom$stalk.surface.above.ring)
ggplot(data_mushroom, aes(x=class, y=stalk.surface.above.ring)) + geom_jitter()

table(data_mushroom$class, data_mushroom$stalk.surface.below.ring)
ggplot(data_mushroom, aes(x=class, y=stalk.surface.below.ring)) + geom_jitter()

table(data_mushroom$class, data_mushroom$stalk.color.above.ring)
ggplot(data_mushroom, aes(x=class, y=stalk.color.above.ring)) + geom_jitter()

table(data_mushroom$class, data_mushroom$stalk.color.below.ring)
ggplot(data_mushroom, aes(x=class, y=stalk.color.below.ring)) + geom_jitter()

table(data_mushroom$class, data_mushroom$veil.type)
ggplot(data_mushroom, aes(x=class, y=veil.type)) + geom_jitter()

table(data_mushroom$class, data_mushroom$veil.color)
ggplot(data_mushroom, aes(x=class, y=veil.color)) + geom_jitter()

table(data_mushroom$class, data_mushroom$ring.number)
ggplot(data_mushroom, aes(x=class, y=ring.number)) + geom_jitter()

table(data_mushroom$class, data_mushroom$ring.type)
ggplot(data_mushroom, aes(x=class, y=ring.type)) + geom_jitter()

####
table(data_mushroom$class, data_mushroom$spore.print.color)
ggplot(data_mushroom, aes(x=class, y=spore.print.color)) + geom_jitter()

table(data_mushroom$class, data_mushroom$population)
ggplot(data_mushroom, aes(x=class, y=population)) + geom_jitter()

table(data_mushroom$class, data_mushroom$habitat)
ggplot(data_mushroom, aes(x=class, y=habitat)) + geom_jitter()

#=============================================================

data1 <- data_mushroom %>%
  filter(class == "poisonous")

ggplot(data_mushroom, aes(fill = class)) + 
  geom_bar(aes(x=cap.shape), position = "fill") 

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=cap.surface), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=cap.color), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=bruises), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=odor), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.attachment), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.spacing), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.size), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.color), position = "fill")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.shape), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.root), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.surface.above.ring), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.surface.below.ring), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.color.above.ring), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.color.below.ring), position = "dodge")


str(data_mushroom)


ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=veil.type), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=veil.color), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=ring.number), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=ring.type), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=spore.print.color), position = "fill")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=population), position = "dodge")

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=habitat), position = "dodge")

unique(data_mushroom$veil.color)

ggplot(data_mushroom, aes(color = class)) + geom_jitter(aes(x = stalk.surface.above.ring, y = stalk.surface.below.ring))

ggplot(data_mushroom, aes(color = class)) + geom_jitter(aes(x = habitat, y = population))

#=============================================================
# separate train and test data set
#=============================================================

#round1
mush_train_ind <- sample(nrow(data_mushroom), 0.6*nrow(data_mushroom))
mush_train <- data_mushroom[mush_train_ind,]
mush_train %>% count(class)
#edible : poison
# 51.4  : 48.6

mush_test <- data_mushroom[-mush_train_ind,]
mush_test %>% count(class)
#edible : poison
# 52.4  : 47.6

model1 <- rpart(class ~., data = mush_train, control = rpart.control(cp = 0.001))
model1
res1 <- predict(model1, mush_test, type = "class")
res1
confusionMatrix(res1, mush_test$class, positive = 'edible')


#round2
mush_train_ind2 <- sample(nrow(data_mushroom), 0.6*nrow(data_mushroom))
mush_train2 <- data_mushroom[mush_train_ind2,]
mush_train2 %>% count(class)
table(mush_train2$habitat)
#edible : poison
#   : 

mush_test2 <- data_mushroom[-mush_train_ind2,]
mush_test2 %>% count(class)
#edible : poison
#   : 

model2 <- rpart(class ~ odor + stalk.root + stalk.surface.above.ring +
                  stalk.surface.below.ring +
                  stalk.color.above.ring + habitat + spore.print.color + ring.type
                , data = mush_train2, control = rpart.control(cp = 0.001))
#model2
res2 <- predict(model2, mush_test2, type = "class")
res2
confusionMatrix(res2, mush_test2$habitat)


confusionMatrix(res2, mush_test2$class, mode = "prec_recall")

pdf('decisiontree.pdf')
rpart.plot(model2)
dev.off()

data_mushroom$variable.importance
#=============================================================

model2 <- rpart(habitat ~ cap.shape + cap.surface + cap.color + 
                  gill.attachment + gill.spacing + gill.size +
                  gill.color + stalk.shape + stalk.root + stalk.surface.above.ring +
                  stalk.surface.below.ring + stalk.color.above.ring + 
                  stalk.color.below.ring + veil.type + veil.color + ring.type + population
                , data = mush_train2)

str(data_mushroom)



mush_train_ind3 <- sample(nrow(data_mushroom), 0.6*nrow(data_mushroom))
mush_train3 <- data_mushroom[mush_train_ind3,]
mush_train3 <- mush_train3 %>% select(-stalk.color.below.ring, -veil.type)
mush_train3 %>% count(class)

mush_test3 <- data_mushroom[-mush_train_ind3,]
mush_test3 %>% count(class)
mush_test3 <- mush_test3 %>% select(-stalk.color.below.ring, -veil.type)
table(mush_train3$class)

str(mush_train3)

model3 <- glm(class ~ ., 
              data = mush_train3)
res3 <- predict(model3, mush_test3, type = "response")
table(mush_test3$class)
res3_c <- factor(ifelse(res3 < 0.5, "edible", "poisonous"))
confusionMatrix(res3_c, mush_test3$class, positive = 'edible')




ggplot(data_mushroom, aes(color = habitat)) + geom_point()




#model <- glm(class ~ cap.shape + spore.print.color + ring.type, data_mushroom, family = binomial)
#res <- predict(model, data_mushroom, type = "response")
#res_c <- factor(ifelse(res > 0.1, "edible", "poisonous"))
#confusionMatrix(res_c, data_mushroom$class, mode = "prec_recall", positive = "edible") 


#model1 <- glm(B16 ~ B9 + B10 + B11, dataFull, family = binomial)
#model1
#summary(model1)
#res1 <- predict(model1, dataFull, type = "response")
#res1_c <- factor(ifelse(res1 > 0.5, "+", "-"))
#confusionMatrix(res1_c, dataFull$B16, mode = "prec_recall", positive = "+") 
#Accuracy:  0.8599
#Precision: 0.7877
#Recall:    0.9400

unique(data_mushroom$stalk.color.below.ring)

unique(data_mushroom$veil.type)




ggplot(data_mushroom, aes(x=veil.type, y=ring.number)) + 
  geom_jitter()





ggplot(data_mushroom, aes(fill=habitat)) + geom_bar(aes(x=cap.shape), position = "fill")

ggplot(data_mushroom, aes(fill=habitat)) + geom_bar(aes(x=cap.color), position = "fill")

ggplot(data_mushroom, aes(fill=habitat)) + geom_bar(aes(x=spore.print.color), position = "fill")

ggplot(data_mushroom, aes(fill=habitat)) + geom_bar(aes(x=stalk.surface.above.ring), position = "fill")



ggplot(data_mushroom, aes(color = class)) +
  geom_jitter(aes(x=stalk.surface.above.ring, y=stalk.surface.below.ring))


#===============================================================================

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=cap.shape), position = "dodge") +
  facet_wrap(vars(habitat))
#grass,wood,path

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=cap.surface), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=cap.color), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=bruises), position = "dodge") +
  facet_wrap(vars(habitat))
###

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=odor), position = "dodge") +
  facet_wrap(vars(habitat))
#

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.attachment), position = "dodge") +
  facet_wrap(vars(habitat))
#grass,path

data10 <- data_mushroom %>% filter(habitat == c("grasses", "paths", "woods"))
ggplot(data10, aes(fill = class)) +
  geom_bar(aes(x=gill.attachment), position = "dodge") +
  facet_wrap(vars(cap.shape))
#useless

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.spacing), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.size), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=gill.color), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.shape), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.root), position = "dodge") +
  facet_wrap(vars(habitat))


ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.surface.above.ring), position = "dodge") +
  facet_wrap(vars(habitat))


ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.surface.below.ring), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.color.above.ring), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=stalk.color.below.ring), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=veil.type), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=veil.color), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=ring.number), position = "dodge") +
  facet_wrap(vars(habitat))


ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=ring.type), position = "dodge") +
  facet_wrap(vars(habitat))
#####

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=spore.print.color), position = "dodge") +
  facet_wrap(vars(habitat))

ggplot(data_mushroom, aes(fill = class)) +
  geom_bar(aes(x=population), position = "dodge") +
  facet_wrap(vars(habitat))


#===============================================================================

#set.seed(48)



mush_train_ind4 <- sample(nrow(data_mushroom), 0.6*nrow(data_mushroom))
mush_train4 <- data_mushroom[mush_train_ind4,]
table(mush_train4$class)

mush_test4 <- data_mushroom[-mush_train_ind4,]
table(mush_test4$class)


#surface only
model4 <- rpart(class ~ cap.surface + stalk.surface.above.ring +
                  stalk.surface.below.ring,
                mush_train4, control = rpart.control(cp = 0.001))

#color only
model4 <- rpart(class ~ cap.color + stalk.color.above.ring +
                  gill.color + veil.color,
                mush_train4, control = rpart.control(cp = 0.001))

#shape only
model4 <- rpart(class ~ cap.shape + stalk.shape,
                mush_train4, control = rpart.control(cp = 0.001))

#stalk only
model4 <- rpart(class ~ stalk.shape + stalk.root + stalk.surface.above.ring +
                  stalk.surface.below.ring + stalk.color.above.ring,
                mush_train4, control = rpart.control(cp = 0.001))


model4 <- rpart(class ~ stalk.root + stalk.surface.above.ring +
                  stalk.surface.below.ring + stalk.color.above.ring,
                mush_train4, control = rpart.control(cp = 0.001))



model4$variable.importance

pdf("decition_stalk5.pdf")
rpart.plot(model4)
dev.off()


mush_train_ind4 <- sample(nrow(data_mushroom), 0.6*nrow(data_mushroom))
mush_train4 <- data_mushroom[mush_train_ind4,]
table(mush_train4$class)

mush_test4 <- data_mushroom[-mush_train_ind4,]
table(mush_test4$class)


res4 <- predict(model4, mush_test4, type = "class")
confusionMatrix(res4, mush_test4$class, mode = "prec_recall", positive = "edible" )





model4 <- rpart(class ~ cap.shape + cap.surface +
                  bruises + stalk.shape + cap.color,
                mush_train4, control = rpart.control(cp = 0.001))



model4 <- rpart(class ~ gill.spacing + gill.size + gill.color +
                  gill.attachment,
                mush_train4, control = rpart.control(cp = 0.001))







set.seed(3317)
mush_train_ind <- sample(nrow(data_mushroom), 0.6*nrow(data_mushroom))
mush_train <- data_mushroom[mush_train_ind,]
table(mush_train$class)

mush_test <- data_mushroom[-mush_train_ind,]
table(mush_test$class)

AllStalk <- rpart(class ~ stalk.root +
                    stalk.surface.below.ring + odor + cap.color,
                  mush_train, control = rpart.control(cp = 0.001))

res <- predict(AllStalk, mush_train, type = "class")
confusionMatrix(res, mush_train$class, mode = "prec_recall", positive = "edible" )

pdf("decition_stalk6.pdf")
rpart.plot(model4)
dev.off()


ggplot(data_mushroom,aes(x=stalk.surface.below.ring, y=stalk.shape)) +
  geom_jitter(aes(color = class)) + facet_wrap(vars(stalk.root))


stalk.shape + stalk.root + stalk.surface.above.ring +
  stalk.surface.below.ring + stalk.color.above.ring

#=======================================================================================

data11 <- data_mushroom %>%
  filter(stalk.root == "bulbous")

data12 <- data_mushroom %>%
  filter(stalk.root == "equal")



ggplot(data11,aes(x=stalk.surface.below.ring, y=stalk.shape)) +
  geom_jitter(aes(color = class)) + facet_wrap(vars(odor,cap.color))
#:) eiei






ggplot(data_mushroom,aes(x=cap.color, y=odor)) +
  geom_jitter(aes(color = class))


ggplot(data_mushroom,aes(x=stalk.surface.below.ring, y=stalk.shape)) +
  geom_jitter(aes(color = class)) + facet_wrap(vars(stalk.root))

ggplot(data_mushroom,aes(x=stalk.surface.below.ring, y=odor)) +
  geom_jitter(aes(color = class)) + facet_wrap(vars(stalk.root, stalk.shape))








ggplot(data_mushroom, aes(x=cap.color, y=odor, color = class)) +
  geom_jitter()

ggplot(data_mushroom, aes(x=cap.color, fill = class)) + geom_bar(position = "dodge")



stalk.root +
  stalk.surface.below.ring + odor + cap.color

model4$variable.importance
ggplot(data_mushroom, aes(x = stalk.root, y = cap.color)) +
  geom_jitter(aes(color = class)) + facet_wrap(vars(stalk.surface.below.ring, odor))





ggplot(data_mushroom, aes(color = class)) +
  geom_jitter(aes(x = cap.shape, y = spore.print.color))

ggplot(data_mushroom, aes(x = class)) + geom_bar(aes(fill=odor))
ggplot(data_mushroom, aes(x = class)) + geom_bar(aes(fill=stalk.root))





ggplot(data_mushroom,aes(x=stalk.shape)) +
  geom_bar(aes(fill = class))

ggplot(data_mushroom,aes(x=stalk.shape, y=cap.color)) +
  geom_jitter(aes(color = class))

ggplot(data_mushroom,aes(x=stalk.surface.above.ring)) +
  geom_bar(aes(fill = class)) + facet_wrap(vars(stalk.root,stalk.shape))

ggplot(data_mushroom,aes(x=stalk.surface.above.ring, y=stalk.surface.below.ring)) +
  geom_jitter(aes(color = class)) + facet_wrap(vars(stalk.root,stalk.shape))


ggplot(data_mushroom, aes(x=stalk.shape, fill = class)) + geom_bar() +
  facet_wrap(vars(cap.color))