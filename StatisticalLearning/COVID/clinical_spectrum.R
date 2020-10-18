### Statistical Learning Final Project

library(tidyverse)


#Goal 1: Use unsupervised learning to explore the data.
# eg. Dendrogram, Kmeans, Heirarchical

clinical_spectrum <- read_csv('../clinical_spectrum/clinical-spectrum.csv')

head(clinical_spectrum)


clinical_spectrum_filtered <- clinical_spectrum[,c(1:39)]

clinical_spectrum_filtered <- clinical_spectrum_filtered[,-c(21, 28)]

clinical_spectrum_filtered[,c('influenza_a')] %>%
  na.omit() %>%
  mutate(status = ifelse(influenza_a == 'not_detected', 0, 1)) %>%
  summarize(prop = mean(status))

clinical_spectrum_filtered %>%
  filter(sars_cov_2_exam_result == 'positive') %>%
  summarize(prop_positive= n() / nrow(clinical_spectrum_filtered))



clinical_spectrum_clean <- clinical_spectrum_filtered %>%
  na.omit()

clinical_spectrum_numbers <- clinical_spectrum_clean[,c(7:20)]

clinical_spectrum_clean %>%
  filter(sars_cov_2_exam_result == 'positive')

#scale the clinical spectrum values; this data frame only includes the common objective reported stats
cs_numbers_scaled <- scale(clinical_spectrum_numbers)

#calculate a distance matrix
cs_dist <- dist(cs_numbers_scaled)

hc1 <- hclust(cs_dist)

plot(hc1, cex = .5)

#let's try to clean up the output
install.packages('dendextend')
install.packages('circlize')
library(dendextend)
library(circlize)

#display dendrogram with the positive test results underneath the clusters
hc1 %>%
  as.dendrogram() %>%
  place_labels(clinical_spectrum_clean$sars_cov_2_exam_result) %>%
  set('labels_cex', .3) %>%
  color_branches(k = 4) %>%
  color_unique_labels() %>%
  plot()


#ok let's try to reduce our variables using PCA
pca1 <- prcomp(clinical_spectrum_numbers,
               scale. = TRUE)

#Let's make a plot predicting test result from the other values using PCA
install.packages('pls')
library(pls)

validationplot(pca1)

#first append the test results back onto the numbers data frame
cs_numbers_results <- cbind(clinical_spectrum_numbers, clinical_spectrum_clean$sars_cov_2_exam_result)

cs_for_pca <- cs_numbers_results %>%
  mutate(test_result = ifelse(`clinical_spectrum_clean$sars_cov_2_exam_result` == 'negative', 0, 1))

cs_for_pca <- cs_for_pca[,-c(15)]

pca2 <- pcr(test_result ~ .,
               data = cs_for_pca,
               scale = TRUE)

validationplot(pca2)

biplot(pca1)


km1 <- kmeans(cs_numbers_scaled,
              3)

view(cs_numbers_scaled)

cs_numbers_scaled %>%
  as.data.frame() %>%
  mutate(cluster = km1$cluster) %>%
  ggplot(aes(x = platelets,
             y = hemoglobin)) +
  geom_point(aes(color = factor(cluster)))
 

km2 <- kmeans(cs_for_pca,
              3)  


cs_for_pca %>%
  as.data.frame() %>%
  mutate(cluster = km2$cluster) %>%
  ggplot(aes(x = platelets,
             y = hemoglobin)) +
  geom_point(aes(color = factor(cluster))) +
  geom_point(aes(color = factor(test_result)))


cs_for_pca %>%
  mutate(cluster = km2$cluster) %>%
  group_by(cluster) %>%
  summarize(proportion_positive = mean(test_result),
            cluster_size = n(),
            mean_hematocrit = mean(hematocrit),
            mean_hemoglobin = mean(hemoglobin))


tot <- NULL
for(i in 1:50){
  km <- kmeans(cs_for_pca,
               i)
  
  tot[i] <- km$tot.withinss/i
}

plot(tot)

km3 <- kmeans(cs_for_pca,
        30)

likely_positive_comparison <- cs_for_pca %>%
  mutate(cluster = km3$cluster) %>%
  group_by(cluster) %>%
  summarize(proportion_positive = mean(test_result),
            cluster_size = n(),
            mean_hematocrit = mean(hematocrit),
            mean_hemoglobin = mean(hemoglobin),
            mean_platelets = mean(platelets),
            mean_platelet_volume = mean(mean_platelet_volume),
            mean_rbc = mean(red_blood_cells),
            mean_lymphocytes = mean(lymphocytes),
            mean_corpuscular_hemoglobin_concentration_mchc = mean(mean_corpuscular_hemoglobin_concentration_mchc),
            mean_leukocytes = mean(leukocytes),
            mean_basophils = mean(basophils),
            mean_corpuscular_hemoglobin_mch = mean(mean_corpuscular_hemoglobin_mch),
            mean_eosinophils = mean(eosinophils),
            mean_mcv = mean(mean_corpuscular_volume_mcv),
            mean_monocytes = mean(monocytes),
            mean_rdw = mean(red_blood_cell_distribution_width_rdw)) %>%
  arrange(-proportion_positive) %>%
  mutate(likely_positive = ifelse(proportion_positive>= 0.25, 1, 0)) %>%
  group_by(likely_positive) %>%
  select(-c(cluster, cluster_size)) %>%
  summarize_all(mean)

view(likely_positive_comparison)

gathered_likely_positive_comparison <- likely_positive_comparison %>%
  gather(key = 'likely_positive',
         value = 'blood_comparison',
         -likely_positive) %>%
  mutate(category = ifelse(row_number()%%2 == 0, 1, 0))


gathered_likely_positive_comparison %>%
  ggplot(aes(x = likely_positive,
             y = factor(blood_comparison))) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           aes(fill = category))

gathered_likely_positive_comparison %>%
  ggplot(aes(x = likely_positive,
             y = blood_comparison,
             fill = factor(category))) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  coord_flip()


## Let's try some different models to try to predict positive test results

#first a caret random forest

library(caret)
library(randomForest)
library(ipred)

#cleaning data before implementing random forest
clinical_spectrum_clean_rf <- clinical_spectrum_clean[,-c(1,4:6)]

clinical_spectrum_clean_rf <- clinical_spectrum_clean_rf %>%
  mutate(exam_result = ifelse(exam_result == 'negative', 0, 1))

clinical_spectrum_clean_rf <- clinical_spectrum_clean_rf[,-c(2)]

#test train split
rf_indexes <- sample(nrow(clinical_spectrum_clean_rf),
                     .7*nrow(clinical_spectrum_clean_rf),
                     replace = FALSE)

rf_indexes

rf_train = clinical_spectrum_clean_rf[rf_indexes,]
rf_test = clinical_spectrum_clean_rf[-rf_indexes,]

rf1 <- randomForest(factor(exam_result) ~ .,
                    data = rf_train)

bag1 <- bagging(exam_result ~ .,
                data = clinical_spectrum_clean_rf)

varImpPlot(rf1)
varImpPlot

?train

tune.grid = expand.grid(mtry = c(9, 12, 15),
                        splitrule = c('gini','extratrees'),
                        min.node.size = 1)


ranger1 <- train(factor(exam_result) ~ ., 
            data = rf_train,
            method = "ranger",
            tuneGrid = tune.grid)

ranger1

#create a confusion matrix

predict(ranger1, rf_test)

table(predict(ranger1, rf_test), rf_test$exam_result)

?table

plot(ranger1)





