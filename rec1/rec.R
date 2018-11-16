#Recommendarlab
install.packages("recommenderlab")
library(recommenderlab)
library(countrycode)

set.seed(1)
#1. data load
file_in <- "anonymous-msweb.test"
table <- read.csv(file_in, header=F)
users <- table[, 1:2] #1열과 2열이 사용자 고유번호와, 조회한 웹페이지 번호

head(users)

library(data.table)
table_users <- data.table::data.table(users)
setnames(table_users, 1:2, c("category", "value"))
table_users <- table_users[category %in% c("C","V")] 
#C는 사용자 고유번호가 포함된 행, V는 해당 사용자가 조회한 웹사이트의 영역 번호

#2. Rating Matrix
table_users[, chunk_user := cumsum(category == "C")] #유저의 수 C1....CN
head(table_users,20)

#2.1. 
table_long <- table_users[, list(user = value[1], item = value[-1]), by = "chunk_user"] #사용자 고유번호와 영역번호 분리
table_long[, value := 1]
table_wide <- reshape(data = table_long,
                      direction = "wide",
                      idvar = "user",
                      timevar = "item",
                      v.names = "value")

vector_users <- table_wide[, user]
table_wide[, user := NULL] #유저삭제
table_wide[, chunk_user := NULL] #청크유저 삭제

setnames(table_wide, old = names(table_wide), new = substring(names(table_wide), 7)) #value. <- 이부분 삭제

#3.
matrix_wide <- as.matrix(table_wide)
rownames(matrix_wide) <- vector_users

#3.1
matrix_wide[is.na(matrix_wide)] <- 0
ratings_matrix <- as(matrix_wide, "binaryRatingMatrix")
ratings_matrix

image(ratings_matrix[1:25, 1:25])

#3.2 web site 조회수 분포
n_users <- colCounts(ratings_matrix)
qplot(n_users) + stat_bin(binwidth = 100)

#3.2.1 아웃라이어 제거 plot
qplot(n_users[n_users<100]) + stat_bin(binwidth = 10)


#3.3 아웃라이어 완전 제거
ratings_matrix <- ratings_matrix[, colCounts(ratings_matrix) >= 5]
ratings_matrix #item 236 -> 166
sum(rowCounts(ratings_matrix) == 0) #제거한 영역만 조회한 사람들
ratings_matrix

#3.4 적어도 5개 이상의 영역을 조회한 사람들만 추천하기 위해 전처리
ratings_matrix <- ratings_matrix[rowCounts(ratings_matrix) >= 5, ]


#4 아이템 속성 추출
table <- data.table(table)
table_items <- table[V1 == "A"] #A는 영역을 요약한 정보가 들어가 있음
#######################################
##V2 : 웹사이트 영역번호 ##############
##V4 : 웹사이트 영역 요약 정보 #######
##V5 : 웹사이트 URL ##################

table_items <- table_items[, c(2,4,5), with = FALSE]
setnames(table_items, 1:3, c("id","description","url"))
table_items <- table_items[order(id)]
table_items

#4.1 category 추가 - 공통된값(product)
table_items[, category := "product"]
library(countrycode)




name_countries <- c(countrycode::codelist$country.name.en, "Taiwan","UK","Russia","Venezuela","Slovenija","Caribbean","Central America","Ms North Afroca")
table_items[description %in% name_countries, category := "region"]
table_items[grepl("Region",description), category := "region"] #정규표현식 이용해서 요약에 Region 들어가면 카테고리 region으로
table_items$category %>% table()


#5. 추천 모델 만들기
sample <- sample(1:2,
                    size = nrow(ratings_matrix),
                    replace = TRUE, prob = c(0.7,0.3))
recc_train_set <- ratings_matrix[sample == 1, ]
recc_test_set <- ratings_matrix[sample == 2,]

recc_model <- Recommender(data = recc_train_set,
                          method = "IBCF",
                          parameter = list(method = "Jaccard")) #거리함수
image(recc_model@model$sim) #패턴 발견이 쉽지가 않음
range(recc_model@model$sim) #패턴 발견을 위해 범위확인 0~0.67

#5.1 recc_model 을 이용해서 아이템 간의 조회 유사도 매트릭스 
dist_ratings <- as(recc_model@model$sim, "matrix") 
dist_category <- table_items[, 0.67 - dist(category == "product")] #두영역이 같은 카테고리 0.67 그외 0

dist_category <- as(dist_category, "matrix")

rownames(dist_category) <- table_items[, id]
colnames(dist_category) <- table_items[, id]

vector_items <- rownames(dist_ratings)
dist_category <- dist_category[vector_items, vector_items]

identical(dim(dist_category), dim(dist_ratings))
identical(rownames(dist_category), rownames(dist_ratings))
identical(colnames(dist_category), colnames(dist_ratings))

image(dist_category)

#5.2 테이블 결합 : 결합 방식은 두 매트릭스 간 평점의 가중평균
weight_category <- 0.25
dist_tot <- dist_category * weight_category + dist_ratings * (1 - weight_category)
image(dist_tot) #유사한 점들은 노란색으로

recc_model@model$sim <- as(dist_tot, "dgCMatrix")

#5.3 recommend
n_recommended <- 20
recc_predicted <- predict(object = recc_model,
                          newdata = recc_test_set,
                          n = n_recommended)
recc_predicted@itemLabels #item name


recc_predicted@items[[1]]
p170
