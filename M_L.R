[1] "C:/UCI Har Dataset"
> 
> library(caret)
> library(randomForest)
> library(rpart)
> library(rpart.plot)
> set.seed(1234)
> train <- read.csv("/UCI HAR Dataset/train.csv", na.strings=c("NA","#DIV/0!", ""))
> 
> test <- read.csv("/UCI HAR Dataset/test.csv", na.strings=c("NA","#DIV/0!", ""))
> 
> dim(train)
[1] 19622   160
> dim(test)
[1]  20 160
> 
> train <- train[, colSums(is.na(train)) == 0]
> test <-test[, colSums(is.na(test)) == 0]
> 
> train <-train[, -c(1:7)]
> test <-test[, -c(1:7)]
> 
> dim(train)
[1] 19622    53
> head(train)
  roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z
1      1.41       8.07    -94.4                3         0.00         0.00        -0.02          -21            4           22            -3           599          -313
2      1.41       8.07    -94.4                3         0.02         0.00        -0.02          -22            4           22            -7           608          -311
3      1.42       8.07    -94.4                3         0.00         0.00        -0.02          -20            5           23            -2           600          -305
4      1.48       8.05    -94.4                3         0.02         0.00        -0.03          -22            3           21            -6           604          -310
5      1.48       8.07    -94.4                3         0.02         0.02        -0.02          -21            2           24            -6           600          -302
6      1.45       8.06    -94.4                3         0.02         0.00        -0.02          -21            4           21             0           603          -312
  roll_arm pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell
1     -128      22.5    -161              34        0.00        0.00       -0.02        -288         109        -123         -368          337          516      13.05217
2     -128      22.5    -161              34        0.02       -0.02       -0.02        -290         110        -125         -369          337          513      13.13074
3     -128      22.5    -161              34        0.02       -0.02       -0.02        -289         110        -126         -368          344          513      12.85075
4     -128      22.1    -161              34        0.02       -0.03        0.02        -289         111        -123         -372          344          512      13.43120
5     -128      22.1    -161              34        0.00       -0.03        0.00        -289         111        -123         -374          337          506      13.37872
6     -128      22.0    -161              34        0.02       -0.03        0.00        -289         111        -122         -369          342          513      13.38246
  pitch_dumbbell yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
1      -70.49400    -84.87394                   37                0            -0.02             0.00             -234               47             -271              -559
2      -70.63751    -84.71065                   37                0            -0.02             0.00             -233               47             -269              -555
3      -70.27812    -85.14078                   37                0            -0.02             0.00             -232               46             -270              -561
4      -70.39379    -84.87363                   37                0            -0.02            -0.02             -232               48             -269              -552
5      -70.42856    -84.85306                   37                0            -0.02             0.00             -233               48             -270              -554
6      -70.81759    -84.46500                   37                0            -0.02             0.00             -234               48             -269              -558
  magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y gyros_forearm_z accel_forearm_x
1               293               -65         28.4         -63.9        -153                  36            0.03            0.00           -0.02             192
2               296               -64         28.3         -63.9        -153                  36            0.02            0.00           -0.02             192
3               298               -63         28.3         -63.9        -152                  36            0.03           -0.02            0.00             196
4               303               -60         28.1         -63.9        -152                  36            0.02           -0.02            0.00             189
5               292               -68         28.0         -63.9        -152                  36            0.02            0.00           -0.02             189
6               294               -66         27.9         -63.9        -152                  36            0.02           -0.02           -0.03             193
  accel_forearm_y accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
1             203            -215              -17              654              476      A
2             203            -216              -18              661              473      A
3             204            -213              -18              658              469      A
4             206            -214              -16              658              469      A
5             206            -214              -17              655              473      A
6             203            -215               -9              660              478      A
> dim(test)
[1] 20 53
> head(test)
  roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z
1    123.00      27.00    -4.75               20        -0.50        -0.02        -0.46          -38           69         -179           -13           581          -382
2      1.02       4.87   -88.90                4        -0.06        -0.02        -0.07          -13           11           39            43           636          -309
3      0.87       1.82   -88.50                5         0.05         0.02         0.03            1           -1           49            29           631          -312
4    125.00     -41.60   162.00               17         0.11         0.11        -0.16           46           45         -156           169           608          -304
5      1.35       3.33   -88.60                3         0.03         0.02         0.00           -8            4           27            33           566          -418
6     -5.92       1.59   -87.70                4         0.10         0.05        -0.13          -11          -16           38            31           638          -291
  roll_arm pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell
1     40.7    -27.80     178              10       -1.65        0.48       -0.18          16          38          93         -326          385          481     -17.73748
2      0.0      0.00       0              38       -1.17        0.85       -0.43        -290         215         -90         -325          447          434      54.47761
3      0.0      0.00       0              44        2.10       -1.36        1.13        -341         245         -87         -264          474          413      57.07031
4   -109.0     55.00    -142              25        0.22       -0.51        0.92        -238         -57           6         -173          257          633      43.10927
5     76.1      2.76     102              29       -1.96        0.79       -0.54        -197         200         -30         -170          275          617    -101.38396
6      0.0      0.00       0              14        0.02        0.05       -0.07         -26         130         -19          396          176          516      62.18750
  pitch_dumbbell yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
1       24.96085    126.23596                    9             0.64             0.06            -0.61               21              -15               81               523
2      -53.69758    -75.51480                   31             0.34             0.05            -0.71             -153              155             -205              -502
3      -51.37303    -75.20287                   29             0.39             0.14            -0.34             -141              155             -196              -506
4      -30.04885   -103.32003                   18             0.10            -0.02             0.05              -51               72             -148              -576
5      -53.43952    -14.19542                    4             0.29            -0.47            -0.46              -18              -30               -5              -424
6      -50.55595    -71.12063                   29            -0.59             0.80             1.10             -138              166             -186              -543
  magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y gyros_forearm_z accel_forearm_x
1              -528               -56          141         49.30       156.0                  33            0.74           -3.34           -0.59            -110
2               388               -36          109        -17.60       106.0                  39            1.12           -2.78           -0.18             212
3               349                41          131        -32.60        93.0                  34            0.18           -0.79            0.28             154
4               238                53            0          0.00         0.0                  43            1.38            0.69            1.80             -92
5               252               312         -176         -2.16       -47.9                  24           -0.75            3.10            0.80             131
6               262                96          150          1.46        89.7                  43           -0.88            4.26            1.35             230
  accel_forearm_y accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z problem_id
1             267            -149             -714              419              617          1
2             297            -118             -237              791              873          2
3             271            -129              -51              698              783          3
4             406             -39             -233              783              521          4
5             -93             172              375             -787               91          5
6             322            -144             -300              800              884          6
> sub_samples <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
> sub_train <- train[sub_samples, ] 
> sub_test <- train[-sub_samples, ]
> 
> dim(sub_train)
[1] 14718    53
> head(sub_train)
  roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z
2      1.41       8.07    -94.4                3         0.02         0.00        -0.02          -22            4           22            -7           608          -311
3      1.42       8.07    -94.4                3         0.00         0.00        -0.02          -20            5           23            -2           600          -305
4      1.48       8.05    -94.4                3         0.02         0.00        -0.03          -22            3           21            -6           604          -310
5      1.48       8.07    -94.4                3         0.02         0.02        -0.02          -21            2           24            -6           600          -302
6      1.45       8.06    -94.4                3         0.02         0.00        -0.02          -21            4           21             0           603          -312
7      1.42       8.09    -94.4                3         0.02         0.00        -0.02          -22            3           21            -4           599          -311
  roll_arm pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell
2     -128      22.5    -161              34        0.02       -0.02       -0.02        -290         110        -125         -369          337          513      13.13074
3     -128      22.5    -161              34        0.02       -0.02       -0.02        -289         110        -126         -368          344          513      12.85075
4     -128      22.1    -161              34        0.02       -0.03        0.02        -289         111        -123         -372          344          512      13.43120
5     -128      22.1    -161              34        0.00       -0.03        0.00        -289         111        -123         -374          337          506      13.37872
6     -128      22.0    -161              34        0.02       -0.03        0.00        -289         111        -122         -369          342          513      13.38246
7     -128      21.9    -161              34        0.00       -0.03        0.00        -289         111        -125         -373          336          509      13.12695
  pitch_dumbbell yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
2      -70.63751    -84.71065                   37                0            -0.02             0.00             -233               47             -269              -555
3      -70.27812    -85.14078                   37                0            -0.02             0.00             -232               46             -270              -561
4      -70.39379    -84.87363                   37                0            -0.02            -0.02             -232               48             -269              -552
5      -70.42856    -84.85306                   37                0            -0.02             0.00             -233               48             -270              -554
6      -70.81759    -84.46500                   37                0            -0.02             0.00             -234               48             -269              -558
7      -70.24757    -85.09961                   37                0            -0.02             0.00             -232               47             -270              -551
  magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y gyros_forearm_z accel_forearm_x
2               296               -64         28.3         -63.9        -153                  36            0.02            0.00           -0.02             192
3               298               -63         28.3         -63.9        -152                  36            0.03           -0.02            0.00             196
4               303               -60         28.1         -63.9        -152                  36            0.02           -0.02            0.00             189
5               292               -68         28.0         -63.9        -152                  36            0.02            0.00           -0.02             189
6               294               -66         27.9         -63.9        -152                  36            0.02           -0.02           -0.03             193
7               295               -70         27.9         -63.9        -152                  36            0.02            0.00           -0.02             195
  accel_forearm_y accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
2             203            -216              -18              661              473      A
3             204            -213              -18              658              469      A
4             206            -214              -16              658              469      A
5             206            -214              -17              655              473      A
6             203            -215               -9              660              478      A
7             205            -215              -18              659              470      A
> dim(sub_test)
[1] 4904   53
> head(sub_test)
   roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z
1       1.41       8.07    -94.4                3         0.00         0.00        -0.02          -21            4           22            -3           599          -313
21      1.60       8.10    -94.4                3         0.02         0.00        -0.02          -20            1           20           -10           607          -304
22      1.57       8.09    -94.4                3         0.02         0.02        -0.02          -21            3           21            -2           604          -313
23      1.56       8.10    -94.3                3         0.02         0.00        -0.02          -21            4           21            -4           606          -311
25      1.53       8.11    -94.4                3         0.03         0.00         0.00          -19            4           21            -8           605          -319
26      1.55       8.09    -94.4                3         0.02         0.00         0.00          -21            3           22           -10           601          -312
   roll_arm pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell
1      -128      22.5    -161              34        0.00        0.00       -0.02        -288         109        -123         -368          337          516      13.05217
21     -129      20.9    -161              34        0.03       -0.02       -0.02        -288         111        -124         -375          337          513      13.38246
22     -129      20.8    -161              34        0.03       -0.02       -0.02        -289         111        -123         -372          338          510      13.37872
23     -129      20.7    -161              34        0.02       -0.02       -0.02        -290         110        -123         -373          333          509      13.35451
25     -129      20.7    -161              34       -0.02       -0.02        0.00        -289         109        -123         -370          340          512      13.05217
26     -129      20.7    -161              34       -0.02       -0.02       -0.02        -290         108        -123         -366          346          511      12.80060
   pitch_dumbbell yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
1       -70.49400    -84.87394                   37                0            -0.02             0.00             -234               47             -271              -559
21      -70.81759    -84.46500                   37                0            -0.02             0.00             -234               48             -269              -554
22      -70.42856    -84.85306                   37                0            -0.02             0.00             -233               48             -270              -554
23      -70.63995    -84.64919                   37                0            -0.02             0.00             -234               48             -270              -557
25      -70.49400    -84.87394                   37                0            -0.02             0.00             -234               47             -271              -555
26      -70.31305    -85.11886                   37                0            -0.02            -0.02             -233               46             -271              -563
   magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y gyros_forearm_z accel_forearm_x
1                293               -65         28.4         -63.9        -153                  36            0.03            0.00           -0.02             192
21               299               -72         26.9         -63.9        -151                  36            0.03           -0.03           -0.02             194
22               301               -65         27.0         -63.9        -151                  36            0.02           -0.03           -0.02             191
23               294               -69         26.9         -63.8        -151                  36            0.02           -0.02           -0.02             194
25               290               -68         27.1         -63.7        -151                  36            0.05           -0.03            0.00             191
26               294               -72         27.0         -63.7        -151                  36            0.03            0.00            0.00             190
   accel_forearm_y accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
1              203            -215              -17              654              476      A
21             208            -214              -11              654              469      A
22             206            -213              -17              654              478      A
23             206            -214              -10              653              467      A
25             202            -214              -14              667              470      A
26             203            -216              -16              658              462      A
> plot(sub_train$classe, col="gold", xlab="Classe levels", ylab="Frequency")
> model_1 <- rpart (classe ~ ., data=sub_train, method="class")
> 
> prediction_1 <- predict(model_1, sub_test, type = "class")
> 
> rpart.plot(model_1, main="Decision Tree", extra=102, under=TRUE, faclen=0)
> confusionMatrix(prediction_1, sub_test$classe)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1235  157   16   50   20
         B   55  568   73   80  102
         C   44  125  690  118  116
         D   41   64   50  508   38
         E   20   35   26   48  625

Overall Statistics
                                          
               Accuracy : 0.7394          
                 95% CI : (0.7269, 0.7516)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.6697          
 Mcnemar's Test P-Value : < 2.2e-16       

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.8853   0.5985   0.8070   0.6318   0.6937
Specificity            0.9307   0.9216   0.9005   0.9529   0.9678
Pos Pred Value         0.8356   0.6469   0.6313   0.7247   0.8289
Neg Pred Value         0.9533   0.9054   0.9567   0.9296   0.9335
Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
Detection Rate         0.2518   0.1158   0.1407   0.1036   0.1274
Detection Prevalence   0.3014   0.1790   0.2229   0.1429   0.1538
Balanced Accuracy      0.9080   0.7601   0.8537   0.7924   0.8307
> model_2 <- randomForest(classe ~. , data=sub_train, method="class")
> 
> prediction_2 <- predict(model_2, sub_test, type = "class")
> 
> confusionMatrix(prediction_2, sub_test$classe)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1395    2    0    0    0
         B    0  945    9    0    0
         C    0    2  844    6    0
         D    0    0    2  798    0
         E    0    0    0    0  901

Overall Statistics
                                          
               Accuracy : 0.9957          
                 95% CI : (0.9935, 0.9973)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9946          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.9958   0.9871   0.9925   1.0000
Specificity            0.9994   0.9977   0.9980   0.9995   1.0000
Pos Pred Value         0.9986   0.9906   0.9906   0.9975   1.0000
Neg Pred Value         1.0000   0.9990   0.9973   0.9985   1.0000
Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
Detection Rate         0.2845   0.1927   0.1721   0.1627   0.1837
Detection Prevalence   0.2849   0.1945   0.1737   0.1631   0.1837
Balanced Accuracy      0.9997   0.9968   0.9926   0.9960   1.0000
> predict_outcome <- predict(model_2, test, type="class")
> predict_outcome
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E

> setwd ("/UCI Har Dataset")
> 
> pml_write_files = function(x){
+   n = length(x)
+   for(i in 1:n){
+     filename = paste0("problem_id_", i, ".txt")
+     write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names = FALSE) }
+ }
> 
> pml_write_files(predict_outcome)
> 
