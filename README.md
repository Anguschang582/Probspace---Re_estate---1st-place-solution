# Probspace---Re_estate---1st-place-solution

https://prob.space/competitions/re_real_estate_2020/ranking

1st solution of "Re：不動産取引価格予測" competition on Probspace


1. Run `function.R`
2. Run `preprocessing.R`
3. Run `lgbm_psudeo_labeling.R`
4. Run `get_importance.R`, this will generate the feature importance (impp.csv)
5. Run `model.R`

# Solution
### About me
Currently, I am a Master's student at Tokyo Tech. For further information, please refer to my [Kaggle Profile](https://www.kaggle.com/andrew60909).

### Summary
My final score (Public: 0.25848 / Private: 0.25854 / CV:0.261616) is based on a single LGBM (5 fold bagging) by select top700 features from lgb feature importance. I only use Lightgbm here since it is good for dealing with tabular data and relatively fast compared to Xgboost and Catboost, which allow you to test more ideas in limited time. For the validation scheme, I simply use 5-Fold cross-validation and it works very well, CV score always aligns with the LB score.  

### Feature engineering
The data is a little bit dirty, but compared to data from Signate student cup 2019, it was not a problem at all for me. I just spent some time on transforming them from 全角 to 半角, then separating them into the single feature so that we can do some feature engineering on it.

My whole FE is composed of 6 parts : 

- Group method (numeric2cate) : Apply statistics of numeric features in different categorical features group. For example, applying "**mean**" on "**面積（㎡）**" group by "**市区町村コード**".
    The statistics functions I used :
    + Mean, max, min, std, sum, skewness, kurtosis
    + Bayes mean
    + IQR : q75 - q25 
    + IQR_ratio : q75 / q25
    + Median absolute deviation : median( abs(x - median(x)) )
    + Mean variance : std(x) / mean(x)
    + hl_ratio : The ratio of numbers of the samples that higher and lower than the mean [(Ref, Table 2)](https://arxiv.org/pdf/1801.07323.pdf).
    + MAD : Median Absolute Deviation : median( |x - median(x)| )
    + Beyond1std : Calculating the ratio beyond 1 std
    + Range : max - min
    + Range_ratio : max / min
    + Shapiro-Wilk Statistic 
    + diff and ratio : "x - mean(x)" or "x / mean(x)"
    + Z-score : ( x-mean(x) ) / std(x)

- Group method (cate2cate) : Apply statistics of categorical features in different categorical features group. For example, applying "**entropy**" on the frequency table of "**最寄駅：名称**" group by "**市区町村コード**".
  The statistics functions I used :
     + n_distinct : number of unique
     + Entropy : apply entropy on frequency table
     + freq1name : the number of most frequently appeared category
     + freq1ratio : the number of most frequently appeared category / group size

- Target encoding : [Reference](http://helios.mm.di.uoa.gr/~rouvas/ssi/sigkdd/sigkdd.vol3.1/barreca.pdf) and [code](https://www.kaggle.com/brandenkmurray/it-is-lit)

- Count encoding : This works very well on some categorical features like "**取引の事情等**"

- Feature from land_price.csv : Making features by 2 different "Group method" that I have mentioned above. Applying the statistics on the features that is grouped by "**所在地コード**", then just merge it to our train+test data

- Feature pseudo-labeling : Build a LGBM model to predict the important features (I used "**sq_meter**", "**land__mean__ON__h31_price**", "**nobeyuka_m2**", "**Age of house**","**time_to_nearest_aki**"), and then take the oof predictions. 


### Hyper-parameter
Suprisingly that tuning "alpha" in huber loss give me really a big boost (~0.001). In huber loss, alpha=1 basically means absolute loss (same formula). So if we lower the alpha value, it will make your model less sensitive to those "outlier cases".

```
lgb_param <- list(boosting_type = 'gbdt',
                  objective = "huber",
                  boost_from_average = 'false',
                  metric = "none",
                  learning_rate = 0.008,
                  num_leaves = 128,
                  #  min_gain_to_split = 0.01,
                  feature_fraction = 0.05,
                  #  feature_fraction_seed = 666666,
                  bagging_freq = 1,
                  bagging_fraction = 1,
                  min_sum_hessian_in_leaf = 5,
                  #  min_data_in_leaf = 100,
                  lambda_l1 = 0,
                  lambda_l2 = 0,
                  alpha = 0.3
                  )
```


&nbsp;
