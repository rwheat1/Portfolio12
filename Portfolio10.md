Portfolio 10 – NBA Playoffs
================
Ryan Wheat
05/08/2023

## Background

It’s cliche in the sports world to joke that the NBA playoffs are a
completely different sport from the Regular Season, because the quality
of basketball is regarded to be much higher. There are typically two
points made to support this. (1) Players generally play much harder when
the stakes are higher, and (2) the “great” players tend to rise to the
challenge.

Indeed, the playoffs seem to establish who the best players in the
league truly are, because there are many playoff games one could point
to in which an NBA legend refused to let their team lose. But in the
regular season, the stars tend not to will their teams to victory nearly
as often.

These perceived differences in basketball play between the playoffs and
the regular season make playoff basketball substantially more fun to
watch – but does empirical evidence support these ideas? That is, do the
great players actually perform *better* in the playoffs, consistent with
the lay theories? To my knowledge, this question has not faced empirical
scrutiny.

To investigate this question, I’ve downloaded data from
fivethirtyeight.com containing their advanced analytics on all NBA
players from 2014-present. Specifically, they have a metric known as
“RAPTOR,” which goes beyond typical box score numbers (e.g., how many
points, rebounds, assists, etc. a player gets) to to quantify how “good”
a basketball player is. That is, some basketball players play well (or
poorly) in ways that do not necessarily show up in traditional box score
statistics – and RAPTOR accounts for these things.

In any case, I am going to split this file up between the regular season
and the playoffs, and see if any differences quality of performance
emerge as a function of their performance during the regular season.

## Data Cleaning

First, I need to filter the data so that each player has both (1) data
from the regular season, and (2) data from the playoffs for a given NBA
season.

``` r
#sort data so that each player included has data from the regular season and playoffs for a given year

RAPTOR_filtered <- RAPTOR %>%
  group_by(player_id, season) %>%
  mutate(filter_by = if_else(length(season) > 1 & length(season) <= 2, 1, 0)) %>%
  ungroup()

RAPTOR_filtered <- RAPTOR_filtered %>%
  group_by(player_id, season) %>%
  filter(filter_by == 1) %>%
  ungroup()

#there are some people who were on multiple teams in a given season that did not make the playoffs. Let's filter those players out.

RAPTOR_filtered <- RAPTOR_filtered %>%
  group_by(player_id, season) %>%
  filter("PO" %in% season_type) %>%
  ungroup()

RAPTOR_filtered %>%
  count(season_type)
```

    ## # A tibble: 2 × 2
    ##   season_type     n
    ##   <chr>       <int>
    ## 1 PO           1702
    ## 2 RS           1702

Okay, we’ve now got a dataset of players who played for a given team in
a given year during both the regular season and the playoffs.

Let’s now do a median split on total_RAPTOR to seperate the “good”
players from the “bad” players. We’ll also seperate by +-1 standard
deviation on total_RAPTOR.

``` r
#regular season and playoff data

RAPTOR_rs <- RAPTOR_filtered %>%
  filter(season_type == "RS")

RAPTOR_po <- RAPTOR_filtered %>%
  filter(season_type == "PO")

#median split

RAPTOR_median_rs <- RAPTOR_rs %>%
  mutate(median_split = case_when(raptor_total > median(raptor_total) ~ "Good Player",
                                  raptor_total < median(raptor_total) ~ "Bad Player",
                                  raptor_total == median(raptor_total) ~ "Average Player"))

RAPTOR_median_po <- RAPTOR_po %>%
  mutate(median_split = case_when(raptor_total > median(raptor_total) ~ "Good Player",
                                  raptor_total < median(raptor_total) ~ "Bad Player",
                                  raptor_total == median(raptor_total) ~ "Average Player"))


#+/- 1 SD split

raptor_mean <- mean(RAPTOR_filtered$raptor_total)
raptor_sd <- sd(RAPTOR_filtered$raptor_total)

RAPTOR_SD_rs <- RAPTOR_rs %>%
  mutate(sd_split = case_when(raptor_total > raptor_mean + raptor_sd ~ "Good Player",
                              raptor_total < raptor_mean - raptor_sd ~ "Bad Player",
                              .default = "Average Player"))

RAPTOR_SD_po <- RAPTOR_po %>%
  mutate(sd_split = case_when(raptor_total > raptor_mean + raptor_sd ~ "Good Player",
                              raptor_total < raptor_mean - raptor_sd ~ "Bad Player",
                              .default = "Average Player"))

RAPTOR_median_rs %>%
  count(median_split)
```

    ## # A tibble: 2 × 2
    ##   median_split     n
    ##   <chr>        <int>
    ## 1 Bad Player     851
    ## 2 Good Player    851

``` r
#put the regular season evaluations into the playoff datasets

RAPTOR_median_rs %>%
  arrange(player_id)
```

    ## # A tibble: 1,702 × 25
    ##    player_name  playe…¹ season seaso…² team   poss    mp rapto…³ rapto…⁴ rapto…⁵
    ##    <chr>        <chr>    <dbl> <chr>   <chr> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Alex Abrines abrina…   2017 RS      OKC    2215  1055   0.771  -0.180   0.591
    ##  2 Alex Abrines abrina…   2018 RS      OKC    2313  1134   0.236  -1.72   -1.48 
    ##  3 Precious Ac… achiup…   2021 RS      MIA    1557   737  -3.88    1.17   -2.70 
    ##  4 Precious Ac… achiup…   2022 RS      TOR    3482  1725  -2.44    2.03   -0.412
    ##  5 Jordan Adams adamsj…   2015 RS      MEM     498   248   1.96   -0.572   1.39 
    ##  6 Steven Adams adamss…   2014 RS      OKC    2420  1197  -1.12    0.765  -0.355
    ##  7 Steven Adams adamss…   2016 RS      OKC    4110  2014   0.800   4.96    5.76 
    ##  8 Steven Adams adamss…   2017 RS      OKC    4950  2389  -2.24    1.67   -0.568
    ##  9 Steven Adams adamss…   2018 RS      OKC    5121  2487   0.509   1.34    1.85 
    ## 10 Steven Adams adamss…   2019 RS      OKC    5830  2669  -0.981   2.02    1.04 
    ## # … with 1,692 more rows, 15 more variables: raptor_onoff_offense <dbl>,
    ## #   raptor_onoff_defense <dbl>, raptor_onoff_total <dbl>, raptor_offense <dbl>,
    ## #   raptor_defense <dbl>, raptor_total <dbl>, war_total <dbl>,
    ## #   war_reg_season <dbl>, war_playoffs <dbl>, predator_offense <dbl>,
    ## #   predator_defense <dbl>, predator_total <dbl>, pace_impact <dbl>,
    ## #   filter_by <dbl>, median_split <chr>, and abbreviated variable names
    ## #   ¹​player_id, ²​season_type, ³​raptor_box_offense, ⁴​raptor_box_defense, …

``` r
RAPTOR_SD_rs %>%
  arrange(player_id)
```

    ## # A tibble: 1,702 × 25
    ##    player_name  playe…¹ season seaso…² team   poss    mp rapto…³ rapto…⁴ rapto…⁵
    ##    <chr>        <chr>    <dbl> <chr>   <chr> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Alex Abrines abrina…   2017 RS      OKC    2215  1055   0.771  -0.180   0.591
    ##  2 Alex Abrines abrina…   2018 RS      OKC    2313  1134   0.236  -1.72   -1.48 
    ##  3 Precious Ac… achiup…   2021 RS      MIA    1557   737  -3.88    1.17   -2.70 
    ##  4 Precious Ac… achiup…   2022 RS      TOR    3482  1725  -2.44    2.03   -0.412
    ##  5 Jordan Adams adamsj…   2015 RS      MEM     498   248   1.96   -0.572   1.39 
    ##  6 Steven Adams adamss…   2014 RS      OKC    2420  1197  -1.12    0.765  -0.355
    ##  7 Steven Adams adamss…   2016 RS      OKC    4110  2014   0.800   4.96    5.76 
    ##  8 Steven Adams adamss…   2017 RS      OKC    4950  2389  -2.24    1.67   -0.568
    ##  9 Steven Adams adamss…   2018 RS      OKC    5121  2487   0.509   1.34    1.85 
    ## 10 Steven Adams adamss…   2019 RS      OKC    5830  2669  -0.981   2.02    1.04 
    ## # … with 1,692 more rows, 15 more variables: raptor_onoff_offense <dbl>,
    ## #   raptor_onoff_defense <dbl>, raptor_onoff_total <dbl>, raptor_offense <dbl>,
    ## #   raptor_defense <dbl>, raptor_total <dbl>, war_total <dbl>,
    ## #   war_reg_season <dbl>, war_playoffs <dbl>, predator_offense <dbl>,
    ## #   predator_defense <dbl>, predator_total <dbl>, pace_impact <dbl>,
    ## #   filter_by <dbl>, sd_split <chr>, and abbreviated variable names ¹​player_id,
    ## #   ²​season_type, ³​raptor_box_offense, ⁴​raptor_box_defense, ⁵​raptor_box_total

``` r
RAPTOR_po <- RAPTOR_po %>%
  arrange(player_id) %>%
  mutate(reg_median_split = RAPTOR_median_rs$median_split) %>%
  mutate(reg_sd_split = RAPTOR_SD_rs$sd_split) %>%
  mutate(reg_raptor = RAPTOR_SD_rs$raptor_total)
```

## Main Analyses

Ok, now that we’ve set that up, I’m going to run a series of regression
models. If I were doing a full thing for this, I would probably run
something to account for potential lack of independent observations
(e.g., players nested within teams). But I’m not going to do all that
today.

Instead, I will see if the dichotomous variable of season_type predicts
total_raptor. In other words: do players generally play better or worse
in the playoffs?

``` r
#create variable to show that the same person is 

po_v_rs <- lm(raptor_total ~ season_type, RAPTOR_filtered)

summary(po_v_rs)
```

    ## 
    ## Call:
    ## lm(formula = raptor_total ~ season_type, data = RAPTOR_filtered)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -60.189  -2.471   0.336   3.038 104.149 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)    -0.4217     0.1753  -2.406   0.0162 *
    ## season_typeRS   0.3374     0.2479   1.361   0.1735  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.23 on 3402 degrees of freedom
    ## Multiple R-squared:  0.0005444,  Adjusted R-squared:  0.0002506 
    ## F-statistic: 1.853 on 1 and 3402 DF,  p-value: 0.1735

The lack of signficant results suggests that the playoffs vs. regular
season distinction doesn’t matter. That is, players do not generally
play better or worse in the playoffs than they do in the regular season.

Next, I will run a regression model to see whether each player’s playoff
total_raptor can be predicted by their regular season total_raptor. In
other words, does how well a player plays in the regular season predict
how well they play in the playoffs? If RAPTOR is a valid measure of
player quality, I expect this to be a strong relationship.

``` r
reg_po_raptor <- lm(raptor_total ~ reg_raptor, RAPTOR_po)

summary(reg_po_raptor)
```

    ## 
    ## Call:
    ## lm(formula = raptor_total ~ reg_raptor, data = RAPTOR_po)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -56.996  -3.718   0.471   4.053 103.941 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.38333    0.22519  -1.702   0.0889 .  
    ## reg_raptor   0.45485    0.05776   7.874 6.06e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.288 on 1700 degrees of freedom
    ## Multiple R-squared:  0.03519,    Adjusted R-squared:  0.03462 
    ## F-statistic:    62 on 1 and 1700 DF,  p-value: 6.06e-15

Good – the higher a player’s regular season RAPTOR, the higher their
postseason RAPTOR in a given year. You would hope this would be the case
if a metric such as RAPTOR was supposed to be a good measure of how good
a basketball player was.

Now, I’m going to create a difference score between postseason and
regular season RAPTOR to see if being good (and bad) in the regular
season predicts *change* in how good (or bad) a player is in the
playoffs.

Positive values on this metric indicate that a player got BETTER from
the regular season to the playoffs. Negative values indicate that a
player got WORSE, instead.

There are two competing hypotheses, here. One possibility is that being
bad in the regular season will predict improving in the playoffs,
because of a floor effect. This seems unlikely to me. The other
possibility is that being good (or bad) in the regular season predicts
getting better (or worse) in the playoffs, consistent with lay
conceptions.

``` r
#create variable

RAPTOR_po <- RAPTOR_po %>%
  mutate(raptor_change = raptor_total - reg_raptor)

po_change_m <- lm(raptor_change ~ reg_median_split, RAPTOR_po)

po_change_sd <- lm(raptor_change ~ reg_sd_split, RAPTOR_po)

summary(po_change_m)
```

    ## 
    ## Call:
    ## lm(formula = raptor_change ~ reg_median_split, data = RAPTOR_po)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -56.973  -3.944   0.205   3.839 104.450 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                   0.4213     0.3256   1.294    0.196   
    ## reg_median_splitGood Player  -1.5175     0.4604  -3.296    0.001 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.498 on 1700 degrees of freedom
    ## Multiple R-squared:  0.006349,   Adjusted R-squared:  0.005764 
    ## F-statistic: 10.86 on 1 and 1700 DF,  p-value: 0.001002

``` r
summary(po_change_sd)
```

    ## 
    ## Call:
    ## lm(formula = raptor_change ~ reg_sd_split, data = RAPTOR_po)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -59.121  -3.793   0.504   4.055 104.025 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              -0.6709     0.2331  -2.878  0.00405 ** 
    ## reg_sd_splitBad Player   10.8843     1.2349   8.814  < 2e-16 ***
    ## reg_sd_splitGood Player  -1.6200     1.3930  -1.163  0.24501    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.315 on 1699 degrees of freedom
    ## Multiple R-squared:  0.04484,    Adjusted R-squared:  0.04372 
    ## F-statistic: 39.88 on 2 and 1699 DF,  p-value: < 2.2e-16

This is interesting. “Good players,” identified as those above the
median in RAPTOR, seem to get worse in the playoffs. “Bad players,” or
those below the median, seem to improve.

This also seems to be the case if doing a split at +/- 1 SD (instead of
the median). That is, “good” players are predicted to have worse RAPTOR
in the playoffs than the regular season compared to average players, and
“bad” players are predicted to have a better raptor in the playoffs than
they did in the regular season compared to average players.

There are a lot of things that could be happening here, and I’m sure
there is a way to unpack the data that better answers the question of
interest. There are also a number of limitations to the analyses I’ve
run, especially regarding the way I’ve operationalized a “good”
basketball player for this portfolio.

However, taking these results at face value, the empirical evidence I’ve
gathered goes against popular wisdom: good players don’t seem to
systematically improve in the NBA playoffs. In fact, they seem to get
worse.
