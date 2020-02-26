Loading packages
================

SOCIAL ENGAGEMENT
=================

(3-4 sentences)

Question: Most studies have been focused on social observation, but is
there a difference between social observation and social engagement? -
investigated with eyetracking.

Importance: theoretical discussion - are tasks i general saying what
they want to say? Arguably, knowing what social observation studies say
about real engagement has implication when understanding the results
from other studies.

H: pupil size will increase more, when you feel social engaged compared
to social observing --&gt; Engagement gives increased pupil size
(physiological arousal)

### Import data and create relevant columns

    # Import sample data
    samples <- read_csv("C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv") %>% 
      mutate(GazeY = 1051-GazeY, Fix_MeanY = 1051-Fix_MeanY) %>% 
      filter(Time<=41202)

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   ParticipantID = col_character(),
    ##   ParticipantGender = col_character(),
    ##   EyeTracked = col_character(),
    ##   Task = col_character(),
    ##   ForagingType = col_character(),
    ##   Stimulus = col_character(),
    ##   Video = col_logical(),
    ##   Sac_Blink = col_logical(),
    ##   Sac_Direction = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 154090 parsing failures.
    ##    row   col           expected         actual                                                                                                                          file
    ## 681701 Video 1/0/T/F/TRUE/FALSE m_pl_o1_div_+o 'C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv'
    ## 681702 Video 1/0/T/F/TRUE/FALSE m_pl_o1_div_+o 'C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv'
    ## 681703 Video 1/0/T/F/TRUE/FALSE m_pl_o1_div_+o 'C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv'
    ## 681704 Video 1/0/T/F/TRUE/FALSE m_pl_o1_div_+o 'C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv'
    ## 681705 Video 1/0/T/F/TRUE/FALSE m_pl_o1_div_+o 'C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv'
    ## ...... ..... .................. .............. .............................................................................................................................
    ## See problems(...) for more details.

    # Making a subset from samples only containing social engagement data and relevant columns
    social <- samples %>% subset(Task == "SocialEngagement") %>% select(c("ParticipantID","SaccadeNo","Trial","Fix_Duration","PupilSize","Sac_Amplitude","Task","FixationNo","Sac_Duration","Time","Fix_MeanPupilSize","Fix_MeanX","Fix_MeanY"))

    # Importing information from another csv file
    soc_info <- read_csv("C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/logfiles_SocialEngagement/logfiles_SocialEngagement.csv")

    ## Parsed with column specification:
    ## cols(
    ##   Trial = col_double(),
    ##   ParticipantID = col_character(),
    ##   Video = col_character()
    ## )

    # Merging the two dataframes by Trial and ParticipantID
    social <- merge(social,soc_info, by = c("ParticipantID","Trial"))

    # Making a new colums with direction and ostensive
    social$direction[grepl("dir", social$Video)] = "direct"
    social$direction[grepl("div", social$Video)] = "averted"

    social$osten[grepl("+o", social$Video)] = "ostensive"
    social$osten[grepl("-o", social$Video)] = "non-ostensive"

    # Remove original video column
    social$Video <- NULL

### Plot pupil size distribution

    # Distribution of pupil size density
    pupil <- social[!is.na(social$PupilSize),] %>% # remember to remove NAs 
      group_by(ParticipantID, Trial) %>% 
      summarize(PupilSize = mean(PupilSize), 
                Task = Task[1],
                osten = osten[1],
                direction = direction[1])

    # plot density of pupil sizes
    ggplot(pupil, aes(PupilSize, color = osten)) + geom_density() + facet_wrap(.~ParticipantID)

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ggplot(pupil, aes(PupilSize, color = direction)) + geom_density() + facet_wrap(.~osten)

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    ggplot(pupil, aes(PupilSize, color = ParticipantID)) + geom_density() + facet_wrap(.~osten)

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-2-3.png)

    #Plot
    p_direct <- ggplot(social, aes(Time, PupilSize, color = ParticipantID)) +
      geom_smooth() + facet_wrap(.~direction, scales = "free_x") + ggtitle('Direction')
    p_ostensive <- ggplot(social, aes(Time, PupilSize, color = ParticipantID)) +
      geom_smooth() + facet_wrap(.~osten, scales = "free_x") + ggtitle('Ostensiveness')

    p_direct / p_ostensive

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 769 rows containing non-finite values (stat_smooth).

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 769 rows containing non-finite values (stat_smooth).

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-2-4.png)

    # Overview of data in table
    social_table <- social[!is.na(social$PupilSize),] 

    social_table %>% 
      summarize(
      MeanPupil = mean(PupilSize),
      MaxPupil = max(PupilSize),
      MinPupil = min(PupilSize),
      StandardD = sd(PupilSize),
      Participants = length(unique(ParticipantID))
      
    )

    ##   MeanPupil MaxPupil MinPupil StandardD Participants
    ## 1  6523.734     8488     1815  835.8168            6

### Scanpath

    # Scanpath for one participant
    F3 = subset(social, ParticipantID ==    "F3")


    ## Let's make a summary dataset
    fix <- F3[!is.na(F3$FixationNo),] %>%
      group_by(FixationNo) %>%
      summarize(MeanX = Fix_MeanX[1], 
                MeanY = Fix_MeanY[1], 
                Duration = Fix_Duration[1],
                osten = osten[1],
                direction = direction[1]) %>% 
      filter(Duration>=300)

    # Run ggplot
    ggplot(fix, aes(MeanX, MeanY, color = fix$FixationNo)) + 
      geom_path(color = "black") +
      geom_point(size = fix$Duration*.01, alpha = .5) +
      ggrepel::geom_text_repel(aes(label = fix$Duration), size = 3, color = "black") +
      xlim(0,1680) + ylim(0,1050)

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### Models

    # Remove saccades and use remaining data (fixation data)
    social_model <- social[!is.na(social$FixationNo),] 

    # Average pupilsize on each fixation in order to downsample
    social_model <- social_model %>% group_by(FixationNo,ParticipantID,Trial) %>% 
      summarize(
        pupil_mean = mean(PupilSize),
        osten = osten[1],
        direction = direction[1]
      )


    # Gaussian model
    model_gau <- lme4::glmer(pupil_mean ~ 1 + direction:osten + (1 + direction:osten|ParticipantID),
          data = social_model,
          family = gaussian(link = identity))

    ## Warning in lme4::glmer(pupil_mean ~ 1 + direction:osten + (1 +
    ## direction:osten | : calling glmer() with family=gaussian (identity link) as
    ## a shortcut to lmer() is deprecated; please call lmer() directly

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

    ## boundary (singular) fit: see ?isSingular

    summary(model_gau)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## pupil_mean ~ 1 + direction:osten + (1 + direction:osten | ParticipantID)
    ##    Data: social_model
    ## 
    ## REML criterion at convergence: 7972.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1670 -0.5511  0.1467  0.6531  2.2694 
    ## 
    ## Random effects:
    ##  Groups        Name                                Variance Std.Dev. Corr
    ##  ParticipantID (Intercept)                         501218   708.0        
    ##                directionaverted:ostennon-ostensive  66770   258.4    0.12
    ##                directiondirect:ostennon-ostensive  109757   331.3    0.36
    ##                directionaverted:ostenostensive      72356   269.0    0.18
    ##                directiondirect:ostenostensive       84210   290.2    0.37
    ##  Residual                                          109681   331.2        
    ##                
    ##                
    ##                
    ##  0.51          
    ##  0.25 0.80     
    ##  0.36 0.90 0.97
    ##                
    ## Number of obs: 551, groups:  ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error t value
    ## (Intercept)                          6566.87     351.63  18.675
    ## directionaverted:ostennon-ostensive  -203.32     133.54  -1.523
    ## directiondirect:ostennon-ostensive     47.04      72.25   0.651
    ## directionaverted:ostenostensive      -157.60      51.30  -3.072
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnv:- drctnd:-
    ## drctnvrtd:- -0.415                  
    ## drctndrct:-  0.040  0.347           
    ## drctnvrtd:s -0.502  0.096   -0.063  
    ## fit warnings:
    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # Lognormal model with interaction effect
    model_log <- lme4::glmer(pupil_mean ~ 1 + direction*osten + (1 + direction*osten|ParticipantID),
          data = social_model,
          family = gaussian(link = log))

    ## boundary (singular) fit: see ?isSingular

    summary(model_log)

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: pupil_mean ~ 1 + direction * osten + (1 + direction * osten |  
    ##     ParticipantID)
    ##    Data: social_model
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8266.8   8331.5  -4118.4   8236.8      536 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2513 -0.5611  0.1497  0.6562  2.3265 
    ## 
    ## Random effects:
    ##  Groups        Name                           Variance Std.Dev. Corr 
    ##  ParticipantID (Intercept)                      7575.8  87.04        
    ##                directiondirect                   413.8  20.34    0.23
    ##                ostenostensive                    302.7  17.40    0.15
    ##                directiondirect:ostenostensive    377.3  19.42   -0.02
    ##  Residual                                     105905.9 325.43        
    ##             
    ##             
    ##             
    ##   0.83      
    ##  -0.97 -0.86
    ##             
    ## Number of obs: 551, groups:  ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)                     8.63366    0.07100 121.602   <2e-16 ***
    ## directiondirect                 0.04905    0.05577   0.879    0.379    
    ## ostenostensive                  0.01649    0.04316   0.382    0.702    
    ## directiondirect:ostenostensive -0.03223    0.05940  -0.543    0.587    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnd ostnst
    ## directndrct -0.717              
    ## ostenostnsv -0.900  0.894       
    ## drctndrct:s  0.783 -0.978 -0.930
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # Lognormal model without interaction effect
    model_log2 <- lme4::glmer(pupil_mean ~ 1 + direction+osten + (1 + direction+osten|ParticipantID),
          data = social_model,
          family = gaussian(link = log))

    ## boundary (singular) fit: see ?isSingular

    summary(model_log2)

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: pupil_mean ~ 1 + direction + osten + (1 + direction + osten |  
    ##     ParticipantID)
    ##    Data: social_model
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8211.9   8255.0  -4096.0   8191.9      541 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2870 -0.6416  0.1294  0.6490  2.8103 
    ## 
    ## Random effects:
    ##  Groups        Name            Variance  Std.Dev. Corr       
    ##  ParticipantID (Intercept)       1517.69  38.958             
    ##                directiondirect     11.05   3.325   1.00      
    ##                ostenostensive      85.67   9.256  -0.16 -0.16
    ##  Residual                      116320.47 341.058             
    ## Number of obs: 551, groups:  ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)      8.7556570  0.0467046 187.469  < 2e-16 ***
    ## directiondirect  0.0300252  0.0060581   4.956 7.19e-07 ***
    ## ostenostensive  -0.0004398  0.0110803  -0.040    0.968    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnd
    ## directndrct  0.615       
    ## ostenostnsv -0.156 -0.093
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # exp(intercept + slope) - exp(intercept) --> Get the beta on a normal scale
    exp(0.0300252 + 8.7556570) - exp(8.7556570)

    ## [1] 193.4442

    #to how much variance it explains we need R^2
    MuMIn::r.squaredGLMM(model_log2)

    ## Registered S3 method overwritten by 'MuMIn':
    ##   method         from
    ##   predict.merMod lme4

    ## Warning: 'r.squaredGLMM' now calculates a revised statistic. See the help
    ## page.

    ##               R2m        R2c
    ## [1,] 1.913216e-09 0.01392186

### Check residuals of log and gau models

    # HOW do they predict
    social_model$predict_gau <- predict(model_gau)
    social_model$predict_log <- predict(model_log)

    plot(density(social_model$predict_gau))

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    plot(density(social_model$predict_log))

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-5-2.png)

    # Run Dharma plots on the residuals 
    dGaus <- DHARMa::simulateResiduals(model_gau)
    dLog <- DHARMa::simulateResiduals(model_log)
    plot(dGaus)

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-5-3.png)

    plot(dLog)

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-5-4.png)

### Plot results - spaghetti

    # run plot
    spaghetti_plot <- social_model %>% ggplot(aes(osten, pupil_mean, group = ParticipantID, color = ParticipantID,labs = T)) + facet_wrap(.~direction) + geom_smooth(method = "lm", se = TRUE, alpha = .15, aes(fill = ParticipantID))
    spaghetti_plot

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    # Plot with ggplot , , color = ParticipantID
    spaghetti_plot2 <- social_model %>% ggplot(aes(direction, pupil_mean, group = ParticipantID, color = ParticipantID, labs = T)) + geom_smooth(method = "lm") + ggtitle("Spaghetti plot - Direction")
    spaghetti_plot2

![](social_engagement_files/figure-markdown_strict/unnamed-chunk-6-2.png)
