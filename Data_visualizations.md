Set settigns
============

Import data
===========

    Samples <- read_csv("C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/Exported_EyeLink_data/Samples_merged.csv") %>% 
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

    # While importing data, the y-axis is being flipped so it fits when we try to visualise. + filtering of time, because longer times does not make sense.

Sanity checks
=============

### Check distribution of fixations

Let's start with density plots

    # before plotting, we must make a summary dataset
    Fix <- Samples[!is.na(Samples$FixationNo),] %>% # remember to remove NAs 
      group_by(ParticipantID, Trial) %>% 
      summarize(Fix_Number = max(FixationNo), 
                Fix_Duration = Fix_Duration[1],
                Task = Task[1])

    # plot density of fixation number
    ggplot(Fix, aes(Fix_Number, color = ParticipantID)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20fixations-1.png)

    # plot density of fixation duration
    ggplot(Fix, aes(Fix_Duration, color = ParticipantID)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20fixations-2.png)

We can also use histograms:

    # before doing this we must make a summary dataset
    Fix <- Samples[!is.na(Samples$FixationNo),] %>% # remember to remove NAs 
      group_by(ParticipantID, Trial, FixationNo) %>% 
      summarize(Fix_Duration = Fix_Duration[1],
                Task = Task[1], ParticipantGender = ParticipantGender[1]) %>% 
      group_by(ParticipantID, Trial) %>% 
      summarize(Fix_Number = max(FixationNo),
                Fix_Duration = mean(Fix_Duration), Task = Task[1],ParticipantGender = ParticipantGender[1] )

    # plot density of fixation number
    ggplot(Fix, aes(Fix_Number, fill = ParticipantGender)) + geom_histogram() + facet_wrap(.~Task)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20fixations%20histograms-1.png)

    # plot density of fixation duration
    ggplot(Fix, aes(Fix_Duration, fill = ParticipantGender)) + geom_histogram() + facet_wrap(.~Task)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20fixations%20histograms-2.png)

### Check distribution of saccades

-   notice anything interesting about the number of saccades?

<!-- -->

    ## Check distribution of saccades

    # before doing this we must make a summary dataset
    Sac <- Samples[!is.na(Samples$SaccadeNo),] %>% # remember to remove NAs 
      # We group for ID, trial and saccade number so the following is with respect to each saccade (the duration and the amplitude is the same value in all columns)
      group_by(ParticipantID, Trial, SaccadeNo) %>% 
      summarize(Sac_Duration = Sac_Duration[1],
                Sac_Amplitude = Sac_Amplitude[1],
                Task = Task[1], ParticipantGender = ParticipantGender[1]) %>% 
      # In this summary, we take the values we 'picked' before and take the mean and max of them
      group_by(ParticipantID, Trial) %>% 
      summarize(Sac_Number = max(SaccadeNo),
                Sac_Amplitude = mean(Sac_Amplitude),
                Sac_Duration = mean(Sac_Duration), 
                Task = Task[1],ParticipantGender = ParticipantGender[1] )

    # plot density of saccade number
    ggplot(Sac, aes(Sac_Number, color = ParticipantID)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20saccades-1.png)

    # plot density of saccade duration
    ggplot(Sac, aes(Sac_Duration, color = ParticipantID)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20saccades-2.png)

    # plot density of saccade amplitude
    ggplot(Sac, aes(Sac_Amplitude, color = ParticipantID)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20saccades-3.png)

    # plot density of saccade number by gender
    ggplot(Sac, aes(Sac_Number, color = ParticipantGender)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/sanity%20checks%20saccades-4.png)

### Remove all the data points that fall outside of the screen coordinates (1680, 1050)

    # before... Check with plots
    plot(density(Samples$GazeX, na.rm = TRUE))

![](Data_visualizations_files/figure-markdown_strict/remove%20artefacts-1.png)

    plot(density(Samples$GazeY, na.rm = TRUE))

![](Data_visualizations_files/figure-markdown_strict/remove%20artefacts-2.png)

    # Remove data points falling outside the screen
    Samples <- Samples %>% filter(GazeX >= 0 & GazeX <= 1680 & GazeY >= 0 & GazeY <= 1050)

    # ...and after - check
    plot(density(Samples$GazeX, na.rm = TRUE))

![](Data_visualizations_files/figure-markdown_strict/remove%20artefacts-3.png)

    plot(density(Samples$GazeY, na.rm = TRUE))

![](Data_visualizations_files/figure-markdown_strict/remove%20artefacts-4.png)

### Check distribution of pupil sizes

    # before doing this we must make a summary dataset
    Pup <- Samples[!is.na(Samples$PupilSize),] %>% # remember to remove NAs 
      group_by(ParticipantID, Trial) %>% 
      summarize(PupilSize = mean(PupilSize), Task = Task[1])

    # plot density of pupil sizes
    ggplot(Pup, aes(PupilSize, color = ParticipantID)) + geom_density() + facet_wrap(.~Task)

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    # This not a plot of how the pupil size change within each trial but across trials for each participant

Visualizations
--------------

### First scanpath Scanpath

Example of scanpath for one participant on one picture.

    ## Here I am making the scanpath for one participant in one trial
    x = subset(Samples, ParticipantID ==    'F7_2' & Trial == 10)

    ## Let's make a summary dataset
    Fix <- x[!is.na(x$FixationNo),] %>% # remove lines with NA's
      group_by(FixationNo) %>% # since I only have one participant and one trial
      summarize(MeanX = Fix_MeanX[1], MeanY = Fix_MeanY[1], Duration = Fix_Duration[1]) %>% 
      filter(Duration>=300) # only keep fixations > 300 ms - fixations under 300mm does not say anyhting cognitively

    img <- jpeg::readJPEG('C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/4-1-portfolio/stimuli_Foraging/space_capsules.jpg')  
    img <- grid::rasterGrob(img, width=unit(1, "npc"), height = unit(1,"npc"),
                            interpolate = FALSE) # something with the size

    ggplot(Fix, aes(MeanX, MeanY, color = Fix$FixationNo)) + 
      annotation_custom(img, xmin = 0, xmax = 1680, ymin = 0, ymax = 1050) +
      geom_path(color = "black") +
      geom_point(size = Fix$Duration*.02, alpha = .8) +
      ggrepel::geom_text_repel(aes(label = Fix$Duration), size = 3, color = "white") +
      xlim(0,1680) + ylim(0,1050)

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-2-1.png)

### Make scan path contrast image between 2 participants of 2 diffeent conditions.

    # Get overview to select relevant participant and pictures to make scan ploth for assignment
    view <- Samples %>% group_by(Stimulus, ParticipantID) %>% 
      summarise(type = ForagingType[1])
    head(view)

    ## # A tibble: 6 x 3
    ## # Groups:   Stimulus [1]
    ##   Stimulus  ParticipantID type  
    ##   <chr>     <chr>         <chr> 
    ## 1 birds.jpg F6_1          Search
    ## 2 birds.jpg F7_2          Count 
    ## 3 birds.jpg F8_1          Search
    ## 4 birds.jpg F9_2          Count 
    ## 5 birds.jpg M2_1          Search
    ## 6 birds.jpg M3_2          Count

    # As factor 
    Samples$FixationNo <- as.factor(Samples$FixationNo)

    ## subset with 2 partipants looking at birds picture , count and search
    Q = subset(Samples, ParticipantID == 'M2_1' & Stimulus == 'birds.jpg') # Search
    H = subset(Samples, ParticipantID == 'F9_2' & Stimulus == 'birds.jpg') # count          
               
    # Q
    Fix_Q <- Q[!is.na(Q$FixationNo),] %>% # remove lines with NA's
      group_by(FixationNo) %>% # since I only have one participant and one trial
      summarize(MeanX = Fix_MeanX[1], MeanY = Fix_MeanY[1], Duration = Fix_Duration[1]) %>% 
      filter(Duration>=300) # only keep fixations > 300 ms - fixations under 300mm does not say anyhting cognitively

    img <- jpeg::readJPEG('C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/4-1-portfolio/stimuli_Foraging/birds.jpg')  
    img <- grid::rasterGrob(img, width=unit(1, "npc"), height = unit(1,"npc"),
                            interpolate = FALSE) # something with the size

    search <- ggplot(Fix_Q, aes(MeanX, MeanY, color = Fix_Q$FixationNo)) + 
      annotation_custom(img, xmin = 0, xmax = 1680, ymin = 0, ymax = 1050) +
      geom_path(color = "black") +
      geom_point(size = Fix_Q$Duration*.02, alpha = .8) +
      ggrepel::geom_text_repel(aes(label = Fix_Q$Duration), size = 3, color = "white") +
      xlim(0,1680) + ylim(0,1050) + ggtitle("Search")



    # H
    Fix_H <- H[!is.na(H$FixationNo),] %>% # remove lines with NA's
      group_by(FixationNo) %>% # since I only have one participant and one trial
      summarize(MeanX = Fix_MeanX[1], MeanY = Fix_MeanY[1], Duration = Fix_Duration[1]) %>% 
      filter(Duration>=300) # only keep fixations > 300 ms - fixations under 300mm does not say anyhting cognitively

    img <- jpeg::readJPEG('C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/4-1-portfolio/stimuli_Foraging/birds.jpg')  
    img <- grid::rasterGrob(img, width=unit(1, "npc"), height = unit(1,"npc"),
                            interpolate = FALSE) # something with the size

    count <- ggplot(Fix_H, aes(MeanX, MeanY, color = Fix_H$FixationNo)) + 
      annotation_custom(img, xmin = 0, xmax = 1680, ymin = 0, ymax = 1050) +
      geom_path(color = "black") +
      geom_point(size = Fix_H$Duration*.02, alpha = .8) +
      ggrepel::geom_text_repel(aes(label = Fix_H$Duration), size = 3, color = "white") +
      xlim(0,1680) + ylim(0,1050) + ggtitle("Count")

    # Plot the 2 plots next to eachother with patchwork
    search + count

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### All scanpaths with loop

Using a for-loop, make a scanpath for each participant in the Foraging
experiment. Use facets to plot the 10 trials separately for each
participant. Use these plots as diagnostic tools in order to answer the
following questions:

1.  Do the data look reasonable and of good quality? Do we have any
    issues?
2.  Can we differentiate between the two conditions (Count and Search)
    only by looking at the scanpaths?
3.  Can we spot the trials in which the participants found the star?

<!-- -->

    # Making a subset only with foraging
    foraging <- Samples[Samples$Task == 'Foraging',]

    participants <- as.character(levels(as.factor(foraging$ParticipantID)))
    for(i in 1:length(participants)){
      for(t in levels(as.factor(foraging$Trial)))
        
        # making a subset that changes for each participant and trial
        x = subset(foraging, ParticipantID ==   participants[i], Trial = t)
        
        # with the subset x, stimulus column is changed to factor. change into levels
        for(p in levels(as.factor(x$Stimulus))){
          Fix <- x[!is.na(x$FixationNo) & x$Stimulus == p,] %>% 
          # makes a subset from x but only from one participant, trial and picture
            
          group_by(FixationNo) %>% # since I only have one participant and one trial
          summarize(MeanX = Fix_MeanX[1], MeanY = Fix_MeanY[1], Duration = Fix_Duration[1],
                    Stimulus = Stimulus[1], Trial = Trial[1])%>% 
          filter(Duration>=300) # only keep fixations > 300 ms
          
          # specify where to get the picture - paste() is constructing the filepath by adding stimulus name
          picpath <- paste('C:/Users/askes/OneDrive/Skrivebord/Cognitive Science/4. semester - big files/day 5/4-1-portfolio/stimuli_Foraging/',p, sep = "" )
          
          
          img <- jpeg::readJPEG(picpath)  
          img <- grid::rasterGrob(img, width=unit(1, "npc"), height = unit(1,"npc"),
                            interpolate = FALSE) # make it match with screen size
          
          # Making plot name
          plotname <- paste("Participant: ", participants[i], " Trial: ", t, " Picture: ", p, sep = "")
          
          # Making plot
          great_plot <- ggplot(Fix, aes(MeanX, MeanY, color = Fix$FixationNo)) + 
          annotation_custom(img, xmin = 0, xmax = 1680, ymin = 0, ymax = 1050) +
          geom_path(color = "black") +
          geom_point(size = Fix$Duration*.02, alpha = .8) +
          ggrepel::geom_text_repel(aes(label = Fix$Duration), size = 3, color = "white") +
          xlim(0,1680) + ylim(0,1050) + ggtitle(plotname)
          
          
          print(great_plot)
        }
        ## could use patchwork function to make the plots look nicer
            #name <- paste("ID", i, sep = "")
    }

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-1.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-2.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-3.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-4.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-5.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-6.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-7.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-8.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-9.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-10.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-11.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-12.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-13.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-14.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-15.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-16.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-17.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-18.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-19.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-20.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-21.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-22.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-23.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-24.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-25.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-26.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-27.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-28.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-29.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-30.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-31.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-32.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-33.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-34.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-35.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-36.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-37.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-38.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-39.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-40.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-41.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-42.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-43.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-44.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-45.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-46.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-47.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-48.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-49.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-50.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-51.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-52.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-53.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-54.png)![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-4-55.png)

Running models
==============

The models (lognormal and gaussian)

    # 1 row per saccade
    # data has to contain amplitude, forreign type, iD and stimulus 

    # Subset data so we only have one row per saccade and only foraging task
    foraging <- subset(Samples, Task == 'Foraging')

    Sac <- foraging[!is.na(foraging$SaccadeNo),] %>% # remember to remove NAs 
      group_by(ParticipantID, Trial, SaccadeNo) %>% # stimulus, forarging type? where to make summaries...
      
      summarize(Sac_Duration = Sac_Duration[1], 
                Sac_Amplitude = Sac_Amplitude[1], # by change, some can contain NA - use min() na.rm = T
                ParticipantGender = ParticipantGender[1], 
                Stimulus = Stimulus[1], 
                ForagingType = ForagingType[1])

    head(Sac)

    ## # A tibble: 6 x 8
    ## # Groups:   ParticipantID, Trial [1]
    ##   ParticipantID Trial SaccadeNo Sac_Duration Sac_Amplitude ParticipantGend~
    ##   <chr>         <dbl>     <dbl>        <dbl>         <dbl> <chr>           
    ## 1 F6_1              1         1           54          5.54 Female          
    ## 2 F6_1              1         2           46          4.02 Female          
    ## 3 F6_1              1         3           52         10.6  Female          
    ## 4 F6_1              1         4           28          4.29 Female          
    ## 5 F6_1              1         5           10          0.45 Female          
    ## 6 F6_1              1         6           92         21.4  Female          
    ## # ... with 2 more variables: Stimulus <chr>, ForagingType <chr>

    # Remove NA's from Sac_amplitude
    Sac <- Sac[!is.na(Sac$Sac_Amplitude),] 

    # Plot the subset 
    ggplot(Sac, aes(Sac_Amplitude, color = ForagingType)) + geom_density()

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # Gaussian distributions
    model_gau <- lme4::glmer(Sac_Amplitude ~ 1 + ForagingType + (1+ForagingType|ParticipantID) + (1+ForagingType|Stimulus),
          data = Sac,
          family = gaussian(link = identity))


    # Lognormal 
    model_log <- lme4::glmer(Sac_Amplitude ~ 1 + ForagingType + (1+ForagingType|ParticipantID) + (1+ForagingType|Stimulus),
          data = Sac,
          family = gaussian(link = log))

    ## boundary (singular) fit: see ?isSingular

    summary(model_log)

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: 
    ## Sac_Amplitude ~ 1 + ForagingType + (1 + ForagingType | ParticipantID) +  
    ##     (1 + ForagingType | Stimulus)
    ##    Data: Sac
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    20430    20486   -10206    20412     3752 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5868 -0.6025 -0.3036  0.3152  6.2017 
    ## 
    ## Random effects:
    ##  Groups        Name               Variance Std.Dev. Corr
    ##  Stimulus      (Intercept)         0.0000  0.0000       
    ##                ForagingTypeSearch  0.3845  0.6201    NaN
    ##  ParticipantID (Intercept)         0.0000  0.0000       
    ##                ForagingTypeSearch  0.1257  0.3545    NaN
    ##  Residual                         13.0548  3.6131       
    ## Number of obs: 3761, groups:  Stimulus, 10; ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)         0.92695    0.03554  26.085  < 2e-16 ***
    ## ForagingTypeSearch  0.53735    0.07907   6.796 1.08e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## FrgngTypSrc -0.449
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # when we get the estimates from the models, in order to calculate the estimates on a normal scale (right now it is log), we need to invert it with exp(). But because our slope is the difference between the conditions. we need to do : exp(intercept + slope) - exp(intercept) . we have to incorporate the intercept because the log scale is totally different so the difference will vary depending on where on the scale u are...
    exp(0.92669 + 0.53761) - exp(0.92669)

    ## [1] 1.798381

    # 1.798 amplitude degree

Plot results

    # Plot with ggplot
    spaghetti_plot <- Sac %>% ggplot(aes(ForagingType, Sac_Amplitude, group = ParticipantID, color = ParticipantID,labs = T)) + geom_smooth(method = "lm") + ggtitle("Spaghetti plot")
    spaghetti_plot

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Variance with R^2

    #to how much variance it explains we need R^2
    MuMIn::r.squaredGLMM(model_log)

    ##              R2m        R2c
    ## [1,] 0.005277972 0.02693766

    # Generating new data sets 
    # we dont want error to change no matter what we predict 
     
    # Predictions of the 2 models
    pred_log <- predict(model_log)
    pred_gau <- predict(model_gau)

    Sac$predict_log <- pred_log
    Sac$predict_gau <- pred_gau

    # Plot of predictions 
    plot(density(pred_log))

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    plot(density(pred_gau))

![](Data_visualizations_files/figure-markdown_strict/unnamed-chunk-7-2.png)
