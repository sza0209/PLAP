### Question 1

1.  A YAML header is the section at the very top of an R Markdown
    document, enclosed by —. It contains metadata and configuration
    settings for the R markdown document, such as the title, author,
    date, and output format (e.g., HTML, PDF, or Word). It also controls
    how the document is rendered when you knit it.

2.  Literate programming is a style of writing code where you combine
    explanatory text and code in the same document. Instead of just
    showing code, you explain what the code is doing and why.

### Question 2

**a. Clickable link to manuscript where data is published**

- Noel, Z.A., Roze, L.V., Breunig, M., Trail, F. 2022. Endophytic fungi
  as promising biocontrol agent to protect wheat from *Fusarium
  graminearum* head blight. Plant
  Disease.<https://doi.org/10.1094/PDIS-06-21-1253-RE>

**b. Read the data using a relative file path**

``` r
mycotoxin <- read.csv("MycotoxinData.csv", na.strings = "na")
```

**c. Last question of coding challenge 3**

Before working with the data, we load the packages needed for plotting
and figure arrangement.

``` r
library(ggplot2)
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 4.5.2

Define a colorblind-friendly palette

``` r
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
```

Creating a boxplot of DON by Treatment

``` r
plotA = ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(position = position_dodge()) +
  geom_point(aes(fill = Cultivar), shape = 21, position = position_jitterdodge(dodge.width = 0.9), alpha = 0.6, color = "black") +     # Jittering points, changing transparency of points to 0.6
  scale_fill_manual(values = c(cbbPalette[[3]], cbbPalette[[4]])) +
  facet_wrap(~Cultivar) +    # faceting cultivar
  xlab("") +
  ylab("DON (ppm)") +    # labeling y-axis
  theme_classic()       # adding theme classic
plotA
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Changing the factor order level so that the treatment “NTC” is first,
followed by “Fg”, “Fg + 37”, “Fg + 40”, and “Fg + 70.

``` r
mycotoxin$Treatment <- factor(mycotoxin$Treatment,
                              levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))
```

creating a boxplot of X15ADON by Treatment

``` r
plotB = ggplot(mycotoxin, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(position = position_dodge()) +
  geom_point(aes(fill = Cultivar), shape = 21, position = position_jitterdodge(dodge.width = 0.9), alpha = 0.6, color = "black") +     # Jittering points, changing transparency of points to 0.6
  scale_fill_manual(values = c(cbbPalette[[3]], cbbPalette[[4]]), labels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70")) +
  facet_wrap(~Cultivar) +    # faceting cultivar
  xlab("") +
  ylab("15ADON") +    # labeling y-axis
  theme_classic()
plotB
```

    ## Warning: Removed 10 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Creating a boxplot of MassperSeed_mg by Treatment

``` r
plotC = ggplot(mycotoxin, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(position = position_dodge()) +
  geom_point(aes(fill = Cultivar), shape = 21, position = position_jitterdodge(dodge.width = 0.9), alpha = 0.6, color = "black") +     # Jittering points, changing transparency of points to 0.6
  scale_fill_manual(values = c(cbbPalette[[3]], cbbPalette[[4]]), labels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70")) +
  facet_wrap(~Cultivar) +    # faceting cultivar
  xlab("") +
  ylab("Seed Mass (mg)") +    # labeling y-axis
  theme_classic()
plotC
```

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Using geom_pwc() to add t.test pairwise comparisons to the three plots
made above. Saving each plot as a new R object, and combine them with
*ggarange* function

``` r
plotD <- plotA +
  geom_pwc(aes(group = Treatment), method = "t_test", label = "{p.adj.format}{p.adj.signif}")

plotE <- plotB +
  geom_pwc(aes(group = Treatment), method = "t_test", label = "{p.adj.format}{p.adj.signif}")

plotF <- plotC +
  geom_pwc(aes(group = Treatment), method = "t_test", label = "{p.adj.format}{p.adj.signif}")

figure <- ggarrange(
  plotD,  # First plot (pwc_DON by Treatment)
  plotE,  # Second plot (pwc_X15ADON by Treatment)
  plotF,  # Third plot (pwc_MassperSeed_mg by Treatment)
  labels = c("A", "B", "C"),  # label the plots (A, B, C)
  nrow = 1,  # Arrange the plots in 3 rows
  ncol = 3,  # Arrange the plots in 1 column
  common.legend = TRUE  # Including a legend in the combined figure
)
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_pwc()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_pwc()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 10 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 10 rows containing non-finite outside the scale range
    ## (`stat_pwc()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_pwc()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
figure
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
