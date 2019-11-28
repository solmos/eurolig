
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eurolig

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The eurolig package provides a set of tools to obtain and analyze
basketball generated data from the Euroleague.

## Overview

  - API wrapper around the official website of the
    [Euroleague](https://www.euroleague.net/).

  - Functions to obtain stats, play-by-play data and shot location data
    in a tidy format within R.

  - Tools to analyze and visualize the data.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("solmos/eurolig")
```

## Example

Let’s say we want to analyze play-by-play data from the Championship
game of the 2017-2018 season between Real Madrid and Fenerbache Dogus
Istanbul. We can obtain the data by entering the game code and the
season to the function `extractPbp()`. The game code for the game can be
found in the game’s
[URL](https://www.euroleague.net/main/results/showgame?gamecode=260&seasoncode=E2017).

``` r
library(eurolig)
pbp <- extractPbp(game_code = 260, season = 2017)
pbp
#> # A tibble: 583 x 29
#>    season game_code play_number team_code player_name play_type
#>     <int>     <dbl>       <int> <chr>     <chr>       <chr>    
#>  1   2017       260           2 <NA>      <NA>        BP       
#>  2   2017       260           3 MAD       AYON, GUST… TPOFF    
#>  3   2017       260           4 ULK       GUDURIC, M… TPOFF    
#>  4   2017       260           5 ULK       DUVERIOGLU… 2FGM     
#>  5   2017       260           6 ULK       GUDURIC, M… AST      
#>  6   2017       260           7 ULK       VESELY, JAN CPF      
#>  7   2017       260           8 MAD       REYES, FEL… RPF      
#>  8   2017       260           9 MAD       AYON, GUST… 2FGA     
#>  9   2017       260          10 MAD       AYON, GUST… ORB      
#> 10   2017       260          11 ULK       GUDURIC, M… CPF      
#> # … with 573 more rows, and 23 more variables: time_remaining <chr>,
#> #   quarter <dbl>, points_home <dbl>, points_away <dbl>, play_info <chr>,
#> #   seconds <dbl>, home_team <chr>, away_team <chr>, home <lgl>,
#> #   team_name <chr>, last_ft <lgl>, and1 <lgl>, home_player1 <chr>,
#> #   home_player2 <chr>, home_player3 <chr>, home_player4 <chr>,
#> #   home_player5 <chr>, away_player1 <chr>, away_player2 <chr>,
#> #   away_player3 <chr>, away_player4 <chr>, away_player5 <chr>,
#> #   lineups <chr>
```

From these data we can extract information about the assists in that
game from, say, Real Madrid (MAD):

``` r
assists <- getAssists(pbp, team = "MAD")
assists
#> # A tibble: 16 x 13
#>    season game_code team_code passer shooter shot_type points
#>     <int>     <dbl> <chr>     <chr>  <chr>   <chr>      <dbl>
#>  1   2017       260 MAD       FERNA… LLULL,… 3FG            3
#>  2   2017       260 MAD       LLULL… RANDOL… 3FG            3
#>  3   2017       260 MAD       FERNA… TAYLOR… 2FG            2
#>  4   2017       260 MAD       LLULL… TAVARE… 2FG            2
#>  5   2017       260 MAD       DONCI… CARROL… 2FG            2
#>  6   2017       260 MAD       TAYLO… THOMPK… 2FG            2
#>  7   2017       260 MAD       DONCI… TAVARE… 2FG            2
#>  8   2017       260 MAD       DONCI… CARROL… 3FG            3
#>  9   2017       260 MAD       AYON,… CAUSEU… 3FG            3
#> 10   2017       260 MAD       CAUSE… REYES,… 2FG            2
#> 11   2017       260 MAD       DONCI… CAUSEU… 3FG            3
#> 12   2017       260 MAD       CAUSE… REYES,… 2FG            2
#> 13   2017       260 MAD       FERNA… DONCIC… 3FG            3
#> 14   2017       260 MAD       TAVAR… LLULL,… <NA>           0
#> 15   2017       260 MAD       TAVAR… CARROL… 3FG            3
#> 16   2017       260 MAD       THOMP… TAVARE… 2FG            2
#> # … with 6 more variables: time_remaining <chr>, quarter <dbl>,
#> #   seconds <int>, foul <lgl>, and1 <lgl>, ftm <dbl>
```

Shot location data can also be retrieved and visualized:

``` r
shots <- extractShots(260, 2017)
plotShotchart(shots)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->
