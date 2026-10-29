``` r
pkgs <- c("readr", "dplyr", "tidyr", "stringr", "ggplot2", "ggrepel", "knitr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(knitr)
```

## 1. Rank-to-score function

``` r
# 1. Create a function for the logit-to-range transformation
map_rank_to_logit_score <- function(ranks, N = 12) {
  # Convert rank to an adjusted fractional probability (Hazen correction)
  p <- (ranks - 0.5) / N

  # Invert so Rank 1 gets the highest probability
  p_inverted <- 1 - p

  # Apply logit transformation: ln(p / (1 - p))
  logits <- log(p_inverted / (1 - p_inverted))

  # Calculate absolute min/max limits for a perfect rank 1 to 12 set
  ideal_p <- (c(1, N) - 0.5) / N
  ideal_logits <- log((1 - ideal_p) / (1 - (1 - ideal_p)))
  logit_min <- min(ideal_logits)
  logit_max <- max(ideal_logits)

  # Scale linearly to the 70-100 range
  scores <- 70 + 30 * (logits - logit_min) / (logit_max - logit_min)
  return(scores)
}
```

## 2. Read data from GitHub

``` r
raw <- read_csv(params$data_url, show_col_types = FALSE)

# Shorten LimeSurvey headers: "rankContent[1]. Please order ..." -> "rankContent[1]"
names(raw) <- sub("\\.\\s.*$", "", names(raw))

# Keep only completed responses (incomplete ones have no ownGroup)
responses <- raw %>%
  filter(!is.na(ownGroup)) %>%
  mutate(ownGroup = str_trim(ownGroup))

cat("Completed responses:", nrow(responses), "\n")
```

    ## Completed responses: 55

## 3. Reshape and drop each rater’s own group

``` r
long <- responses %>%
  select(id, ownGroup,
         starts_with("rankContent["), starts_with("rankLikability[")) %>%
  pivot_longer(
    cols          = -c(id, ownGroup),
    names_to      = c("dimension", "rank_orig"),
    names_pattern = "rank(Content|Likability)\\[(\\d+)\\]",
    values_to     = "group"
  ) %>%
  mutate(rank_orig = as.integer(rank_orig),
         group     = str_trim(group)) %>%
  filter(!is.na(group))

N_GROUPS <- n_distinct(long$group)
cat("Groups ranked:", N_GROUPS, "\n")
```

    ## Groups ranked: 12

``` r
# How often did raters put their own group first? (just for information)
self_rank <- long %>%
  filter(group == ownGroup) %>%
  count(dimension, rank_orig, name = "raters") %>%
  filter(rank_orig == 1)
kable(self_rank, caption = "Raters who ranked their own group #1 (ignored in scoring)")
```

| dimension  | rank_orig | raters |
|:-----------|----------:|-------:|
| Content    |         1 |     11 |
| Likability |         1 |      9 |

Raters who ranked their own group #1 (ignored in scoring)

``` r
# Remove own group, then (optionally) re-rank the remaining groups 1..N-1
ratings <- long %>%
  filter(group != ownGroup) %>%
  group_by(id, dimension) %>%
  arrange(rank_orig, .by_group = TRUE) %>%
  mutate(
    rank = if (params$rerank_after_drop) row_number() else rank_orig,
    N    = if (params$rerank_after_drop) n()          else N_GROUPS,
    score = map_rank_to_logit_score(rank, N = first(N))
  ) %>%
  ungroup()

N_SCALE <- if (params$rerank_after_drop) N_GROUPS - 1 else N_GROUPS
```

## 4. Scores per group

Each rater’s rank is converted to a 70–100 score, then scores are
averaged across raters.

``` r
w_c <- params$w_content
w_l <- 1 - w_c

group_dim <- ratings %>%
  group_by(group, dimension) %>%
  summarise(raters    = n(),
            mean_rank = mean(rank),
            sd_score  = sd(score),
            score     = mean(score),
            .groups   = "drop")

group_scores <- group_dim %>%
  select(group, dimension, score) %>%
  pivot_wider(names_from = dimension, values_from = score) %>%
  left_join(group_dim %>% filter(dimension == "Content") %>%
              select(group, raters), by = "group") %>%
  mutate(Weighted = w_c * Content + w_l * Likability) %>%
  arrange(desc(Weighted)) %>%
  select(group, raters, Content, Likability, Weighted)

kable(group_scores, digits = 1,
      col.names = c("Group", "Raters", "Content", "Likability",
                    sprintf("Weighted (%d/%d)", round(100 * w_c), round(100 * w_l))))
```

| Group | Raters | Content | Likability | Weighted (70/30) |
|:------------------------------------|-----:|------:|--------:|-------------:|
| Goal framing theory | 51 | 88.5 | 94.5 | 90.3 |
| Norm Activation Model | 50 | 88.3 | 87.8 | 88.2 |
| ABC Model | 50 | 87.9 | 88.2 | 88.0 |
| Value-Belief-Norm (VBN) Theory | 49 | 88.5 | 85.5 | 87.6 |
| Comprehensive Action Determination Model (CADM) | 50 | 87.6 | 83.9 | 86.5 |
| Protection Motivation Theory (PMT) | 52 | 84.9 | 85.9 | 85.2 |
| Social practice theory | 52 | 83.0 | 85.2 | 83.6 |
| Mind-Behavior Gap Framework | 52 | 82.2 | 85.6 | 83.2 |
| Costly Signaling Theory (CST) | 47 | 83.3 | 82.9 | 83.2 |
| Theory of Planned Behavior (TPB) | 50 | 83.2 | 80.2 | 82.3 |
| Social Identity theory and Environmental Identity | 51 | 81.4 | 80.3 | 81.0 |
| Dual Process / Nudge Theory | 51 | 81.4 | 79.9 | 81.0 |

``` r
write_csv(group_scores %>% mutate(across(where(is.numeric), ~ round(.x, 2))),
          params$output_csv)
```

## 5. Ranks on the logit curve

The line shows the rank → score mapping. Grey dots are the scores
available for each rank; coloured points are each group’s average rank
and average score. Averages fall slightly off the curve because the mean
of the scores is not the score at the mean rank.

``` r
curve_df <- tibble(rank = seq(1, N_SCALE, length.out = 300)) %>%
  mutate(score = map_rank_to_logit_score(rank, N = N_SCALE))

step_df <- tibble(rank = 1:N_SCALE) %>%
  mutate(score = map_rank_to_logit_score(rank, N = N_SCALE))

ggplot() +
  geom_line(data = curve_df, aes(rank, score), linewidth = 1, colour = "grey40") +
  geom_point(data = step_df, aes(rank, score), colour = "grey60", size = 2) +
  geom_point(data = group_dim, aes(mean_rank, score, colour = group), size = 3.5) +
  geom_text_repel(data = group_dim, aes(mean_rank, score, label = group, colour = group),
                  size = 3, max.overlaps = Inf, show.legend = FALSE, seed = 1) +
  facet_wrap(~ dimension) +
  scale_x_continuous(breaks = 1:N_SCALE) +
  scale_y_continuous(limits = c(70, 100)) +
  labs(x = "Rank (1 = best)", y = "Score",
       title = "Peer ranks mapped to scores on the logit curve",
       subtitle = sprintf("Own group excluded; N = %d ranks per rater", N_SCALE)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
```

![](HW_ranking_scores_files/figure-markdown_github/curve-1.png)

## 6. Spread of individual scores

``` r
ggplot(ratings, aes(score, reorder(group, score), fill = dimension)) +
  geom_boxplot(alpha = 0.6, outlier.size = 1) +
  labs(x = "Score from individual raters", y = NULL, fill = NULL,
       title = "Distribution of scores per group") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
```

![](HW_ranking_scores_files/figure-markdown_github/spread-1.png)

## 7. Final scores

``` r
group_scores %>%
  pivot_longer(c(Content, Likability, Weighted), names_to = "measure", values_to = "score") %>%
  ggplot(aes(score, reorder(group, score * (measure == "Weighted")), colour = measure)) +
  geom_point(aes(size = measure == "Weighted")) +
  scale_size_manual(values = c(2.5, 4.5), guide = "none") +
  labs(x = "Score", y = NULL, colour = NULL,
       title = sprintf("Content, likability and weighted score (%d%% / %d%%)",
                       round(100 * w_c), round(100 * w_l))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
```

![](HW_ranking_scores_files/figure-markdown_github/final-1.png)
