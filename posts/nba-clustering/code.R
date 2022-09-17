
library(tidyverse)
library(tidymodels)
library(tidytext)
library(Rtsne)
library(embed)




nba <- read_csv("/Users/karatatiwantsinghsidhu/Documents/Code/karat_codes/posts/nba-clustering/data/nbaplayersdraft.csv")

nba |> view()

colnames(nba)



nba <- nba |> select(-c(id, college))

nba <- nba |> na.omit()


set.seed(123)


pca_rec <- recipe(~., data = nba) |> # what data to use
    update_role(player, team, new_role = "id") |>
    step_normalize(all_predictors()) |> # normalize all other columns
    step_pca(all_predictors()) # pca for all other columns


pca_prep <- prep(pca_rec)

pca_prep



tidied_pca <- tidy(pca_prep, 2)

tidied_pca

tidied_pca |>
    filter(
        component == "PC1" |
            component == "PC2" |
            component == "PC3" |
            component == "PC4"
    ) |>
    mutate(component = fct_inorder(component)) |>
    ggplot(aes(value, terms, fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, nrow = 1) +
    hrbrthemes::theme_ipsum() +
    labs(y = NULL)




tidied_pca |>
    filter(component %in% paste0("PC", 1:2)) |>
    group_by(component) |>
    top_n(8, abs(value)) |>
    ungroup() |>
    mutate(terms = reorder_within(terms, abs(value), component)) |>
    ggplot(aes(abs(value), terms, fill = value > 0)) +
    geom_col() +
    facet_wrap(~component, scales = "free_y") +
    scale_y_reordered() +
    labs(
        x = "Absolute value of contribution",
        y = NULL, fill = "Positive?"
    ) +
    hrbrthemes::theme_ipsum()




juice(pca_prep) |>
    ggplot(aes(PC1, PC2, label = player)) +
    geom_point(aes(color = team), alpha = 0.7, size = 2) +
    ggrepel::geom_text_repel(max.overlaps = 20) +
    labs(color = NULL) +
    hrbrthemes::theme_ipsum()



umap_rec <- recipe(~., data = nba) |>
    update_role(player, team, new_role = "id") |>
    step_normalize(all_predictors()) |>
    step_umap(all_predictors())




umap_prep <- prep(umap_rec)

umap_prep


juice(umap_prep) |>
    ggplot(aes(UMAP1, UMAP2, label = player)) +
    geom_point(aes(color = team), alpha = 0.7, size = 2) +
    ggrepel::geom_text_repel(max.overlaps = 20) +
    labs(color = NULL) +
    hrbrthemes::theme_ipsum()


nba_tsne <- nba |> select(-c(team, player))



tsne <- Rtsne(nba_tsne,
    perplexity = 30,
    eta = 100,
    max_iter = 2000
)


Y <- as.data.frame(tsne$Y)

teams <- nba$team
players <- nba$player




ggplot(Y, aes(x = V1, y = V2, label = players)) +
    geom_point(aes(color = teams)) +
    labs(x = "tsne-1", y = "tsne-2", color = "team") +
    ggrepel::geom_text_repel(max.overlaps = 50) +
    hrbrthemes::theme_ipsum()
