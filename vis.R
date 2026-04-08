library(tidyverse)


#### Scores from CORnet-Z classifier. ####
scores <-
    read_csv("data/entropy_scores.csv") |>
    extract(
        file,
        c("uniqueID", NA, "blur"),
        "/(\\w+?)(_(\\d+))?.jpg",
        remove=F,
        convert=T
    ) |>
    mutate(
        blur = case_when(
            is.na(blur) ~ 0,
            .default=blur
        )
    )

scores

ggplot(scores) +
    geom_line(aes(x=blur, y=entropy, color = uniqueID)) +
    scale_color_viridis_d(option="rocket") +
    labs(
        title="Network Model Uncertainty on Blurred PCRM Stimuli",
        subtitle="CORnet-Z (512 dim. Output Layer)",
        x="Blur Level",
        y="Uncertainty (Entropy)",
        color="THINGS Image uniqueID"
    ) +
    theme_classic() +
    theme(legend.position="bottom")

ggsave("cornet-blur-uncertainty.png", width=6, height=4)

#### Scores from CLIP ViT-B/32 model. ####

read_csv("data/clip_scores_window.csv") |>
    mutate(i=row_number()) |>
    ggplot() +
    geom_bar(aes(x=i, y=prob, fill=i), stat='identity') +
    geom_text(
        aes(x=i, y=prob, label=label),
        nudge_x=.6, nudge_y=.005, hjust='left',
        color="grey30", size=2
    ) +
    xlim(0,50) +
    labs(
        title="Semantic Match with CLIP",
        subtitle="ViT-B/32 with 1854 THINGS Concepts as Output",
        x="50 Highest Probability Concepts (THINGS)",
        y="Probability (Softmax Output Layer)"
    ) +
    scale_fill_viridis_c(option="rocket", limits=c(-10,50), direction=-1) +
    theme_classic()

ggsave("clip-match-window.png", width=6, height=4)



read_csv("data/clip_scores.csv") |>
    extract(
        file,
        c("uniqueID", NA, "blur"),
        "/(\\w+?)(_(\\d+))?.jpg",
        remove=F,
        convert=T
    ) |>
    mutate(
        blur = case_when(
            is.na(blur) ~ 0,
            .default=blur
        )
    ) |>
    ggplot() +
    geom_line(aes(x=blur, y=entropy, color = uniqueID)) +
    scale_color_viridis_d(option="rocket") +
    labs(
        title="CLIP Model Uncertainty on Blurred PCRM Stimuli",
        subtitle="ViT-B/32 with 1854 THINGS Concepts as Output",
        x="Blur Level",
        y="Uncertainty (Entropy)",
        color="THINGS Image uniqueID"
    ) +
    theme_classic() +
    theme(legend.position="bottom")

ggsave("clip-blur-uncertainty.png", width=6, height=4)
