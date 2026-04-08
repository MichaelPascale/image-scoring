library(tidyverse)

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
    scale_color_viridis_d(option="turbo") +
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
