gp <- games %>%
  select(year, date, home_team, away_team, winner, pts_win, pts_loss) %>%
  arrange(desc(pts_win)) %>%
  slice(1) 

p.max.point <- ggplot(gp) +
  geom_blank() +
  coord_fixed(ratio = 1/200) +
  labs(title = "Max Points",
       subtitle = paste("<br>
                        <b style='color:#ffc205;font-size:35px'>", gp$pts_win, "</b><br>
                        by", gp$winner, "<br>",
                        gp$home_team, "vs.", gp$away_team, "<br>",
                        gp$pts_win, ":", gp$pts_loss, "<br>",
                        gp$year, "", gp$date)) +
  theme(plot.subtitle = element_markdown(hjust = .5),
        plot.title = element_markdown(hjust = .5))



df <- data.frame(
  label = c(gp$pts_win,
            paste("by", gp$winner),
            paste(gp$home_team, "vs.", gp$away_team), 
            paste(gp$pts_win, ":", gp$pts_loss),
            paste(gp$year, gp$date)
  ),
  x = c(2, 2, 2, 2, 2),
  y = c(1.95, 1.7, 1.6, 1.5, 1.4),
  hjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
  color = c("gold", "white", "white", "white", "white"),
  size = c(11, 5, 5, 5, 5))


ggplot(df) +
  aes(x, y, label = label, color = color,
    hjust = hjust, size = size) +
  geom_richtext(label.color = NA, fill = NA, family=font_rc) +
xlim(-10, 13) + ylim(1, 2.1) +
  labs(x = "",
       y = "",
       title = "Max Points") +
  scale_discrete_identity(aesthetics = c("color", "size")) +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5),
        legend.position = "none")

