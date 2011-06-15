########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sun Nov  7 16:20:43 2010
########################################################
#Small Multiples of Homicide Rates by State




smallMStates <- function(df, title, kminy, kmaxy,
                         name = "Rate", size = "Homicides",
                         order = "") {
  df$rates <- df[[name]]
  df <- ddply(df, .(State), transform,
                    order = rates[length(rates)])
  df$State <- reorder(df$State, -df$order)
  ggplot(df, aes_string(x = "Year", y = name)) +
      geom_line() +
      geom_point(aes_string(size = size)) +
      scale_size("Number of\nhomicides") +
      ylab("homicide rate") +
      xlab("year") +
      opts(title = title) +
      opts(axis.text.x = theme_text(angle = 60, hjust = 1)) +
      scale_x_continuous(breaks = c(kminy:kmaxy)) +
      scale_y_continuous(limits = c(0, max(df[[name]]))) +
      facet_wrap(~ State)
}

title <- str_c("Homicide Rates in Mexico by State ", "(", kminy,
               "-", kmaxy, ")")
smallMStates(homicide.state, title, kminy, kmaxy)
ggsave("graphs/state-rates.png", dpi = 100,
       width = 9, height = 7)

titlew <- str_c("Female Homicide Rates in Mexico by State ", "(",
               kminy, "-", kmaxy, ")")
smallMStates(homicide.state, titlew, kminy, kmaxy, "RateFemales", "HomicidesFemales")
ggsave("graphs/state-f-rates.png", dpi = 100,
       width = 9, height = 7)



