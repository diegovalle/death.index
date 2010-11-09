op.mich <- as.Date("2006-12-11")
op.chi <- as.Date("2008-03-27")

labelJuarez <- function(ll.juarez) {
  ll.juarez$weekly <- ll.juarez$weekly +
    geom_vline(aes(xintercept = op.chi), alpha = .7) +
    geom_vline(aes(xintercept = op.mich), alpha = .7) +
    geom_text(aes(x,y, label = "Joint Operation Chihuahua"),
            data = data.frame(x = op.chi, y = 55),
            size = 4, hjust = 1.01, vjust = 0) +
    geom_text(aes(x,y, label = "Start of the Drug War"),
            data = data.frame(x = op.mich, y = 55),
            size = 4, hjust = 1.01, vjust = 0)
  ll.juarez
}

labelChart <- function(ll, name,
                       label = "",
                       date) {
  ll[[name]] <- ll[[name]] +
    geom_vline(aes(xintercept = op.chi), alpha = .7) +
    geom_text(aes(x,y, label = "Joint Operation Chihuahua"),
      data = data.frame(x = op.chi, y = 55),
      size = 4, hjust = 1.01, vjust = 0)
}

  
