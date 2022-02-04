library(pdftools)
library(tidyverse)

# I'm going to do my best to use "Players" as the people picking, and "Golfers" as the professional athletes.
# We are treating the odds of winning from draftkings as the true probability of winning the tournament for now.
# It's unlikely that we can outperform the Vegas odds to win, and if we can, why are we not rich yet?
# Our choices based on this would have been horrible though :/. Trust the process? 
dat = pdftools::pdf_data("Desktop/Results03_Farmers Insurance.pdf")
# Needs to be fixed very much
dat2 <- do.call(rbind, dat[3:13]) %>% 
  select(y, text) %>% 
  filter(text != 'Woo', !(y %in% c(74, 205))) %>% 
  arrange(y) %>% 
  group_by(y) %>% 
  filter(n() %% 3 == 0) %>% 
  mutate(names = rep(c("First", "Last", "Money"), times = length(y)/3)) %>% 
  filter(names == "Last") %>% 
  group_by(text) %>% 
  summarise(Count = n())

# Odds come from DraftKings (opening pre-tournament odds).
odds = read_csv('Desktop/FarmersInsuranceOdds.csv') %>% mutate(First = gsub(" [^ ]*$", "", Golfer),
                                                               Last = gsub(".* ", "", Golfer)) 
groups = read_csv("Desktop/GolfersGroups.csv") %>% mutate(First = gsub(" [^ ]*$", "", Golfer),
                                                          Last = gsub(".* ", "", Golfer)) 
odds2 = odds %>% 
  left_join(dat2, by = c("Last" = "text")) %>% 
  left_join(groups, by = c("Golfer", "First", "Last")) %>% 
  mutate(Count = replace_na(Count, 0),
         Group = replace_na(Group, 7), 
         PrWin = odds.converter::odds.us2prob(Winner),
         PrT5 = odds.converter::odds.us2prob(`Top 5`),
         PrT10 = odds.converter::odds.us2prob(`Top 10`),
         PrPicked = Count/77,
         PrWinWith = PrWin * 1/(1 + Count)) %>% 
  rows_update(., tibble(First = c("Kamaiu", "Danny"), # Stupid process of matching chosen golfers off the pdf
                        Last = c("Johnson", "Lee"),  # made a couple mistakes ugh
                        Count = c(0, 0)), 
              by = c("First", "Last"))
  
odds2 %>% # 77 players in this week, magic number ooh 
  filter(Group == 1) %>% 
  pivot_longer(cols = starts_with("Pr"),
               names_to = "Result",
               values_to = "Probability") %>% 
  mutate(Result = factor(Result, levels = c("PrPicked", "PrWin", "PrT5", "PrT10"))) %>% 
ggplot(data = .,
       aes(x = reorder(Last, -Probability), y = Probability, fill = Result)) + 
  geom_col(position = "dodge") + 
  ylim(0, 1) + 
  theme_bw() + 
  labs(x = "Golfer (in Group 1)",
       title = "Farmer's Insurance Open - Draftkings opening odds",
       caption = "Note: Morikawa, McIlroy, Cantlay, and Hovland did not play.") +
  scale_fill_discrete(name = "Result", labels = c("Picked", "Win", "Top 5", "Top 10"))

# If a player is chosen often but doesn't win often enough to justify that, you shouldn't pick them.
# The Pr to win with is the Pr to win divided by the number of players who chose that golfer + 1 (us).
# Making a naive assumption that we have a equal chance of winning if we tie on this particular golfer 
# with the other players who chose that golfer.
# Chose to not add other probabilities (ex: Pr to Win) because Pr to win with is so small.
# Added Woods +150 odds to win the 2001 Masters for 63 players for comparison with Rahm at +650 on 63 players.
# Source: https://www.actionnetwork.com/golf/tiger-woods-betting-odds-masters-major-championship-wins
# Woods was also supposedly +125 to win in the 2000 Buick Open, which my favorite golfer ever won, Rocco Mediate,
# and Woods finished T11. 
# Even Tiger with a "40%" chance to win isn't a good pick when many people are going to pick him. Also there is 
# an implied better chance to win with another golfer because no one else is competing with them. 
# Basically never pick whoever is going to be the most popular picks in this format as far as I can tell. 
odds2 %>% 
  filter(Group %in% c(1,3)) %>% 
  select(Group, Last, Count, PrWin)  %>% 
  mutate(Group = as.character(Group)) %>% 
  rows_insert(., tibble(Group = "Tiger Woods", 
                        Last = "2001 Woods", 
                        PrWin = odds.converter::odds.us2prob(150), 
                        Count = 62), 
              by = c("Group", "Last", "PrWin", "Count")) %>% 
  mutate(PrWinWith = PrWin * 1/(1 + Count)) %>% # Double counting our choices from last week for now
  ggplot(data = ., aes(x = factor(Last, levels = Last[order(Group, desc(PrWinWith))]), 
                       y = PrWinWith, 
                       fill = factor(Group, levels = c("1", "3", "Tiger Woods")))) +
  geom_col() +
  labs(x = "Golfer",
       y = 'Probability', 
       title = "Probability to win choosing each player",
       fill = "Group",
       caption = "Note: Morikawa, McIlroy, Cantlay, and Hovland (Group 1) and Casey, Fitzpatrick, Hatton, Na, and Cam Smith (Group 3) did not play.
       \nAdded Tiger's 2001 +150 odds to win the Masters with 63 teams choosing him as a comparison.") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

