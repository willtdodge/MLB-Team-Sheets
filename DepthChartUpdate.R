library(dplyr)
df <- df %>%
  mutate(Team = ifelse(Name == "Kyle Hendricks", "LAA", Team)) %>%
  mutate(Team = ifelse(Name == "T.J. McFarland", "OAK", Team)) %>%
  mutate(Pos = ifelse(Name == "Darell Hernaiz", "3B", Pos)) %>%
  mutate(Pos = ifelse(Name == "Miguel Amaya", "C", Pos)) %>%
  mutate(Pos = ifelse(Name == "Willson Contreras", "1B", Pos)) %>%
  mutate(Pos = ifelse(Name == "Iván Herrera", "C", Pos)) %>%
  mutate(Pos = ifelse(Name == "Alec Burleson", "DH", Pos)) %>%
  mutate(Pos = ifelse(Name == "Nolan Gorman", "Bench", Pos)) %>%
  mutate(`2025wOBA` = ifelse(Name == "Luke Ritter", 0.279, `2025wOBA`)) %>%
  mutate(Pos = ifelse(Name == "Aaron Judge", "RF", Pos))%>%
  mutate(Pos = ifelse(Name == "Sam Huff", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Garrett Stubbs", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Sandy Alcantara", "ACE", Pos))%>%
  mutate(Pos = ifelse(Name == "Edward Cabrera", "SP", Pos))%>%
  mutate(Team = ifelse(Team == "WSN", "WSH", Team))%>%
  mutate(Pos = ifelse(Name == "MacKenzie Gore", "ACE", Pos))%>%
  add_row(Name = "Felix Bautista", Team = "BAL", Pos = "CL", `2025FIP` = 2.95)%>%
  mutate(Team = ifelse(Name == "Sergio Alcantara", "SFG", Team))%>%
  mutate(Pos = ifelse(Name == "Mookie Betts", "SS", Pos)) %>%
  mutate(Pos = ifelse(Name == "Tommy Edman", "CF", Pos)) %>%
  mutate(Pos = ifelse(Name == "James Outman", "RF", Pos)) %>%
  mutate(`2025wOBA` = ifelse(Name == "Ryan Ward", 0.289, `2025wOBA`))%>%
  mutate(Team = ifelse(Name == "Austin Hedges", "CLE", Team)) %>%
  mutate(Pos = ifelse(Name == "Austin Hedges", "Bench", Pos)) %>%
  mutate(Pos = ifelse(Name == "Daniel Schneemann", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Logan Porter", "SFG", Team))%>%
  mutate(Pos = ifelse(Name == "Scott Kingery", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Kyren Paris", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Trey Lipscomb", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Ildemaro Vargas", "FA", Team))%>%
  mutate(Pos = ifelse(Name == "Tyler Black", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Jake Bauers", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Bryse Wilson", "FA", Team))%>%
  add_row(Name = "Jimmy Herget", Team = "COL", Pos = "RP", `2025FIP` = 4.77)%>%
  mutate(Team = ifelse(Name == "Alex Jackson", "CIN", Team))%>%
  mutate(Team = ifelse(Name == "Kenta Maeda", "DET", Team))%>%
  mutate(Team = ifelse(Name == "Nick Allen", "ATL", Team))%>%
  mutate(Pos = ifelse(Name == "Max Schuemann", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Enoli Paredes", "ATL", Team))%>%
  mutate(Pos = ifelse(Name == "Nick Allen", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Nacho Alvarez Jr.", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Alejo Lopez", "OAK", Team))%>%
  mutate(Team = ifelse(Name == "Travis d'Arnaud", "LAA", Team))%>%
  mutate(Pos = ifelse(Name == "Kyren Paris", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Steven Okert", "HOU", Team))%>%
  add_row(Name = "Luis Curvelo", Team = "TEX", Pos = "RP", `2025FIP` = 4.67)%>%
  mutate(Team = ifelse(Name == "Scott Blewett", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Charles Leblanc", "ATL", Team))%>%
  mutate(Pos = ifelse(Name == "Matt Thaiss", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Kevin Newman", "LAA", Team))%>%
  mutate(Pos = ifelse(Name == "Kevin Newman", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Justin Wilson", "BOS", Team))%>%
  mutate(Pos = ifelse(Name == "Eric Wagaman", "MiLB", Pos))%>%
  add_row(Name = "Liam Hendriks", Team = "BOS", Pos = "CL", `2025FIP` = 3.41)%>%
  filter(!(Name == "Brayan Bello" & `2025FIP` == 4.33))%>%
  mutate(Team = ifelse(Name == "Nick Martinez", "CIN", Team))%>%
  mutate(Pos = ifelse(Name == "Graham Ashcraft", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Aramis Garcia", "ARI", Team))%>%
  mutate(Team = ifelse(Name == "Jose Barrero", "STL", Team))%>%
  add_row(Name = "Abimelec Ortiz", Team = "TEX", Pos = "MiLB", `2025wOBA` = .297)%>%
  add_row(Name = "Alejandro Ortiz", Team = "TEX", Pos = "MiLB", `2025wOBA` = .294)%>%
  mutate(Pos = ifelse(Name == "Alex Jackson", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Eric Yang", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Donovan Walton", "NYM", Team))%>%
  mutate(Team = ifelse(Name == "Austin Slater", "CHW", Team))%>%
  mutate(Pos = ifelse(Name == "Austin Slater", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Brooks Baldwin", "SS", Pos))%>%
  mutate(Team = ifelse(Name == "Nicky Lopez", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Rafael Ortega", "NYM", Team))%>%
  add_row(Name = "Justin Hagenman", Team = "NYM", Pos = "RP", `2025FIP` = 4.8)%>%
  mutate(Team = ifelse(Name == "Pablo Reyes", "NYY", Team))%>%
  mutate(Pos = ifelse(Name == "Pablo Reyes", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Yerry Rodriguez", "PIT", Team))%>%
  mutate(Team = ifelse(Name == "Yohel Pozo", "ATL", Team))%>%
  mutate(`2025wOBA` = ifelse(Name == "Endy Rodriguez", .306 , `2025wOBA`))%>%
  mutate(Pos = ifelse(Name == "Luke Ritter", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Jose Siri", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Jose Siri", "NYM", Team))%>%
  mutate(Pos = ifelse(Name == "Jonny DeLuca", "CF", Pos))%>%
  mutate(Pos = ifelse(Name == "Curtis Mead", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Austin Shenton", "SEA", Team))%>%
  mutate(Team = ifelse(Name == "Trenton Brooks", "SDP", Team))%>%
  mutate(Team = ifelse(Name == "Eli Morgan", "CHC", Team))%>%
  mutate(Pos = ifelse(Name == "Patrick Wisdom", "MiLB", Pos))%>%
  add_row(Name = "Moises Ballesteros", Team = "CHC", Pos = "MiLB", `2025wOBA` = .295)%>%
  mutate(Team = ifelse(Name == "Christian Bethancourt", "FA", Team))%>%
  mutate(Pos = ifelse(Name == "Miles Mastrobuoni", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Jacob Stallings", "COL", Team))%>%
  mutate(Pos = ifelse(Name == "Jacob Stallings", "C", Pos))%>%
  mutate(Pos = ifelse(Name == "Hunter Goodman", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Drew Romo", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Logan Driscoll", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Dylan Carlson", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Curtis Mead", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Rene Pinto", "MiLB", Pos))%>%
  add_row(Name = "Jake Mangum", Team = "TBR", Pos = "MiLB", `2025wOBA` = .282)%>%
  add_row(Name = "Tre' Morgan", Team = "TBR", Pos = "MiLB", `2025wOBA` = .294)%>%
  mutate(Team = ifelse(Name == "Joey Wiemer", "KCR", Team))%>%
  mutate(Team = ifelse(Name == "Jonathan India", "KCR", Team))%>%
  mutate(Pos = ifelse(Name == "Jonathan India", "DH", Pos))%>%
  mutate(Pos = ifelse(Name == "Nick Pratto", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Drew Waters", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Brady Singer", "CIN", Team))%>%
  mutate(Pos = ifelse(Name == "Rhett Lowder", "RP", Pos))%>%
  mutate(Pos = ifelse(Name == "Alec Marsh", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "Christian Encarnacion-Strand", "DH", Pos))%>%
  mutate(Pos = ifelse(Name == "Santiago Espinal", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Noelvi Marte", "3B", Pos))%>%
  mutate(Pos = ifelse(Name == "Yusei Kikuchi", "ACE", Pos))%>%
  mutate(Pos = ifelse(Name == "Kyle Hendricks", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "José Soriano", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "Jack Kochanowicz", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Yusei Kikuchi", "LAA", Team))%>%
  mutate(Team = ifelse(Name == "Patrick Sandoval", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Blake Snell", "LAD", Team))%>%
  mutate(Team = ifelse(Name == "Giovanny Gallegos", "LAD", Team))%>%
  mutate(Team = ifelse(Name == "Brent Honeywell", "FA", Team))%>%
  mutate(Pos = ifelse(Name == "Dustin May", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Jackson Reetz", "NYM", Team))%>%
  mutate(Team = ifelse(Name == "Austin Hays", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Jordan Romano", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Gavin Sheets", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Brent Honeywell", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Ramón Laureano", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Griffin Canning", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Hoby Milner", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Brent Honeywell", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Nick Madrigal", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Mike Tauchman", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Kyle Finnegan", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Tanner Rainey", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Bryce Johnson", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Mason McCoy", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Bryan De La Cruz", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Connor Joe", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Hunter Stratton", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Cal Quantrill", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Brendan Rodgers", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Eric Wagaman", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Bryce Teodosio", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Jordyn Adams", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Dany Jiménez", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Brent Honeywell", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Dillon Tate", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Brent Honeywell", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Sam Haggerty", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Josh Rojas", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Austin Voth", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Jacob Webb", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Dylan Carlson", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Tyler Alexander", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Colin Poche", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Richard Lovelady", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Jon Berti", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Enyel De Los Santos", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Frankie Montas", "NYM", Team))%>%
  mutate(Pos = ifelse(Name == "José Buttó", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "DJ Stewart", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Luis De Los Santos", "NYM", Team))%>%
  mutate(Pos = ifelse(Name == "Luis De Los Santos", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Matthew Boyd", "CHC", Team))%>%
  mutate(Pos = ifelse(Name == "Ben Brown", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Matt Thaiss", "CHC", Team))%>%
  mutate(Pos = ifelse(Name == "Matt Thaiss", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Patrick Wisdom", "FA", Team))%>%
  mutate(Team = ifelse(Name == "David Bote", "FA", Team))%>%
  mutate(Team = ifelse(Name == "Kyle Higashioka", "TEX", Team))%>%
  mutate(Team = ifelse(Name == "Aroldis Chapman", "BOS", Team))%>%
  mutate(Pos = ifelse(Name == "Kyle Higashioka", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Sam Huff", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Luis Severino", "OAK", Team))%>%
  mutate(Pos = ifelse(Name == "Luis Medina", "RP", Pos))%>%
  mutate(Pos = ifelse(Name == "Luis Severino", "ACE", Pos))%>%
  mutate(Pos = ifelse(Name == "JP Sears", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "Ben Rortvedt", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Danny Jansen", "TBR", Team))%>%
  mutate(Pos = ifelse(Name == "Mason McCoy", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Mason McCoy", "SDP", Team))%>%
  mutate(Pos = ifelse(Name == "Clay Holmes", "SP", Pos))%>%
  mutate(Team = ifelse(Name == "Bryce Teodosio", "LAA", Team))%>%
  mutate(Team = ifelse(Name == "Jordan Groshans", "KCR", Team))%>%
  mutate(Team = ifelse(Name == "Ryan Vilade", "STL", Team))%>%
  mutate(Pos = ifelse(Name == "Kris Bubic", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "Kyle Wright", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Ryan Vilade", "STL", Team))%>%
  add_row(Name = "Connor Gillispe", Team = "ATL", Pos = "RP", `2025FIP` = 4.71)%>%
  add_row(Name = "Shane Bieber", Team = "CLE", Pos = "SP", `2025FIP` = 3.83)%>%
  mutate(Team = ifelse(Name == "Jose Devers", "ATL", Team))%>%
  add_row(Name = "Ray Kerr", Team = "ATL", Pos = "RP", `2025FIP` = 3.99)%>%
  add_row(Name = "Royber Salinas", Team = "ATL", Pos = "RP", `2025FIP` = 4.91)%>%
  mutate(Team = ifelse(Name == "Oscar Gonzalez", "SDP", Team))%>%
  mutate(Pos = ifelse(Name == "Triston McKenzie", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Michael Stefanic", "TOR", Team))%>%
  mutate(Team = ifelse(Name == "Génesis Cabrera", "NYM", Team))%>%
  mutate(Team = ifelse(Name == "Jared Oliva", "MIL", Team))%>%
  add_row(Name = "Max Stassi", Team = "SFG", Pos = "MiLB", `2025wOBA` = .286)%>%
  mutate(Team = ifelse(Name == "Yunior Marte", "SEA", Team))%>%
  mutate(Team = ifelse(Name == "Kyle Farmer", "COL", Team))%>%
  mutate(Pos = ifelse(Name == "Kyle Farmer", "2B", Pos))%>%
  mutate(Team = ifelse(Name == "Clay Holmes", "NYM", Team))%>%
  mutate(Pos = ifelse(Name == "Tylor Megill", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Willy Adames", "SFG", Team))%>%
  mutate(Pos = ifelse(Name == "Tyler Fitzgerald", "2B", Pos))%>%
  mutate(Pos = ifelse(Name == "Casey Schmitt", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Jerar Encarnacion", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Nick Maton", "CHW", Team))%>%
  mutate(Team = ifelse(Name == "Adrian Houser", "TEX", Team))%>%
  mutate(Team = ifelse(Name == "Tyler O'Neill", "BAL", Team))%>%
  mutate(Pos = ifelse(Name == "Colton Cowser", "RF", Pos))%>%
  mutate(Pos = ifelse(Name == "Blake Hunt", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Heston Kjerstad", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Daz Cameron", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Rene Pinto", "BAL", Team))%>%
  mutate(Pos = ifelse(Name == "Rene Pinto", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Livan Soto", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Gary Sánchez", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Gary Sánchez", "BAL", Team))%>%
  mutate(Pos = ifelse(Name == "Rene Pinto", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Michael Conforto", "LAD", Team))%>%
  mutate(Pos = ifelse(Name == "Michael Conforto", "RF", Pos))%>%
  mutate(Pos = ifelse(Name == "James Outman", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Ryan Ward", "MiLB", Pos))%>%
  filter(!(Name == "Miles Mikolas" & `2025FIP` == 4.61))%>%
  mutate(Team = ifelse(Name == "Juan Soto", "NYM", Team))%>%
  mutate(Pos = ifelse(Name == "Starling Marte", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Luis De Los Santos", "MiLB", Pos))%>%
  mutate(Team = ifelse(Name == "Blake Treinen", "LAD", Team))%>%
  mutate(Pos = ifelse(Name == "Jarred Kelenic", "LF", Pos))%>%
  mutate(Pos = ifelse(Name == "Nick Allen", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Grant Holmes", "SP", Pos))%>%
  mutate(Team = ifelse(Name == "Austin Nola", "COL", Team))%>%
  mutate(Team = ifelse(Name == "Hunter Stratton", "PIT", Team))%>%
  mutate(Team = ifelse(Name == "Tanner Rainey", "PIT", Team))%>%
  mutate(Team = ifelse(Name == "Dakota Hudson", "LAA", Team))%>%
  mutate(Pos = ifelse(Name == "Dakota Hudson", "RP", Pos))%>%
  add_row(Name = "Jordan Romano", Team = "PHI", Pos = "CL", `2025FIP` = 3.91)%>%
  mutate(Pos = ifelse(Name == "Orion Kerkering", "RP", Pos))%>%
  mutate(Pos = ifelse(Name == "Brandon Marsh", "LF", Pos))%>%
  mutate(Pos = ifelse(Name == "Johan Rojas", "CF", Pos))%>%
  mutate(Pos = ifelse(Name == "Cal Stevenson", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Carson Kelly", "CHC", Team))%>%
  mutate(Pos = ifelse(Name == "Carson Kelly", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Luis Vazquez", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Matt Thaiss", "MiLB", Pos))%>%
  add_row(Name = "Alex Cobb", Team = "DET", Pos = "SP", `2025FIP` = 3.99)%>%
  mutate(Pos = ifelse(Name == "Keider Montero", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Mike Tauchman", "CHW", Team))%>%
  mutate(Pos = ifelse(Name == "Mike Tauchman", "RF", Pos))%>%
  mutate(Pos = ifelse(Name == "Zach DeLoach", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Dominic Fletcher", "DH", Pos))%>%
  filter(!(Name == "Davis Martin" & `2025FIP` == 4.45))%>%
  mutate(Team = ifelse(Name == "Yimi García", "TOR", Team))%>%
  mutate(Team = ifelse(Name == "Thairo Estrada", "COL", Team))%>%
  mutate(Pos = ifelse(Name == "Thairo Estrada", "2B", Pos))%>%
  mutate(Pos = ifelse(Name == "Kyle Farmer", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Aaron Schunk", "MiLB", Pos))%>%
  mutate(Pos = ifelse(Name == "Yimi García", "CL", Pos))%>%
  mutate(Team = ifelse(Name == "Max Fried", "NYY", Team))%>%
  mutate(Pos = ifelse(Name == "Max Fried", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "Marcus Stroman", "RP", Pos))%>%
  mutate(Pos = ifelse(Name == "Ben Rice", "1B", Pos))%>%
  mutate(Pos = ifelse(Name == "DJ LeMahieu", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Andrés Giménez", "TOR", Team))%>%
  mutate(Pos = ifelse(Name == "Will Wagner", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Davis Schneider", "DH", Pos))%>%
  mutate(Team = ifelse(Name == "Spencer Horwitz", "CLE", Team))%>%
  mutate(Pos = ifelse(Name == "Spencer Horwitz", "2B", Pos))%>%
  mutate(Team = ifelse(Name == "Nick Sandlin", "TOR", Team))%>%
  mutate(Team = ifelse(Name == "Nathan Eovaldi", "TEX", Team))%>%
  mutate(Pos = ifelse(Name == "Cody Bradford", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Spencer Horwitz", "PIT", Team))%>%
  mutate(Pos = ifelse(Name == "Spencer Horwitz", "1B", Pos))%>%
  mutate(Pos = ifelse(Name == "Billy Cook", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Jason Delay", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Joshua Palacios", "RF", Pos))%>%
  mutate(Team = ifelse(Name == "Luis L. Ortiz", "CLE", Team))%>%
  add_row(Name = "Johan Oviedo", Team = "PIT", Pos = "SP", `2025FIP` = 4.99)%>%
  mutate(Pos = ifelse(Name == "Angel Martínez", "2B", Pos))%>%
  mutate(Pos = ifelse(Name == "Joey Cantillo", "RP", Pos))%>%
  mutate(Team = ifelse(Name == "Jake Burger", "TEX", Team))%>%
  mutate(Pos = ifelse(Name == "Josh Smith", "Bench", Pos))%>%
  mutate(Pos = ifelse(Name == "Jonathan Ornelas", "Bench", Pos))%>%
  mutate(Team = ifelse(Name == "Garrett Crochet", "BOS", Team))%>%
  mutate(Pos = ifelse(Name == "Tanner Houck", "SP", Pos))%>%
  mutate(Pos = ifelse(Name == "Quinn Priester", "RP", Pos))%>%
  mutate(Pos = ifelse(Name == "Drew Thorpe", "ACE", Pos))%>%
  mutate(Pos = ifelse(Name == "Nick Nastrini", "SP", Pos))
  
