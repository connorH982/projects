# projects
Portfolio: academic, learning, and hobby projects


# Temporary Note on steam_tidy.csv for anyone visiting for HW6 in Data Management

Feel free to contact me about this, I don't mind answering questions!

* app_id: Primary Key, that is associated with a specific game (used by Steam and SteamSpy both)

### Sources

(1) Personal Steam account: I visited my steam account saved the page with all my games/hours played (easy way to deal with webpages that have javascript if its a one-off task), and then parsed the HTML to extract the app_id, games and hours played.

(2) Steam Spy: Used the app_id to get the developer, SteamSpy's score, estimated # of owners, price, tag, and tag votes (Steam decides the tags associated with games, such as "platformer", with a user vote). I selected a few tags with a high vote (>0.75 quantile of votes per game), then selected three I'd be interested in from knowing my own tastes. These make up last columns on the far right, and were made indicator columns to keep things tidy.

### A few things to account for

Price is rendered without the dollar sign just due to api I used to collect the data. Correcting it is simple, just do price \* 0.01. Otherwise you'd have 39.99 as 3999, which is way more expensive than the actual cost.

Several games have 0 hours played. These are things I've gotten from sites like https://www.humblebundle.com/ (mostly that). They give you a bundle of games for a very low price (that goes to charity), but if your only looking for 2 out of 8 of the games then you end up lots of games you never play. 
I'd highly recommend filtering games out with 0 hours, or at least putting in an indicator column somewhere. They are essentially random noise games with no playtime that could throw off any analysis.

Didn't realize at the time, but score didn't actually return results from the Steam Spy API for any game. Checking [here](https://steamspy.com/), it seems to be a currently missing value. I'd recommend just removing the column.
