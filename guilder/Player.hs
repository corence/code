
module Player (
) where

import Data.Map as Map
import Guild

data Player = Player {
    playerGuildToXp :: Map GuildID Integer,
    playerGuild :: GuildID
}

level :: GuildID -> Player -> Maybe Integer
level guildID player = playerXp guildID player >>= (\xp -> xp * guildXpRate (Map.lookup guildID guilds))

playerXp :: GuildID -> Player -> Maybe Integer
playerXp guildID player = Map.lookup guildID (playerGuildToXp player)
