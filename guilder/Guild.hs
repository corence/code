
module Guild (
    GuildID,
    Guild,
    guildName,
    guildXpRate,
    guilds
) where

import Data.Map as Map

type GuildID = String

data Guild = Guild {
    guildName :: String,
    guildXpRate :: Float
}

guilds :: Map GuildID Guild
guilds = fromList $ Prelude.map (\g -> (guildName g, g)) [Guild {
    guildName = "Nomad",
    guildXpRate = 1.1
}]
