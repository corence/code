
import Data.Map
import Data.Ratio

main = do
    putStrLn("hi")

data Property = Property {
    name :: String,
    initial_price :: Rational,
    initial_profit :: Integer,
    coefficient :: Rational,
    initial_time :: Integer
}

data PropertyStatus = PropertyStatus {
    ps_property :: Property,
    ps_amount :: Integer,
    ps_time_multiplier :: Integer,
    ps_profit_multiplier :: Integer
}

lemonade = Property {
    name = "Lemonade Stand",
    initial_price = 1,
    initial_profit = 1,
    coefficient = 1.01,
    initial_time = 2
}

lemonade_status = PropertyStatus {
    ps_property = lemonade,
    ps_amount = 100,
    ps_time_multiplier = 4,
    ps_profit_multiplier = 16
}

data Goal = Goal {
    cost :: Rational,
    effects :: [PropertyStatus]
}

data Unlock = Unlock {
    u_property :: Property,
    u_amount :: Integer,
    u_effect :: PropertyStatus
}

type Upgrade = Goal

cost_for_next status = calculate_cost
    (coefficient property)
    (initial_price property)
    (ps_amount status)
        where property = ps_property status

calculate_cost :: Rational -> Rational -> Integer -> Rational
calculate_cost coefficient initial_cost 1 = initial_cost
calculate_cost coefficient initial_cost amount = initial_cost + calculate_cost coefficient (coefficient * initial_cost) (amount - 1)

properties = [lemonade]
--statuses = [lemonade_status]
statuses = fromList (Prelude.map (\property -> (name property, property)) properties)

purchase_unit_goal :: PropertyStatus -> Goal
purchase_unit_goal status = Goal { cost = cost, effects = [output_status] }
    where cost = cost_for_next status
          output_status = PropertyStatus {
                    ps_property = ps_property status,
                    ps_amount = 1,
                    ps_time_multiplier = 0,
                    ps_profit_multiplier = 0
          }

solve :: [PropertyStatus] -> [Upgrade] -> [Goal]
solve property_statuses upgrades = solve_purchase_units property_statuses ++ solve_purchase_unlocks property_statuses ++ solve_purchase_upgrades property_statuses upgrades

-- base game: [Property], [Upgrade]
-- input: [PropertyStatus], base game
-- output: [Goal] -- which combines goals of "buy 1", "unlock", and "upgrade"
