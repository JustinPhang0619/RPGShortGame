import System.Random (randomRIO)
import Data.Char (digitToInt)
import Control.Monad (foldM, replicateM)

-- Defining a type class for combatants
class Combatant a where
  getName :: a -> String
  getHP :: a -> Int
  getATK :: a -> Int
  getDEF :: a -> Int
  getSPD :: a -> Int
  getMana :: a -> Int
  setHP :: Int -> a -> a
  setMana :: Int -> a -> a
  setATK :: Int -> a -> a
  setDEF :: Int -> a -> a
  setSPD :: Int -> a -> a
  -- Show general stats for combatants
  showStats :: a -> String
  showStats combatant =
    "HP: " ++ show (getHP combatant) ++
    "\nATK: " ++ show (getATK combatant) ++
    "\nDEF: " ++ show (getDEF combatant) ++
    "\nSPD: " ++ show (getSPD combatant) ++
    case getMana combatant of 
      0 -> ""
      _ -> "\nMana: " ++ show (getMana combatant)

-- Data type representing a player character
data Character = Character
  { hp :: Int,
    atk :: Int,
    def :: Int,
    spd :: Int,
    mana :: Int
  }

-- Implementing the Combatant type class for Character
instance Combatant Character where
  getName _ = "Player"
  getHP = hp
  getATK = atk
  getDEF = def
  getSPD = spd
  getMana = mana
  setHP newHP character = character { hp = newHP }
  setATK newATK character = character { atk = newATK }
  setDEF newDEF character = character { def = newDEF }
  setSPD newSPD character = character { spd = newSPD }
  setMana newMana character = character { mana = newMana }

-- Data type representing an enemy character
data Enemy = Enemy
  { enemyName :: String,
    enemyHP :: Int,
    enemyATK :: Int,
    enemyDEF :: Int,
    enemySPD :: Int
  } deriving (Eq, Show)

-- Implementing the Combatant type class for Enemy
instance Combatant Enemy where
  getName = enemyName
  getHP = enemyHP
  getATK = enemyATK
  getDEF = enemyDEF
  getSPD = enemySPD
  getMana _ = 0
  setHP newHP enemy = enemy { enemyHP = newHP }
  setATK newATK enemy = enemy { enemyATK = newATK }
  setDEF newDEF enemy = enemy { enemyDEF = newDEF }
  setSPD newSPD enemy = enemy { enemySPD = newSPD }
  setMana _ enemy = enemy

-- Data type representing different items
data Item = PowerOrb | ArcaneBarrier | PixieBoots | RegenerationGlobe | GrowthPotion | DivineOrb deriving (Eq, Show)

-- Data type representing different player actions
data Action = Attack | Spells | Revitalize | Observe deriving (Eq)

-- Data type representing different spells
data Spell = Restoration | Smite | Pyroblast deriving  (Eq)

-- Represents turns
type Turn = Int

-- Implement Enum instances for Action
instance Enum Action where
  
  fromEnum Attack = 0
  fromEnum Spells = 1
  fromEnum Revitalize = 2
  fromEnum Observe = 3
  
  toEnum 0 = Attack
  toEnum 1 = Spells
  toEnum 2 = Revitalize
  toEnum 3 = Observe
  toEnum _ = error "Invalid action"

-- Implement Enum instances for Spell
instance Enum Spell where
  
  fromEnum Restoration = 0
  fromEnum Smite = 1
  fromEnum Pyroblast = 2
  
  toEnum 0 = Restoration
  toEnum 1 = Smite
  toEnum 2 = Pyroblast
  toEnum _ = error "Invalid spell"

-- Function to calculate damage in battles
calculateDamage :: (Combatant a, Combatant b) => a -> b -> Double -> Int
calculateDamage attacker defender damageModifier =
  max 1 $ round $ damageModifier * fromIntegral (getATK attacker) - fromIntegral (getDEF defender)

-- Function for player's turn and actions
playerTurn :: Combatant a => Combatant b => a -> b -> Action -> Spell -> IO (a, b)
playerTurn player enemy action spell = case action of
  Attack -> do
    let damage = calculateDamage player enemy 1.0
    putStrLn $ "\nYou attacked " ++ getName enemy ++ " for " ++ show damage ++ " damage!"
    let updatedEnemy = setHP (max 0 (getHP enemy - damage)) enemy
    return (player, updatedEnemy)
  Spells -> case spell of
    Restoration -> do
      let healPercentage = 1.0
      let healAmount = min 10 (round $ healPercentage * fromIntegral (getSPD player))
      putStrLn $ "\nYou used Restoration and healed " ++ show healAmount ++ " HP!"
      let updatedPlayer = setHP (min 100 (getHP player + healAmount)) . setMana (getMana player - 5) $ player
      return (updatedPlayer, enemy)
    Smite -> do
      putStrLn "\nYou used Smite and dealt 12 true damage!"
      let updatedPlayer = setMana (getMana player - 8) player
      let updatedEnemy = setHP (max 0 (getHP enemy - 12)) enemy
      return (updatedPlayer, updatedEnemy)
    Pyroblast -> do
      putStrLn "\nYou used Pyroblast and dealt 20 true damage!"
      let updatedPlayer = setMana (getMana player - 12) player
      let updatedEnemy = setHP (max 0 (getHP enemy - 20)) enemy
      return (updatedPlayer, updatedEnemy) 
  Revitalize -> do
    let recoveryPercentage = 0.9
    let recoveryAmount = min 10 (round $ recoveryPercentage * fromIntegral (getSPD player))
    putStrLn $ "\nYou gathered natural energy around you and recovered " ++ show recoveryAmount ++ " Mana!"
    let updatedPlayer = setMana (min 100(getMana player + recoveryAmount)) player
    return (updatedPlayer, enemy)
  Observe -> do
    putStrLn "\nObserving..."
    putStrLn $ "\nPlayer Stats:\n" ++ showStats player
    putStrLn $ "\nEnemy Stats:\n" ++ showStats enemy
    return (player, enemy)

-- Function to handle enemy's turn
enemyTurn :: (Combatant a, Combatant b) => a -> b -> IO (a, b)
enemyTurn player enemy = do
    putStrLn $ getName enemy ++ " is attacking!"
    case getName enemy of
        "Bokoblin" -> do
            let damage = calculateDamage enemy player 1.2
            let newPlayerHP = max 0 (getHP player - damage)
            putStrLn $ getName enemy ++ " used bash and dealt " ++ show damage ++ " damage!"
            let updatedPlayer = setHP newPlayerHP player
            return (updatedPlayer, enemy)

        "Skeleblin" -> do
            putStrLn "\nSkeleblin is choosing an attack:"
            attackChoice <- randomRIO (1, 3 :: Int)
            case attackChoice of
                1 -> do
                    let damage = calculateDamage enemy player 1.2
                    let newPlayerHP = max 0 (getHP player - damage)
                    putStrLn $ getName enemy ++ " used bash and dealt " ++ show damage ++ " damage!"
                    let updatedPlayer = setHP newPlayerHP player
                    return (updatedPlayer, enemy)

                2 -> do
                    let healAmount = 8
                    let newEnemyHP = min (getHP enemy + healAmount) 20
                    putStrLn $ getName enemy ++ " used reattach and healed " ++ show healAmount ++ " HP!"
                    let updatedEnemy = setHP newEnemyHP enemy
                    return (player, updatedEnemy)

                3 -> do
                    let speedReduction = 2
                    putStrLn $ getName enemy ++ " used scream and reduced your speed by " ++ show speedReduction ++ "!"
                    let updatedPlayer = setSPD (max 0 (getSPD player - speedReduction)) player
                    return (updatedPlayer, enemy)

                _ -> do
                    putStrLn "Invalid attack choice for Abyssal Mage."
                    return (player, enemy)

        "Moblin" -> do
            putStrLn "\nMoblin is choosing an attack:"
            attackChoice <- randomRIO (1, 2 :: Int)
            case attackChoice of
                1 -> do
                    let damage = calculateDamage enemy player 1.3
                    let newPlayerHP = max 0 (getHP player - damage)
                    putStrLn $ getName enemy ++ " used giga bash and dealt " ++ show damage ++ " damage!"
                    let updatedPlayer = setHP newPlayerHP player
                    return (updatedPlayer, enemy)

                2 -> do
                    putStrLn "Moblin is hardening!"
                    let newEnemy = setDEF (getDEF enemy + 5) enemy
                    return (player, newEnemy)

                _ -> do
                    putStrLn "Invalid attack choice for Abyssal Mage."
                    return (player, enemy)

        "Abyssal Mage" -> do
            putStrLn "\nAbyssal Mage is choosing an attack:"
            attackChoice <- randomRIO (1, 3 :: Int)
            case attackChoice of
                1 -> do
                    putStrLn "Abyssal Mage is channeling dark energy..."
                    let damageToPlayer = calculateDamage enemy player 1.6
                    let damageToMage = 10
                    let newPlayerHP = max 0 (getHP player - damageToPlayer)
                    let newMageHP = max 0 (getHP enemy - damageToMage)
                    putStrLn $ getName enemy ++ " used dark sacrifice and dealt " ++ show damageToPlayer ++ " damage to you!"
                    putStrLn "Abyssal Mage takes 10 damage to itself"
                    let updatedPlayer = setHP newPlayerHP player
                    let updatedEnemy = setHP newMageHP enemy
                    return (updatedPlayer, updatedEnemy)

                2 -> do
                    let damage = calculateDamage enemy player 0.8
                    let newPlayerHP = max 0 (getHP player - damage)
                    putStrLn $ getName enemy ++ " shot and dealt " ++ show damage ++ " damage!"
                    let updatedPlayer = setHP newPlayerHP player
                    return (updatedPlayer, enemy)

                3 -> do
                    putStrLn "Abyssal Mage is drawing power from the abyss..."
                    let healAmount = 5
                    let newMageHP = min (getHP enemy + healAmount) 50
                    putStrLn $ getName enemy ++ " used abyssal rejuvenation and healed itself by " ++ show healAmount ++ " HP!"
                    let updatedEnemy = setHP newMageHP enemy
                    return (player, updatedEnemy)
                    
                _ -> do
                    putStrLn "Invalid attack choice for Abyssal Mage."
                    return (player, enemy)

        "Ganon" -> do
            putStrLn "\nThe Boss Ganon is choosing an attack:"
            attackChoice <- randomRIO (1, 3 :: Int)
            case attackChoice of
                1 -> do
                    let damage = calculateDamage enemy player 1.1
                    let newPlayerHP = max 0 (getHP player - damage)
                    putStrLn $ getName enemy ++ " used slashed and dealt " ++ show damage ++ " damage!"
                    let updatedPlayer = setHP newPlayerHP player
                    return (updatedPlayer, enemy)

                2 -> do
                    let healAmount = 8
                    let newEnemyHP = min (getHP enemy + healAmount) 80
                    putStrLn $ getName enemy ++ " used recover and healed " ++ show healAmount ++ " HP!"
                    let updatedEnemy = setHP newEnemyHP enemy
                    return (player, updatedEnemy)

                3 -> do
                    let speedReductionPercentage = 20
                    let speedReduction = round (0.01 * fromIntegral speedReductionPercentage * fromIntegral (getSPD player))
                    putStrLn $ getName enemy ++ " used laugh and reduced your speed by " ++ show speedReductionPercentage ++ "%!"
                    let updatedPlayer = setSPD (max 0 (getSPD player - speedReduction)) player
                    return (updatedPlayer, enemy)

                4 -> do
                  let drainHopeDamage = calculateDamage enemy player 1.0
                  let drainHopeHeal = 5
                  let newPlayerHP = max 0 (getHP player - drainHopeDamage + drainHopeHeal)
                  putStrLn $ getName enemy ++ " used drain hope and dealt " ++ show drainHopeDamage ++ " damage to you and recovered " ++ show drainHopeHeal ++ " HP!"
                  let updatedPlayer = setHP newPlayerHP player
                  return (updatedPlayer, enemy)

                5 -> do
                  putStrLn $ getName enemy ++ " just smirks at you and demeans you."
                  return (player, enemy)

                _ -> do
                    putStrLn "Invalid attack choice for Abyssal Mage."
                    return (player, enemy)

        _ -> return (player, enemy)

    
-- Function to determine which spell the user chooses
chooseSpell :: Combatant a => Turn -> a -> IO Spell
chooseSpell turn combatant = do
  putStrLn "\nChoose a spell:"
  putStrLn "1. Restoration (Heal based on 100% of SPD, costs 5 mana)"
  putStrLn "2. Smite (Deal 12 true damage, costs 8 mana)"
  -- Only shows when it is turn 3 onwards
  putStrLn $ if turn >= 3 then "3. Pyroblast (Deal 20 true damage, costs 12 mana and can only be used on turn 3 onwards" else ""
  userInput <- getLine
  case userInput of
    "" -> do
      putStrLn "\nInvalid spell choice. Try again."
      chooseSpell turn combatant
    (x:xs) ->
      if not (null xs) || notElem x (if turn >= 3 then "123" else "12")
        then do
          putStrLn "\nInvalid spell choice. Try again."
          chooseSpell turn combatant
        else 
          return $ toEnum (read [x] - 1)

-- Function to determine which actions the user chooses
getPlayerAction :: IO Action
getPlayerAction = do
  putStrLn "\nChoose your action:"
  putStrLn "1. Attack"
  putStrLn "2. Spells"
  putStrLn "3. Revitalize"
  putStrLn "4. Observe"
  putStrLn "\nAction: "
  userInput <- getLine
  case userInput of
    "" -> do
      putStrLn "\nInvalid action. Try again."
      getPlayerAction
    (x:xs) ->
      if not (null xs) || notElem x "1234"
        then do
          putStrLn "\nInvalid action. Try again."
          getPlayerAction
        else do
          let action = toEnum (digitToInt x - 1)
          putStrLn ""
          return action

-- Function to show status of the entities
showStatus :: (Combatant a, Combatant b) => a -> b -> IO ()
showStatus player enemy = do
  putStrLn $ "\nCurrent State:" ++ "\nHP: " ++ show (getHP player) ++ "\nMana: " ++ show (getMana player)
  putStrLn $ "\nEnemy:" ++ "\nHP: " ++ show (getHP enemy)

-- Function for the main battle
runBattle :: Combatant a => Combatant b => a -> b -> Turn -> IO (a, b)
runBattle player enemy turn = do
    putStrLn $ "\nA wild " ++ getName enemy ++ " appears!"
    putStrLn $ "\nTurn: " ++ show turn

    showStatus player enemy

    -- When the player is faster 
    if getSPD player >= getSPD enemy
      then do
        action <- getPlayerAction
        spell <- if action == Spells then chooseSpell turn player else return Restoration
        (playerAfterTurn, enemyAfterPlayerTurn) <- playerTurn player enemy action spell
        if getHP enemyAfterPlayerTurn <= 0
          then do
            putStrLn $ "You defeated " ++ getName enemy ++ "!"
            (updatedPlayer, rewardItem) <- handleItemReward playerAfterTurn
            let updatedPlayerWithMana = setMana (getMana updatedPlayer + 10) updatedPlayer
            return (updatedPlayerWithMana, enemyAfterPlayerTurn)
          else do
            (finalPlayer, finalEnemy) <- enemyTurn playerAfterTurn enemyAfterPlayerTurn
            if getHP finalPlayer <= 0
              then do
                putStrLn "\nYou were defeated. Game over!"
                return (finalPlayer, finalEnemy)
              else do
                putStrLn "\n------------------------------------------------------------------------"
                runBattle finalPlayer finalEnemy (turn + 1)
      -- When the enemy is faster
      else do
        (playerAfterEnemyTurn, enemyAfterTurn) <- enemyTurn player enemy
        if getHP playerAfterEnemyTurn <= 0
          then do
            putStrLn "\nYou were defeated. Game over!"
            return (playerAfterEnemyTurn, enemyAfterTurn)
          else do
            action <- getPlayerAction
            spell <- if action == Spells then chooseSpell turn player else return Restoration
            (finalPlayer, finalEnemy) <- playerTurn playerAfterEnemyTurn enemyAfterTurn action spell
            if getHP finalEnemy <= 0
              then do
                putStrLn $ "\nYou defeated " ++ getName enemy ++ "!"
                (updatedPlayer, rewardItem) <- handleItemReward finalPlayer

                let updatedPlayerWithMana = setMana (getMana updatedPlayer + 10) updatedPlayer
                return (updatedPlayerWithMana, finalEnemy)
              else do
                putStrLn "\n------------------------------------------------------------------------"
                runBattle finalPlayer finalEnemy (turn + 1)
            
    
-- Handles the logic of each item
useItem :: Combatant a => a -> Item -> IO a
useItem combatant item =
  case item of
    PowerOrb -> do
      putStrLn "You used Power Orb and gained +5 ATK!"
      return $ setATK (getATK combatant + 5) combatant
    ArcaneBarrier -> do
      putStrLn "You used Arcane Barrier and gained +3 DEF!"
      return $ setDEF (getDEF combatant + 3) combatant
    PixieBoots -> do
      putStrLn "You used Pixie Boots and gained +3 SPD!"
      return $ setSPD (getSPD combatant + 3) combatant
    RegenerationGlobe -> do
      putStrLn "You used Regeneration Globe and healed 15 HP and 10 Mana!"
      return $ setHP (getHP combatant + 15) $ setMana (getMana combatant + 10) combatant
    GrowthPotion -> do
      putStrLn "You used Growth Potion and gained +2 ATK, +1 SPD, and +1 DEF!"
      return $ setATK (getATK combatant + 2) $ setSPD (getSPD combatant + 1) $ setDEF (getDEF combatant + 1) combatant
    DivineOrb -> do
      putStrLn "You used Divine Orb and gained +10 HP, 10 Mana, +3 ATK, +2 DEF, and +2 SPD!"
      return $ setHP (getHP combatant + 10) $ setMana (getMana combatant + 10) $ setATK (getATK combatant + 3) $ setDEF (getDEF combatant + 2) $ setSPD (getSPD combatant + 2) combatant

-- Randomly determines which items to let the player choose
getRandomItem :: IO Item
getRandomItem = do
  randomIndex <- randomRIO (0, 5 :: Int)
  return $ case randomIndex of
    0 -> PowerOrb
    1 -> ArcaneBarrier
    2 -> PixieBoots
    3 -> RegenerationGlobe
    4 -> GrowthPotion
    5 -> DivineOrb
    _ -> error "Invalid input! Try again."

-- Handles the logic of showing the item choices and to show the stats after picking
handleItemReward :: Combatant a => a -> IO (a, Item)
handleItemReward initialCombatant = do
  putStrLn "\nYou found three items!"
  items <- replicateM 3 getRandomItem
  selected <- getPlayerChoice items
  putStrLn $ "You selected: " ++ show selected
  updatedCombatant <- foldM useItem initialCombatant [selected]
  putStrLn $ "Updated stats: " ++ "\n" ++ showStats updatedCombatant
  return (updatedCombatant, selected)

-- Handles the logic of how the user chooses the item
getPlayerChoice :: [Item] -> IO Item
getPlayerChoice items = do
  putStrLn "Choose one item:"
  mapM_ (\(index, item) -> putStrLn $ show index ++ ". " ++ show item) (zip [1..] items)
  userInput <- getLine
  case reads userInput of
    [(choice, _ )] | choice >= 1 && choice <= length items -> return (items !! (choice - 1))
    _ -> do
      putStrLn "Invalid choice. Please enter a valid number."
      getPlayerChoice items

-- Updates the game state each time it is utilized
updateAndRun :: Combatant a => Combatant b => a -> b -> Turn -> IO a
updateAndRun player enemy turn = do
  (finalPlayer, _) <- runBattle player enemy turn
  return finalPlayer

-- Home screen for the game
mainMenu :: IO ()
mainMenu = do
  putStrLn "------------------------------------------------------------------------"
  putStrLn "                            TALES OF THE TOWER                          "
  putStrLn "------------------------------------------------------------------------"
  putStrLn "1. Play"
  putStrLn "2. Instructions"
  putStrLn "3. Story"
  putStrLn "4. Exit"
  putStrLn "Choose an option: "
  choice <- getLine
  case choice of
    "1" -> playGame >> mainMenu
    "2" -> showInstructions >> mainMenu
    "3" -> showStory >> mainMenu
    "4" -> putStrLn "Goodbye!"
    _ -> putStrLn "Invalid choice. Please enter a valid number." >> mainMenu

-- Initialize the game with set parameters for each entity
playGame :: IO ()
playGame = do
  let initialPlayer = Character { hp = 60 , atk = 20, def = 12, spd = 12, mana = 40}
  let floors = [Enemy "Bokoblin" 20 12 8 10, Enemy "Skeleblin" 15 15 10 10, Enemy "Moblin" 35 20 15 8, Enemy "Abyssal Mage" 80 25 15 12, Enemy "Ganon" 100 25 25 15]
  finalPlayer <- foldM (\player enemy -> updateAndRun player enemy 1 >>= \updatedPlayer -> return updatedPlayer) initialPlayer floors
  putStrLn "Congratulations! You cleared all the floors!"
  putStrLn "Ganon has been defeated and the world is saved!"
  putStrLn "You are now the hero of the world!"
  putStrLn "Exiting the game..."
  putStrLn $ "Final stats: HP " ++ show (getHP finalPlayer) ++ ", ATK " ++ show (getATK finalPlayer) ++ ", DEF " ++ show (getDEF finalPlayer) ++ ", SPD " ++ show (getSPD finalPlayer) ++ ", Mana " ++ show (getMana finalPlayer)

-- Shows the in-depth logic and instructions of the game
showInstructions :: IO ()
showInstructions = do
  putStrLn "------------------------------------------------------------------------"
  putStrLn "                            INSTRUCTIONS                            "
  putStrLn "------------------------------------------------------------------------"
  putStrLn "Welcome to Tales of the Tower"
  putStrLn "1. Choose 'Play' to start the game."
  putStrLn "\nCombat System:"
  putStrLn "1. During the game, you can choose actions like Attack, Spells, Revitalize, or Observe"
  putStrLn "2. Each entity possess a set of ATK, DEF, SPD, and HP stats. For the player, they also have Mana, which lets them casts spells."
  putStrLn "3. ATK determines how much damage you deal to the enemy. DEF determines how much damage you take from the enemy. SPD determines the turn order"
  putStrLn "4. HP determines how much damage you can take before you are defeated. Mana determines what spells you can cast."
  putStrLn "5. Attack deals damage to the enemy based on your ATK and the enemy's DEF stat."
  putStrLn "6. Spells allow you to use special abilities if you fulfill the mana cost. You can choose from Restoration, Smite, or Pyroblast."
  putStrLn "7. Restoration allows you to recover HP based on your SPD stat (Minimum 10 HP). Smite deals 12 true damage to the enemy."
  putStrLn "8. Pyroblast deals 20 true damage to the enemy, but is only active from the 3rd turn onwards."
  putStrLn "9. Revitalize allows you to recover Mana based on your SPD stat (Minimum 10 Mana)."
  putStrLn "10. Observe allows you to view the stats of the enemy and yourself."
  putStrLn "11. You can 'overheal' and restore HP and Mana far beyond your set HP and Mana."
  putStrLn "12. Certain items and abilities can only heal up to a certain amount. Both the player and enemy have a limit on how much HP and Mana (for the player) they can possess."
  putStrLn "\nEnemies:"
  putStrLn "1. There are 5 floors in total, with a boss on the final floor."
  putStrLn "2. Defeat enemies on each floor to progress the tower."
  putStrLn "3. Each enemy has a set of ATK, DEF, SPD, and HP stats."
  putStrLn "4. Each enemy has a unique set of attacks and abilities."
  putStrLn "5. Defeat the boss on the final floor to win the game."
  putStrLn "6. The enemies are: Bokoblin, Skeleblin, Moblin, Abyssal Mage, and Ganon."
  putStrLn "\nItems:"
  putStrLn "1. Gain items to enhance your character."
  putStrLn "2. There are 6 items in total: Power Orb, Arcane Barrier, Pixie Boots, Regeneration Globe, Growth Potion, and Divine Orb."
  putStrLn "3. Power Orb increases your ATK by 5."
  putStrLn "4. Arcane Barrier increases your DEF by 3."
  putStrLn "5. Pixie Boots increases your SPD by 3."
  putStrLn "6. Regeneration Globe heals 15 HP and 10 Mana."
  putStrLn "7. Growth Potion increases your ATK by 2 and your DEF and SPD by 1."
  putStrLn "8. Divine Orb increases your HP, Mana, ATK, DEF, and SPD by 10, 10, 3, 2, and 2 respectively."
  putStrLn "10. Each item can appear 2 or more times at the end of a battle. If you are lucky, you may the same item multiple times!"
  putStrLn "\nMiscellaneous:"
  putStrLn "1. There are no save points in the game. Proceed with caution!"
  putStrLn "2. If you try to use a spell without mana, you will fail to cast it and lose a turn. Manage your resources carefully!"
  putStrLn "3. Each time you defeat an enemy, you will gain 10 Mana."
  putStrLn "Press anything to return to the main menu..."
  _ <- getLine
  return ()

-- Shows the story of the game
showStory :: IO ()
showStory = do
  putStrLn "------------------------------------------------------------------------"
  putStrLn "                                 STORY                            "
  putStrLn "------------------------------------------------------------------------"
  putStrLn "You are an adventurer who has reached the summit of a treacherous mountain"
  putStrLn "in pursuit of Ganon, a malevolent sorcerer. Ganon has stolen two ancient"
  putStrLn "artifacts, the 'Emblem of the Severed Fates' and the legendary 'Genesis Crystal'."
  putStrLn "\nWitnessing Ganon's dark magic, you see him fly to the top of a mysterious tower,"
  putStrLn "summoning deadly monsters within to stall anyone who dares to challenge him."
  putStrLn "\nYour quest is clear : fight your way through the tower, defeat Ganon, and"
  putStrLn "prevent him from using the artifacts to plunge the Earth into chaos."
  putStrLn "\nPress anything to return to the main menu..."
  _ <- getLine
  return ()

-- Main gateway to start the program
main :: IO ()
main = do
  putStrLn "Welcome to Tales of the Tower!"
  mainMenu