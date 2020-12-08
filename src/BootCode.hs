module BootCode
  ( parseInstructions
  , Instruction(..)
  , FullInstruction(..)
  , runBootSequence
  , initBootState
  ) where

import qualified Data.Set as Set

data Instruction = NOP | ACC | JMP deriving (Eq, Show)
data FullInstruction = FullInstruction Instruction Int deriving (Eq, Show)

type Index = Int
type Accumulator = Int
data BootState = BootState (Set.Set Int) Index Accumulator

-- PARSING

parseInstructions :: String -> [FullInstruction]
parseInstructions input = map parseLine lns
  where lns = lines input

parseLine :: String -> FullInstruction
parseLine line = FullInstruction (parseInstr instr) cleanNmbr
  where (instr, nmbr) = span (/= ' ') line
        cleanNmbr :: Int
        cleanNmbr = read $ dropWhile (\c -> c == '+' || c == ' ') nmbr

parseInstr :: String -> Instruction
parseInstr instr
  | instr == "nop" = NOP
  | instr == "acc" = ACC
  | otherwise      = JMP

-- RUNNING

initBootState :: BootState
initBootState = BootState Set.empty 0 0

runBootSequence :: [FullInstruction] -> BootState -> Int
runBootSequence instructions state@(BootState set idx acc)
  | isNewInstruction =
       runBootSequence instructions (updateState state (instructions !! idx))
  | otherwise = acc
  where
    isNewInstruction = not $ idx `Set.member` set

updateState :: BootState -> FullInstruction -> BootState
updateState (BootState set idx acc) (FullInstruction inst count)
  | inst == NOP = BootState (idx `Set.insert` set) (idx + 1) acc
  | inst == ACC = BootState (idx `Set.insert` set) (idx + 1) (acc + count)
  | inst == JMP = BootState (idx `Set.insert` set) (idx + count) acc

