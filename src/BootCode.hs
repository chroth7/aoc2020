module BootCode
  ( parseInstructions
  , Instruction(..)
  , FullInstruction(..)
  , runBootSequenceTerminateEverywhere
  , runBootSequenceTerminateEndOnly
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

runBootSequenceTerminateEverywhere :: [FullInstruction] -> BootState -> Int
runBootSequenceTerminateEverywhere instructions state@(BootState set idx acc)
  | isNewInstruction =
       runBootSequenceTerminateEverywhere instructions (updateState state (instructions !! idx))
  | otherwise = acc
  where
    isNewInstruction = not $ idx `Set.member` set

runBootSequenceTerminateEndOnly :: [FullInstruction] -> BootState -> Maybe Int
runBootSequenceTerminateEndOnly instructions state@(BootState set idx acc)
  | idx == length instructions = Just acc
  | idx > length instructions = Nothing
  | isNewInstruction =
       runBootSequenceTerminateEndOnly instructions (updateState state (instructions !! idx))
  | otherwise = Nothing
  where
    isNewInstruction = not $ idx `Set.member` set

updateState :: BootState -> FullInstruction -> BootState
updateState (BootState set idx acc) (FullInstruction inst count)
  | inst == NOP = BootState (idx `Set.insert` set) (idx + 1) acc
  | inst == ACC = BootState (idx `Set.insert` set) (idx + 1) (acc + count)
  | inst == JMP = BootState (idx `Set.insert` set) (idx + count) acc

