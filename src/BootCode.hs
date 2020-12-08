module BootCode
  ( parseInstructions
  , Instruction(..)
  , FullInstruction(..)
  , runBootSequenceTerminateEverywhere
  , runBootSequenceTerminateEndOnly
  , initBootState
  , findTermination
  ) where

import           Data.Maybe
import qualified Data.Set   as Set

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
  | idx == length instructions = Just acc -- proper termination
  | idx > length instructions = Nothing -- overshooting
  | isNewInstruction = -- normal operation
       runBootSequenceTerminateEndOnly instructions (updateState state (instructions !! idx))
  | otherwise = Nothing -- this means we are looping
  where
    isNewInstruction = not $ idx `Set.member` set

updateState :: BootState -> FullInstruction -> BootState
updateState (BootState set idx acc) (FullInstruction inst count)
  | inst == NOP = BootState (idx `Set.insert` set) (idx + 1) acc
  | inst == ACC = BootState (idx `Set.insert` set) (idx + 1) (acc + count)
  | inst == JMP = BootState (idx `Set.insert` set) (idx + count) acc

-- Try to fix data
findTermination :: [FullInstruction] -> Int -> Maybe Int
findTermination fullInstrs idxToChange
  | idxToChange > length fullInstrs = Nothing -- we tried everything...
  | instr == ACC = findTermination fullInstrs (idxToChange + 1)
  | otherwise = if isNothing tryToFix
      then findTermination fullInstrs (idxToChange + 1)
      else tryToFix
  where (FullInstruction instr  _) = fullInstrs !! idxToChange
        tryToFix = runBootSequenceTerminateEndOnly (fixedInstructions fullInstrs idxToChange) initBootState

fixedInstructions :: [FullInstruction] -> Int -> [FullInstruction]
fixedInstructions original idx = start ++ changedInstr : end
  where (start,_:end) = splitAt idx original
        FullInstruction instr count = original !! idx
        changedInstr = FullInstruction (flipInstr instr) count

flipInstr :: Instruction -> Instruction
flipInstr ACC = ACC
flipInstr NOP = JMP
flipInstr JMP = NOP

