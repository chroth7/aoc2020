module BootCode
  ( parseInstructions
  , Instruction(..)
  , FullInstruction(..)
  ) where

data Instruction = NOP | ACC | JMP deriving (Eq, Show)
data FullInstruction = FullInstruction Instruction Int deriving (Eq, Show)

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
