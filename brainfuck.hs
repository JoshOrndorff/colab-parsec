-- Episode 3 parser https://www.youtube.com/watch?v=QjGBktGgqdE
-- Episode 4 evaluator https://www.youtube.com/watch?v=VvajXPyKuTo

import Text.Parsec
import Text.Parsec.String
import Control.Monad.State -- Needed this for the evaluator but not for the parser
import qualified Data.IntMap as M -- for representing the tape
import Data.Word -- For representing the characters on the tape?

data BFInst = Prev
            | Next
            | Inc
            | Dec
            | Input
            | Output
            | Loop [BFInst]
            deriving (Show)

-- After we coding the commented parsers, he introduced a way to abstract
parseGen :: Char -> BFInst -> Parser BFInst
parseGen c inst = char c >> return inst

parsePrev :: Parser BFInst
--parsePrev = char '<' >> return Prev -- So return is monad stuff, right?
parsePrev = parseGen '<' Prev

parseNext :: Parser BFInst
--parseNext = char '>' >> return Next
parseNext = parseGen '>' Next

parseInc :: Parser BFInst
--parseInc = char '+' >> return Inc
parseInc = parseGen '+' Inc

parseDec :: Parser BFInst
--parseDec = char '-' >> return Dec
parseDec = parseGen '-' Dec

parseOutput :: Parser BFInst
--parseOutput = char '.' >> return Output
parseOutput = parseGen '.' Output

parseInput :: Parser BFInst
--parseInput = char ',' >> return Input
parseInput = parseGen ',' Input


parseLoop :: Parser BFInst
parseLoop = do
  char '['
  insn <- parseInstructions
  char ']'
  return $ Loop insn

parseComment :: Parser ()
parseComment = do
  many $ noneOf "<>+-.,[]"
  return ()

-- This is the big guy that takes a long sting of syntax and returns a bunch of tokens.
-- Although I guess I just described a lexer, and this is a parser.
-- There is structure in the output because of the loop
parseInstructions :: Parser [BFInst] -- So I guess Parser is the monad, right?
-- parseInstructions = mapM parseInstruction -- he originally tried this but got a type error and switched
-- So what does many do?
parseInstructions = do
  parseComment
  many parseInstruction
  -- I wasn't sure I could put parseComment here. Is there some return value?

-- To parse any individual instruction, we just try the individual parsers we've laready written in the order specified here.
parseInstruction :: Parser BFInst
parseInstruction = do
  i <- parsePrev
   <|> parseNext
   <|> parseInc
   <|> parseDec
   <|> parseOutput
   <|> parseInput
   <|> parseLoop
  parseComment
  return i

-- Int is headpointer, M.IntMap Word8 is tape
-- Transforming the IO monad, no return ()
type BFRunner = StateT (Int, M.IntMap Word8) IO ()

-- Helper that turns empty cells into 0s
-- his version: zeroize = maybe 0 id
zeroize :: Maybe Word8 -> Word8
zeroize (Just w) = w
zeroize Nothing = 0


runInst :: BFInst -> BFRunner
-- Control.Monad.State (modify) :: MonadState s m => (s -> s) -> m ()
runInst Prev = modify (\(h, t) -> (h-1, t))
runInst Next = modify (\(h, t) -> (h+1, t))
runInst Inc = do
  (hd, tape) <- get -- get the initial state
  let old = zeroize (M.lookup hd tape) -- read the character from the tape
  put (hd, M.insert hd (old + 1) tape)-- Put seems to wrap up the final state
runInst Dec = do
  (hd, tape) <- get
  let old = zeroize (M.lookup hd tape)
  put (hd, M.insert hd (old - 1) tape)
runInst Input = do
  (hd, tape) <- get
  new <- liftIO getChar -- read stdin. I think we want an int though
  put (hd, M.insert hd (fromIntegral (fromEnum(new))) tape)
runInst Output = do
  (hd, tape) <- get
  let char = zeroize (M.lookup hd tape)
  liftIO $ putChar $ toEnum $ fromIntegral char -- his line 70
  -- put (hd, tape) -- Is this line not necessary?
runInst (Loop body) = do
  (hd, tape) <- get
  let old = zeroize (M.lookup hd tape)
  if old == 0
    then return () -- could I use put here?
    else runInstructions body >> runInst (Loop body)

runInstructions :: [BFInst] -> BFRunner
runInstructions = mapM_ runInst

main :: IO ()
main = do
  cont <- readFile "hello.bf" -- read the file contents
  -- Why do i need to specify the filename twice here?
  case parse parseInstructions "hello.bf" cont of
    Left e -> print e
    Right prog -> evalStateT (runInstructions prog) (0, M.empty)
