module Layout where

import Text.Megaparsec.Pos
import Lexer
import Debug.Trace (trace)

data LayoutError
  = UnmatchedClose   SourcePos       -- '}' with no matching explicit '{' (never opened)
  | UnmatchedOpen    SourcePos       -- explicit '{' with no matching explicit '{' (never closed)
  | IndentationError SourcePos Int   -- dedent to a column matching no implicit context
  | HangingBlock     SourcePos       -- `do` or `where` with nothing after
  | UnexpectedEOF
  deriving (Eq, Show)

getLne :: TokInfo -> Int
getLne tok = if tok.tokVal == TEOF then 0 else unPos . sourceLine . tokStart $ tok

getCol :: TokInfo -> Int
getCol = unPos . sourceColumn . tokStart

withInfo :: TokInfo -> Tok -> TokInfo
withInfo ref tok = ref { tokVal = tok }

preLay :: [TokInfo] -> [TokInfo]
preLay (curr : tail@(next : rest))
  | elem curr.tokVal [TKey "do", TKey "where"] && next.tokVal /= TBracket '{'
    = curr : withInfo next (TLayBrace (getCol next)) : preLay tail
  | getLne curr < getLne next
    = curr : withInfo next (TLayAngle (getCol next)) : preLay tail
  | otherwise
    = curr : preLay tail
preLay (curr : []) = [curr]
preLay []          = []

pattern V :: Tok -> TokInfo
pattern V t <- TokInfo { tokVal = t }

lay :: [TokInfo] -> [Int] -> Either LayoutError [TokInfo]
lay (t@(V (TLayAngle n)) : ts) (m : ms)
  | n == m -- continued layout equal than current layout -> insert semicolon and do NOT touch stack
    = (withInfo t TSemiColon :) <$> lay ts (m : ms)
  | n < m -- continued layout shallower than current layout -> insert close brace, pop stack, and continuation marker in token stream
    = (withInfo t (TBracket '>') :) <$> lay (withInfo t (TLayAngle n) : ts) ms
lay (t@(V (TLayAngle _)) : ts) ms
    -- continued layout deeper than current layout -> no need to insert anything (considered to be part of same line)
    = lay ts ms

lay (t@(V (TLayBrace n)) : ts) (m : ms)
  | n > m -- opened layout deeper than current layout -> insert open brace and push `n` in stack
    = (withInfo t (TBracket '<') :) <$> lay ts (n : m : ms)
lay (t@(V (TLayBrace n)) : ts) []
  | n > 0 -- opened layout without any layout -> insert open brace and push `n` in stack
    = (withInfo t (TBracket '<') :) <$> lay ts [n]
lay (t@(V (TLayBrace n)) : ts) ms
  -- opened layout but shallower than current layout (no start at all) -> insert empty open and closed curlies
  = do
    res <- lay (withInfo t (TLayAngle n) : ts) ms
    return $ withInfo t (TBracket '<') : withInfo t (TBracket '>') : res

lay (t@(V (TBracket '}')) : ts) (0 : ms)
  -- closed explicit layout inside explicit layout -> insert close brace and pop explicit `0` from stack
  = (t :) <$> lay ts ms
lay (t@(V (TBracket '}')) : _) _
  -- closed explicit layout inside non-explicit layout -> fail
  = Left (UnmatchedClose (tokStart t))
lay (t@(V (TBracket '{')) : ts) ms
  -- open explicit layout always even when no blocks (eg, objects)
  = (t :) <$> lay ts (0 : ms)

lay (t@(V TEOF) : _) ms = closeAll t ms
lay (t : ts) ms         = (t :) <$> lay ts ms
lay [] []               = Right []
lay [] ms               = Left UnexpectedEOF

closeAll :: TokInfo -> [Int] -> Either LayoutError [TokInfo]
closeAll eofTok []       = Right [eofTok]
closeAll eofTok (0 : _)  = Left (UnmatchedOpen (tokStart eofTok))
closeAll eofTok (m : ms) = (withInfo eofTok (TBracket '>') :) <$> closeAll eofTok ms

layout :: [TokInfo] -> Either LayoutError [TokInfo]
layout []         = Right []
layout ts@(t : _) = lay (withInfo t (TLayBrace (getCol t)) : preLay ts) []