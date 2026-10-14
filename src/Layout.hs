module Layout where

import Text.Megaparsec.Pos
import Lexer

data LayoutError
  = UnmatchedClose   SourcePos
  | UnmatchedOpen    SourcePos
  | MismatchedClose  SourcePos
  | IndentationError SourcePos Int
  | CloseInBlock     SourcePos
  | HangingBlock     SourcePos
  | UnexpectedEOF
  deriving (Eq, Show)

startLine :: TokInfo -> Int
startLine = unPos . sourceLine . tokStart

endLine :: TokInfo -> Int
endLine = unPos . sourceLine . tokEnd

getCol :: TokInfo -> Int
getCol = unPos . sourceColumn . tokStart

withInfo :: TokInfo -> Tok -> TokInfo
withInfo ref tok = ref { tokVal = tok }

preLay :: [TokInfo] -> [TokInfo]
preLay (curr : rest@(next : _))
  | elem curr.tokVal [TKey "do", TKey "where"] && next.tokVal /= TBracket '{'
    = curr : withInfo next (TLayBrace (getCol next)) : preLay rest
  | next.tokVal /= TEOF && endLine curr < startLine next
    = curr : withInfo next (TLayAngle (getCol next)) : preLay rest
  | otherwise
    = curr : preLay rest
preLay [curr] = [curr]
preLay []          = []

pattern V :: Tok -> TokInfo
pattern V t <- TokInfo { tokVal = t }

pattern ExplCurly :: Int
pattern ExplCurly = 0

pattern ExplParen :: Int
pattern ExplParen = - 1

pattern ExplSquare :: Int
pattern ExplSquare = - 2

-- NB: copied as directly as possible from Haskell Report Section 10.3.
-- only difference is that @parse-error(t)@ rule is omitted to make it simpler
-- unlike Haskell, ToffeeScript has no juxtaposition application (@f x@ is not allowed)
-- so two adjacent expressions with no separator are always a parse error
lay :: [TokInfo] -> [Int] -> Either LayoutError [TokInfo]
-- inside implicit context
lay (t@(V (TLayAngle n)) : ts) (m : ms)
  | n == m, contKey ts                            = lay ts (m : ms)
  | n == m                                        = (withInfo t TSemiColon :) <$> lay ts (m : ms)
  | n < m                                         = (withInfo t (TBracket '>') :) <$> lay (withInfo t (TLayAngle n) : ts) ms
lay ((V (TLayAngle _)) : ts) ms                   = lay ts ms

-- opened implicit context
lay (t@(V (TLayBrace n)) : ts) (m : ms)
  | n > m                                         = (withInfo t (TBracket '<') :) <$> lay ts (n : m : ms)
lay (t@(V (TLayBrace n)) : ts) []
  | n > 0                                         = (withInfo t (TBracket '<') :) <$> lay ts [n]
lay (t@(V (TLayBrace n)) : ts) ms                 = do
    res <- lay (withInfo t (TLayAngle n) : ts) ms
    return $ withInfo t (TBracket '<') : withInfo t (TBracket '>') : res

-- opened explicit context
lay (t@(V (TBracket '{')) : ts) ms                = (t :) <$> lay ts (ExplCurly : ms)
lay (t@(V (TBracket '(')) : ts) ms                = (t :) <$> lay ts (ExplParen : ms)
lay (t@(V (TBracket '[')) : ts) ms                = (t :) <$> lay ts (ExplSquare : ms)

-- closed explicit context
lay (t@(V (TBracket '}')) : ts) (ExplCurly : ms)  = (t :) <$> lay ts ms
lay (t@(V (TBracket ')')) : ts) (ExplParen : ms)  = (t :) <$> lay ts ms
lay (t@(V (TBracket ']')) : ts) (ExplSquare : ms) = (t :) <$> lay ts ms
lay (t@(V (TBracket c)) : _) (m : _)
  | elem c "})]" && m <= 0                        = Left (MismatchedClose (tokStart t))
lay (t@(V (TBracket c)) : _) _
  | c `elem` "})]"                                  = Left (UnmatchedClose (tokStart t))

-- reached eof
lay (t@(V TEOF) : _) ms                           = closeAll t ms

-- layout change not relevant
lay (t : ts) ms                                   = (t :) <$> lay ts ms
lay [] []                                         = Right []
lay [] _                                          = Left UnexpectedEOF

contKey :: [TokInfo] -> Bool
contKey (V (TKey k) : _) = k `elem` ["then", "else", "catch", "finally"]
contKey _                = False

closeAll :: TokInfo -> [Int] -> Either LayoutError [TokInfo]
closeAll eofTok []                = Right [eofTok]
closeAll eofTok (m : _) | m <= 0  = Left (UnmatchedOpen (tokStart eofTok))
closeAll eofTok (_ : ms)          = (withInfo eofTok (TBracket '>') :) <$> closeAll eofTok ms

layout :: [TokInfo] -> Either LayoutError [TokInfo]
layout []         = Right []
layout ts@(t : _) = lay (withInfo t (TLayBrace (getCol t)) : preLay ts) []