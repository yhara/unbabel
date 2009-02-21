module Unbabel where

import Debug.Trace
import Text.Regex
import Data.List
import System.Process
import System.IO
import Text.ParserCombinators.Parsec

------------------------------------------------------------------------script
data Script = Script {
    src :: String,
    langdef :: LangDef
  }

langNameOf script = name (langdef script)
commentOf script = comment (langdef script)
commandOf script = command (langdef script)
argtypeOf script = argtype (langdef script)
stubOf script = stub (langdef script)

------------------------------------------------------------------------signature
data Signature = Signature {
    funcName :: String,
    returnType :: String,
    argTypes :: [String]
  }
  deriving (Eq, Show)

rexpType = "[^[:space:]]+" --"[\\][:alnum:]\\[]+"
rexpSignature = "[[:blank:]]*(" ++ rexpType ++ ")[[:blank:]]*::" ++
 "([[:blank:]]*"++rexpType++"[[:blank:]]*(->[[:blank:]]*"++rexpType++"[[:blank:]]*)+)"

signatureOf :: Script -> Signature
signatureOf script = 
  let regex = mkRegex (commentOf script ++ rexpSignature) in
  case matchRegex regex (src script) of
    Just matched -> let [name, typesStr, _] = matched
                        types = words $ subRegex (mkRegex "-.") typesStr "" 
                    in Signature name (last types) (init types)
    Nothing -> error "failed to parse signature, or signature not found"

------------------------------------------------------------------------langdef
data LangDef = LangDef {
    name :: String,
    comment :: String,
    command :: String,
    argtype :: String,
    stub :: String
  }
  deriving (Eq, Show)

isStubBegin :: String -> Bool
isStubBegin = (==) "stub:"

parseOpt :: [String] -> [(String, String)]
parseOpt optLines = map parseLine optLines

parseLine :: String -> (String, String)
parseLine line = let (key, value) = break ((==) ':') line
                 in (key, tail value)

parseLangDef :: String -> LangDef
parseLangDef str = 
  let (optLines, stubLines) = break isStubBegin $ lines str
      opts = parseOpt optLines
      stub = unlines $ tail stubLines
      langdef = do name <- lookup "name" opts
                   comment <- lookup "comment" opts 
                   command <- lookup "command" opts
                   argtype <- lookup "argtype" opts
                   return $ LangDef name comment command argtype stub
  in
  case langdef of
    Just langdef -> langdef
    Nothing -> error "failed to parse langdef"

loadLangDef :: String -> IO LangDef
loadLangDef path = do s <- readFile path 
                      return $ parseLangDef s

------------------------------------------------------------------------stub

data Arg = ArgInt Int | ArgStr String | ArgList [Arg]
  deriving (Eq, Show)

data Parens = Parens { 
  pre :: String,
  between :: String,
  post :: String
}

makeStub :: Script -> [Arg] -> String
makeStub script args =
  let sig = signatureOf script in
    gsub "<FUNCTION_DEFINITION>" (src script) $
    gsub "<FUNCTION_NAME>" (funcName sig) $
    gsub "<ARGUMENTS>" (formatArgs args) (stubOf script)
  where
    gsub :: String -> String -> String -> String
    gsub pattern replace original = subRegex (mkRegex pattern) original replace

    formatArgs :: [Arg] -> String
    formatArgs args = concat $ intersperse ", " $ map (formatArg True) args

    formatArg :: Bool -> Arg -> String
    formatArg _ (ArgInt n) = show n
    formatArg _ (ArgStr s) = show s
    formatArg isOuter (ArgList ls) = 
      let (Parens pre between post) = parens isOuter in
        concat [ pre
               , concat (intersperse between $ map (formatArg False) ls)
               , post
               ]

    parens :: Bool -> Parens
    parens isOuter =
      case (argtypeOf script) of
        "square" -> Parens "[" ", " "]"
        "paren" -> Parens "(" ", " ")"
        "sexp" -> Parens (if isOuter then "'(" else "(") " " ")"

------------------------------------------------------------------------parser

data Value = IntValue Int | StrValue String | ListValue [Value]
  deriving (Eq, Show)

parseInt :: Parser Value
parseInt = do cs <- many1 digit
              return $ IntValue (read cs)

parseStr :: Parser Value
parseStr = do char '"'
              x <- many stringParts
              char '"'
              return $ StrValue $ concat x
  where 
    stringParts :: Parser String
    stringParts = do c1 <- char '\\' 
                     c2 <- anyChar
                     return $ c1:[c2]
              <|> do c <- try $ noneOf "\""
                     return $ [c]

parseList :: Parser Value
parseList = do char '('
               ls <- sepBy parseSexp (skipMany1 space)
               char ')'
               return $ ListValue ls

parseSexp :: Parser Value
parseSexp = parseInt
        <|> parseStr
        <|> parseList

parseResult :: String -> Script -> Value
parseResult result script =
  case parse parseSexp "" result of
    Left _  -> error $ "failed to parse result: " ++ result
    Right v -> typeCheck (returnType $ signatureOf script) v
  where
    typeCheck :: String -> Value -> Value
    typeCheck "String" (StrValue v) = StrValue v
    typeCheck "Int" (IntValue v)    = IntValue v
    typeCheck ('[':elemtype) (ListValue v) = 
      case last elemtype of
        ']' -> ListValue $ map (\x -> typeCheck (init elemtype) x) v
        _   -> error $ "parse error in signeture: [" ++ elemtype
    typeCheck wanted given = 
      error $ "type `" ++ wanted ++ "' was wanted but got `" ++ (show given) ++ "'"

------------------------------------------------------------------------runner

runScript :: Script -> [Arg] -> IO Value
runScript script args = 
  do (stdin, stdout, stderr, processHandle) <- runInteractiveCommand (commandOf script)
     hPutStr stdin (makeStub script args)
     result <- hGetContents stdout
     return $ parseResult result script


ruby :: String -> [Arg] -> IO Value
ruby src arg = 
  do langdef <- loadLangDef "../templates/ruby.b21"
     result <- runScript (Script src langdef) arg
     return result
