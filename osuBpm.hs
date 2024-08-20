import System.Environment
import Data.Maybe
import Data.List
import Data.Typeable
import Text.Read as Tr
import Text.Printf


bpmToCps :: (Fractional a) => a -> a
bpmToCps = (*4) . (/60)

cpsToBpm :: (Fractional a) => a -> a
cpsToBpm = (*60) . (/4)

xToY :: (Fractional a) => a -> a -> a -> a
xToY bpm x y = bpm * x / y

data Action = BpmToCps | CpsToBpm | ThirdsToFourths | XToFourths | XToY deriving (Tr.Read, Show, Enum, Bounded)

newtype FlagType = FlagType (Either Char String)

instance Show FlagType where
    show (FlagType (Left x)) = ['-', x]
    show (FlagType (Right x)) = "--" ++ x

data ConfigError = InvalidFlag String
    | UnknownFlag FlagType
    | ExpectedValue FlagType
    | FlagError String
    | ExpectedInput
    | ExpectedAction
    | ExpectedX
    | ExpectedY

instance Show ConfigError where
    show (InvalidFlag x) = "invalid flag: " ++ x
    show (UnknownFlag x) = "unknown flag: " ++ (show x)
    show (ExpectedValue x) = "flag " ++ (show x) ++ " expects a value"
    show (FlagError x) = x
    show ExpectedInput = "provide an input with -i or --input"
    show ExpectedAction = "provide an action with -a or --action"
    show ExpectedX = "provide x with -x"
    show ExpectedY = "provide y with -y"

data Config = Config{action :: Maybe Action, input :: Maybe Double, x :: Maybe Double, y :: Maybe Double, help :: Bool}

type OnFlagFunc = Config -> Maybe String -> Either ConfigError Config

data ProgramFlagType = ProgramFlagType{
    shortFlag :: Maybe Char,
    longFlag :: Maybe String,
    hasValue :: Bool,
    description :: String,
    onFlag :: OnFlagFunc
}

isFlagEq :: FlagType -> ProgramFlagType -> Bool
isFlagEq (FlagType (Left search)) ProgramFlagType{shortFlag=(Just flag)} = search == flag
isFlagEq (FlagType (Right search)) ProgramFlagType{longFlag=(Just flag)} = search == flag
isFlagEq _ _ = False

programFlagFromTuple :: (Maybe Char, Maybe String, Bool, String, OnFlagFunc) -> ProgramFlagType
programFlagFromTuple (shortFlag, longFlag, hasValue, description, onFlag) =
    ProgramFlagType shortFlag longFlag hasValue description onFlag

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing = Left left
maybeToEither _ (Just x) = Right x

-- im definitely abusing the language with this forall a stuff...
allEnumsString :: forall a . (Enum a, Bounded a, Show a) => a -> String
allEnumsString _ = "options: " ++ (foldr1 (\x acc -> x ++ ", " ++ acc) $ map show [(minBound :: a)..])

class FlagReadable a where
    maybeHelp :: a -> Maybe String

instance FlagReadable Action where
    maybeHelp _ = Just $ allEnumsString (undefined :: Action)

instance FlagReadable Double where
    maybeHelp _ = Nothing

flagRead :: forall flagType . (FlagReadable flagType, Typeable flagType, Tr.Read flagType) => String -> Either ConfigError flagType
flagRead x =
    let typeName = show (typeOf (undefined :: flagType))
        helpMessage = case maybeHelp (undefined :: flagType) of
            Nothing -> ""
            Just message -> " (" ++ message ++ ")"
    in (maybeToEither (FlagError ("cant parse " ++ x ++ " as " ++ typeName ++ helpMessage))) $ readMaybe x

programFlags :: [ProgramFlagType]
programFlags = map programFlagFromTuple [
    (Just 'h', Just "help", False, "show this message",
        (\config Nothing -> Right config{help = True})),
    (Just 'a', Just "action", True, "wut to do",
        (\config (Just x) -> fmap (\a -> config{action = Just a}) $ flagRead x)),
    (Just 'i', Just "input", True, "input to action",
        (\config (Just x) -> fmap (\a -> config{input = Just a}) $ flagRead x)),
    (Just 'x', Nothing, True, "x parameter",
        (\config (Just x) -> fmap (\a -> config{x = Just a}) $ flagRead x)),
    (Just 'y', Nothing, True, "y parameter",
        (\config (Just x) -> fmap (\a -> config{y = Just a}) $ flagRead x))]

showShortFlagInner :: Maybe Char -> Bool -> String
showShortFlagInner Nothing _ = "   "
showShortFlagInner (Just x) hasNext = ['-', x, if hasNext then ',' else ' ']

showShortFlag :: Maybe Char -> Bool -> String
showShortFlag flag hasNext = (showShortFlagInner flag hasNext) ++ if hasNext then " " else ""

showLongFlag :: Maybe String -> String
showLongFlag Nothing = ""
showLongFlag (Just x) = "--" ++ x ++ " "

partialShowFlag :: ProgramFlagType -> String
partialShowFlag ProgramFlagType{shortFlag, longFlag, hasValue} =
    showShortFlag shortFlag (isJust longFlag)
    ++ showLongFlag longFlag
    ++ if hasValue then "VALUE" else ""

flagLength :: ProgramFlagType -> Int
flagLength flag = length $ partialShowFlag flag

longestFlag = foldr1 max $ map flagLength programFlags

paddingFor :: ProgramFlagType -> String
paddingFor flag = replicate (longestFlag - flagLength flag) ' '

instance Show ProgramFlagType where
    show flag@ProgramFlagType{description} = partialShowFlag flag
        ++ paddingFor flag
        ++ "        "
        ++ description

emptyConfig = Config{input = Nothing, action = Nothing, x = Nothing, y = Nothing, help = False}

appendConfigParsed :: Config -> FlagType -> Maybe String -> Either ConfigError (Config, Maybe String)
appendConfigParsed config flag value = case (find (isFlagEq flag) programFlags) of
    Nothing -> Left (UnknownFlag flag)
    Just ProgramFlagType{hasValue, onFlag} -> if hasValue
        then case value of
            Nothing -> Left (ExpectedValue flag)
            x@(Just _) -> fmap (,Nothing) $ onFlag config x
        else fmap (,value) $ onFlag config Nothing

appendConfig :: Config -> String -> Maybe String -> Either ConfigError (Config, Maybe String)
appendConfig config ('-':shortFlag:[]) value = appendConfigParsed config (FlagType (Left shortFlag)) value
appendConfig config ('-':'-':longFlag) value = appendConfigParsed config (FlagType (Right longFlag)) value
appendConfig _ invalidFlag _ = Left (InvalidFlag invalidFlag)

fillConfig :: Config -> [String] -> Either ConfigError Config
fillConfig config [] = Right config

fillConfig config (flag : []) = case appendConfig config flag Nothing of
    Left err -> Left err
    Right (newConfig, _) -> Right newConfig

fillConfig config (flag : maybeValue : tail) = case appendConfig config flag (Just maybeValue) of
    Left err -> Left err
    Right (newConfig, leftover) -> case leftover of
        Nothing -> fillConfig newConfig tail
        Just value -> fillConfig newConfig (value : tail)

runAction :: Config -> Either ConfigError String
runAction Config{action = Just BpmToCps, input = Just input} =
    Right $ printf "%.1f bpm is %.2f cps" input $ bpmToCps input

runAction Config{action = Just CpsToBpm, input = Just input} =
    Right $ printf "%.2f cps is %.1f bpm" input $ cpsToBpm input

runAction Config{action = Just XToY, input = Just input, x = Just x, y = Just y} =
    let flooredX = ((floor x) :: Int)
        flooredY = ((floor y) :: Int)
    in Right $ printf "%.1f bpm stream on 1/%i is the same as %.1f bpm stream on 1/%i"
            input
            flooredX
            (xToY input (fromIntegral flooredX) (fromIntegral flooredY))
            flooredY

runAction config@Config{action = Just XToFourths, input = Just input, x = Just x} =
    runAction config{action = Just XToY, y = Just 4}

runAction config@Config{action = Just ThirdsToFourths, input = Just input} =
    runAction config{action = Just XToFourths, x = Just 3}

runAction Config{action = Just XToFourths, input = Just _, x = Nothing} = Left ExpectedX
runAction Config{action = Just XToY, input = Just _, x = Nothing} = Left ExpectedX

runAction Config{action = Just XToY, input = Just _, x = Just _, y = Nothing} = Left ExpectedY

runAction Config{action = Just _, input = Nothing} = Left ExpectedInput
runAction Config{action = Nothing} = Left ExpectedAction

runConfig :: String -> Config -> Either ConfigError String
runConfig progName Config{help = True} = Right $ usageString progName
runConfig _ config = runAction config

programFlagsString = foldl1 (\acc s -> acc ++ ('\n' : s)) $ map show programFlags

usageString :: String -> String
usageString execPath = "usage: " ++ execPath ++ " [args]\n"
    ++ programFlagsString

main = do
    progName <- getProgName
    args <- getArgs
    case fillConfig emptyConfig args of
        Left err -> putStrLn (show err)
        Right filledConfig -> let returnedValue = runConfig progName filledConfig
            in putStrLn $ case returnedValue of
                Right value -> value
                Left err -> show err
