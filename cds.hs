{-# language LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

import System.Environment
import System.Process
import Data.List
import Data.List.Split

type Nickname = String
type Dir = String
type CdCounter = Int
type NickNameInfo = (Dir, CdCounter)
type NickNameTuple = (Nickname, NickNameInfo)

nicknames_file :: String
nicknames_file = "/.dir_nicknames"

main :: IO ()
main =
  set_nicknames_file >>
  getArgs >>= \case
    ["add", nickname, dir] -> try_to_add_nickname nickname dir
    ["addwd", nickname] -> try_to_addwd_nickname nickname
    ["del", nickname] -> try_to_delete_nickname nickname
    ["list"] -> list
    ["inc_cd_counter", nickname] -> inc_cd_counter nickname
    _ -> error main_err

-- set/get nicknames file
set_nicknames_file :: IO ()
set_nicknames_file =
  getEnv "HOME" >$> (++ nicknames_file) >>= setEnv "nicknames"

get_nicknames_file :: IO Dir
get_nicknames_file = getEnv "nicknames"

-- try to add/addwd/cd/delete/
try_to_add_nickname :: Nickname -> FilePath -> IO ()
try_to_add_nickname nickname dir =
  check_if_exists nickname >>= \case
    Nothing -> add_nickname nickname dir
    Just (old_dir, _) -> print("Nickname already exists for: " ++ old_dir)
  where
  add_nickname :: Nickname -> FilePath -> IO ()
  add_nickname nickname dir =
    get_nicknames_file >>= \nicknames_file ->
    print("\nadding " ++ init dir ++ " as " ++ nickname ++ "\n") >>
    appendFile nicknames_file (nickname ++ "," ++ dir ++ ",0")

try_to_addwd_nickname :: Nickname -> IO ()
try_to_addwd_nickname nickname =
  command_read_output "pwd" >$> filter (/= '\n') >>=
  try_to_add_nickname nickname

try_to_delete_nickname :: Nickname -> IO ()
try_to_delete_nickname nickname =
  get_tuples >>= \tuples ->
  case lookup nickname tuples of
    Nothing -> print("Nickname \"" ++ nickname ++ "\" does not exist")
    Just (dir, _) -> delete_nickname tuples nickname dir

delete_nickname :: [NickNameTuple] -> Nickname -> Dir -> IO ()
delete_nickname tuples nickname dir =
  print("\nDeleting " ++ nickname ++ " pointing to " ++ dir ++ "\n") >>
  (remove_nickname nickname tuples &> tuples_to_file)

list :: IO ()
list =
  get_tuples >$> sort_most_used >$> convert_to_str >>= print
  where
  sort_most_used :: [NickNameTuple] -> [NickNameTuple]
  sort_most_used = sortOn (snd .> snd) .> reverse

  convert_to_str :: [NickNameTuple] -> String
  convert_to_str = map to_pointing_str .> intercalate "\n\n" .> nl_top_bottom

  to_pointing_str :: NickNameTuple -> String
  to_pointing_str (nickname, (dir, _)) = nickname ++ " -> " ++ dir

  nl_top_bottom :: String -> String
  nl_top_bottom = ("\n" ++) .> (++ "\n")

inc_cd_counter :: Nickname -> IO ()
inc_cd_counter nickname =
  get_tuples >>= \tuples ->
  case lookup nickname tuples of
    Nothing -> error inc_cd_counter_err
    Just (dir, cd_counter) ->
      tuples_to_file new_tuples
      where
      new_tuples :: [NickNameTuple]
      new_tuples =
        (nickname, (dir, cd_counter + 1)) : remove_nickname nickname tuples

-- tuples from/to file
get_tuples :: IO [NickNameTuple]
get_tuples =
  get_nicknames_file >>= \nicknames_file ->
  readFile nicknames_file >$> lines >$> filter (/= "") >$> map line_to_tuple
  where
  line_to_tuple :: String -> NickNameTuple
  line_to_tuple =
    splitOn "," .> \case
      [nickname, dir, cd_counter] -> (nickname, (dir, read cd_counter))
      other -> error $ line_to_tuple_err ++ show other

tuples_to_file :: [NickNameTuple] -> IO ()
tuples_to_file tuples =
  get_nicknames_file >>= \nicknames_file ->
  (tuples &> map tuple_to_line &> unlines &> writeFile "/tmp/temp") >>
  callCommand ("mv /tmp/temp " ++ nicknames_file)
  where
  tuple_to_line :: NickNameTuple -> String
  tuple_to_line (nickname, (dir, cd_counter)) =
    nickname ++ "," ++ dir ++ "," ++ show cd_counter

-- helpers
check_if_exists :: Nickname -> IO (Maybe NickNameInfo)
check_if_exists nickname = get_tuples >$> lookup nickname

command_read_output :: String -> IO String
command_read_output command = readProcess command [] []

remove_nickname :: Nickname -> [NickNameTuple] -> [NickNameTuple]
remove_nickname nickname tuples = tuples &> filter (fst .> (/= nickname))

-- to use "print" for strings instead of "putStrLn"
instance {-# OVERLAPS #-} Show String where
  show = id

(>$>) = flip (<$>)
(.>) = flip (.)
x &> f = f x

-- errors
type Error = String

main_err :: Error
main_err = "main: none of the commands was run"

line_to_tuple_err :: Error
line_to_tuple_err = "line_to_tuple: not correct format\n"

inc_cd_counter_err :: Error
inc_cd_counter_err =
  "inc_cd_counter: trying to increase counter on non existant nickname"

