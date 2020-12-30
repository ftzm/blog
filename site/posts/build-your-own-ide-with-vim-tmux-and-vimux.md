---
title: Build Your Own IDE with Vim, Tmux, and Vimux
published: 2016-04-16
teaser: Ok, maybe not a fully fledged IDE, but running unit tests in an external terminal with a single vim shortcut is pretty slick.
tags: [haskell, tmux, vim]
---

I program in Vim, which means that everything I do when programming--making database migrations, using git, etc.-- happens in a terminal. I like to pack all of those terminals into a Tmux session to keep things organized.

My main focus recently has been TDDD (Test Driven Django Development). I'm testing constantly--so much so that switching back and forth between vim and a dedicated testing terminal once or twice a minute has become a real drag on my workflow.

Thankfully, vim being vim, someone has written a plugin that makes your vim-tmux workflow as seamless as possible. [Vimux](https://github.com/benmills/vimux) is a vim plugin that facilitates communication with the tmux session vim is running in. Its core feature is to open a small tmux pane beneath vim and send commands in it. This is perfect for running tests.

All you need to get unit testing bound to a vim shortcut is a script to find the appropriate tests and run them. I wrote a simple, Django-specific implementation in Haskell (I won't waste an opportunity to practice Haskell).

```Haskell
import System.Directory
import System.FilePath
import System.Process
import Control.Monad
import Data.Maybe
import Control.Exception

dirParents :: FilePath -> [FilePath]
dirParents = takeWhile (/="/") . iterate takeDirectory

inDirConts :: String -> FilePath -> IO Bool
inDirConts x y = elem x <$> getDirectoryContents y

lowMatch :: String -> FilePath -> IO (Maybe FilePath)
lowMatch x y = listToMaybe <$> filterM (inDirConts x) (dirParents y)

cmdArgs :: IO (Maybe [FilePath])
cmdArgs = do
  mod_dir <- getCurrentDirectory >>= lowMatch "__init__.py"
  base_dir <- maybe (return Nothing) (lowMatch "manage.py") mod_dir
  return $ sequence [base_dir, fmap takeBaseName mod_dir]

runCmd :: Maybe [String] -> IO ()
runCmd (Just (x:y:[])) = callProcess "python" [x ++ "/manage.py", "test", y]
runCmd _ = return ()

main :: IO ()
main = catch runtest handler
  where
    flags = readProcess "tmux" ["display-message", "-p", "'#F'"] []
    runtest = do
      cmdArgs >>= runCmd
      (notElem 'Z' <$> flags) >>= flip when (callCommand "tmux resize-pane -Z -t {top}")
      callCommand "tmux display 'Tests Succesful'"
    handler :: SomeException -> IO ()
    handler _ = (elem 'Z' <$> flags) >>= flip when (callCommand "tmux resize-pane -Z")
```

It searches up the directory hierarchy for the root of the python module, then for the directory holding manage.py, then runs unit testing for the module. If the tests fail it reveals the testing terminal so I can see what went wrong, and if the tests pass it hides the testing terminal and flashes a happy message.

Here is more or less the same thing in Python:

```python
import os
from subprocess import call, check_output


def find_up(query_dir, string):
    while query_dir is not "/":
        if string in os.listdir(query_dir):
            return query_dir
        else:
            query_dir = os.path.dirname(query_dir)


def unzoom():
    flags = check_output("tmux display-message -p '#F'".split(' '))
    if "Z" in str(flags):
        call("tmux resize-pane -Z", shell=True)


def assemble_command():
    module_dir = find_up(os.getcwd(), "__init__.py")
    try:
        module_name = os.path.basename(module_dir)
        base_dir = find_up(module_dir, "manage.py")
        return "python {}/manage.py test {}".format(base_dir, module_name)
    except AttributeError:
        return None


def main():
    command = assemble_command()
    if command:
        unzoom()
        call(command, shell=True)

if __name__ == "__main__":
    main()
```

For scripts like the above tot work they need to be run in the same directory, so you'll need to use it in conjunction with VimuxRunCommandInDir("/path/to/script", 0). the 0 indicates that the filename won't be given as an argument to the preceding command. Bind ":call VimuxRunCommandInDir("/path/to/script", 0)" to, say, <leader>t in your vimrc and you're all set.

There are many advantages to using Tmux: terminal multiplexing, detaching and attaching to session, and now with Vimux, integration with Vim to improve your efficiency. Happy testing!
