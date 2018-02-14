module Unused.GitContext
    ( gitContextForResults
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Process as P
import           Unused.Types (TermResults(trGitContext), GitContext(..), GitCommit(..), RemovalLikelihood(High), removalLikelihood, resultAliases)

newtype GitOutput = GitOutput { unOutput :: String }

gitContextForResults :: Int -> (String, TermResults) -> IO [(String, TermResults)]
gitContextForResults commitCount a@(token, results) =
    case removalLikelihood results of
        High -> do
            gitContext <- logToGitContext <$> gitLogSearchFor commitCount (resultAliases results)
            return [(token, results { trGitContext = Just gitContext })]
        _ -> return [a]

-- 58e219e Allow developer-authored configurations
-- 307dd20 Introduce internal yaml configuration of auto low likelihood match handling
-- 3b627ee Allow multiple matches with single-occurring appropriate tokens
-- f7a2e1a Add Hspec and tests around parsing
logToGitContext :: GitOutput -> GitContext
logToGitContext =
    GitContext . map GitCommit . shaList . unOutput
  where
    shaList = map (T.unpack . head . T.splitOn " " . T.pack) . lines

gitLogSearchFor :: Int -> [String] -> IO GitOutput
gitLogSearchFor commitCount ts = do
  let gitLogFlag = if length ts == 1 then "-S" else "-G"
  (_, results, _) <- P.readProcessWithExitCode "git" ["log", gitLogFlag, L.intercalate "|" ts, "--oneline", "-n", show commitCount] ""
  return $ GitOutput results
