{-# LANGUAGE CPP #-}

module Main (main) where

import Codec.Compression.GZip (compress)
import Control.Carrier.Lift (runM)
import Control.Effect.Pretty (PrettyC, runPretty)
import Control.Monad.Codensity (Codensity (Codensity))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Coerce
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.SARIF as Sarif
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Audit (AuditConfig (..), buildAdvisories)
import Distribution.Client.NixStyleOptions (defaultNixStyleFlags)
import Distribution.Package (PackageName, unPackageName)
import qualified Distribution.Verbosity as Verbosity
import GitHub.REST
import Options.Applicative
import Security.Advisories
import Security.Advisories.Cabal
import Security.Advisories.SBom.Types (prettyVersion)
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (readProcess)

main :: IO ()
main = do
  (auditConfig, ghContext) <- customExecParser (prefs showHelpOnEmpty) $ do
    info (helper <*> ((,) <$> cliAuditParser <*> cliGithubContextParser)) $ do
      mconcat
        [ fullDesc,
          progDesc "audit your cabal projects for vulnerabilities and upload the result to GitHub",
          header "Welcome to github-action-scan"
        ]
  getAdvisories auditConfig >>= sendAdvisories ghContext

data GitHubContext = GitHubContext
  { token :: ByteString,
    ownerRepository :: Text,
    ref :: Text,
    commitSha :: Text
  }

cliAuditParser :: Parser AuditConfig
cliAuditParser =
  MkAuditConfig
    <$> ( ( Left
              <$> strOption
                ( mconcat
                    [ long "advisories-file-path",
                      metavar "FILEPATH",
                      help "the path to the repository containing an advisories directory"
                    ]
                )
          )
            <|> ( Right
                    <$> strOption
                      ( mconcat
                          [ long "advisories-repository",
                            metavar "REPOSITORY",
                            help "the url to the repository containing an advisories directory",
                            value "https://github.com/haskell/security-advisories"
                          ]
                      )
                )
        )
    <*> pure Verbosity.normal
    <*> pure (error "outputFormat cannot be constructed, and it was working, it was not used")
    <*> pure (Codensity $ \k -> k stdout)
    <*> pure False
    <*> pure False

cliGithubContextParser :: Parser GitHubContext
cliGithubContextParser =
  GitHubContext
    <$> byteStringOption
      ( long "token"
          <> metavar "TOKEN"
          <> help "GitHub API token"
      )
    <*> textOption
      ( long "owner-repository"
          <> metavar "REPOSITORY"
          <> help "Repository name"
      )
    <*> textOption
      ( long "ref"
          <> metavar "REF"
          <> help "Git reference"
      )
    <*> textOption
      ( long "commit-sha"
          <> metavar "COMMIT_SHA"
          <> help "Commit SHA"
      )
  where
    byteStringOption mod' = B.pack <$> strOption mod'
    textOption mod' = T.pack <$> strOption mod'

getAdvisories :: AuditConfig -> IO [(PackageName, ElaboratedPackageInfoAdvised)]
getAdvisories auditConfig = do
  let nixStyleFlags = defaultNixStyleFlags ()
      interpretPretty :: forall m a. PrettyC [Text] m a -> m a
      interpretPretty = runPretty (const id)

  runM $ interpretPretty $ Map.toList <$> buildAdvisories auditConfig nixStyleFlags

sendAdvisories :: GitHubContext -> [(PackageName, ElaboratedPackageInfoAdvised)] -> IO ()
sendAdvisories ghContext packageAdvisories = do
  ghcVersion <- T.pack <$> readProcess "ghc" ["--version"] ""
  let advisories =
        Map.elems $
          Map.fromListWith (\(advisory, pkgsInfo) (_, pkgsInfo') -> (advisory, pkgsInfo <> pkgsInfo')) $
            flip concatMap packageAdvisories $ \(pkgName, pkgInfo) ->
              flip map (runIdentity pkgInfo.packageAdvisories) $ \(advisory, fixedAt) ->
                (advisory.advisoryId, (advisory, [(pkgName, fixedAt)]))
      run =
        MkRun
          { runTool =
              let tool name version =
                    defaultToolComponent
                      { toolComponentName = Just name,
                        toolComponentVersion = Just version
                      }
               in MkTool
                    { toolExtensions =
                        [ tool "cabal-audit" VERSION_cabal_audit,
                          tool "hsec-tools" VERSION_hsec_tools,
                          tool "ghc" ghcVersion
                        ],
                      toolDriver = tool "github-action-scan" VERSION_github_action_scan
                    },
            runResults =
              flip map advisories $ \(advisory, concernedInfo) ->
                MkResult
                  { resultRuleId = T.pack $ printHsecId advisory.advisoryId,
                    resultMessage =
                      defaultMultiformatMessageString $
                        let hsecId = T.pack (printHsecId advisory.advisoryId)
                         in T.intercalate "\n" $
                              concat
                                [ [hsecId <> " \"" <> advisory.advisorySummary <> "\""],
                                  ["published: " <> T.pack (show advisory.advisoryPublished)],
                                  ["https://haskell.github.io/security-advisories/advisory/" <> hsecId],
                                  ["Concerned package:"],
                                  flip map concernedInfo $ \(pkgName, fixedAt) ->
                                    "* "
                                      <> T.pack (unPackageName pkgName)
                                      <> ": "
                                      <> case fixedAt of
                                        Nothing -> "No fix version available"
                                        Just fv -> "Fix available since version " <> prettyVersion fv,
                                  [T.intercalate ", " (coerce advisory.advisoryKeywords)]
                                ],
                    resultLocations = mempty, -- TODO cabal files/lock?
                    resultLevel = Just Sarif.Error
                  },
            runArtifacts = mempty -- TODO cabal files/lock?
          }

  let ghSettings =
        GitHubSettings
          { token = Just $ BearerToken ghContext.token,
            userAgent = "github-action-scan",
            apiVersion = "2022-11-28"
          }

  runGitHubT ghSettings $ do
    -- https://docs.github.com/en/rest/code-scanning/code-scanning?apiVersion=2022-11-28#upload-an-analysis-as-sarif-data
    sarif <-
      queryGitHub
        GHEndpoint
          { method = POST,
            endpoint = "/repos/:repo/code-scanning/sarifs",
            endpointVals =
              [ "repo" := ghContext.ownerRepository
              ],
            ghData =
              [ "ref" := ghContext.ref,
                "commit_sha" := ghContext.commitSha,
                "sharif" := extractBase64 (encodeBase64 $ compress $ encodeSarifAsLBS defaultLog {logRuns = [run]}),
                "tool_name" := ("github-action-scan" :: Text),
                "validate" := True
              ]
          }
    let kv = "sarif-id=" <> (sarif .: "id")
    -- log to stderr
    liftIO $ hPutStrLn stderr $ "Setting: " ++ kv
    -- output to stdout into $GITHUB_OUTPUT
    liftIO $ putStrLn kv
