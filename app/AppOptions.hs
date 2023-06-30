{-# LANGUAGE StrictData #-}
module AppOptions
  ( AppOptions(OptimizeOptions, InfoOptions, ExtractOptions)
  , appOptions
  ) where

import           Options.Applicative            ( Parser
                                                , argument
                                                , command
                                                , help
                                                , info
                                                , long
                                                , metavar
                                                , progDesc
                                                , short
                                                , str
                                                , auto
                                                , subparser
                                                , switch
                                                )

data AppOptions
  = OptimizeOptions Bool FilePath FilePath
  | InfoOptions FilePath
  | ExtractOptions Int FilePath

appOptions :: Parser AppOptions
appOptions = subparser
  (  command
      "info"
      (info
        (   InfoOptions
        <$> argument str (metavar "IN" <> help "PDF file to analyze")
        )
        (progDesc "Print information about a PDF file")
      )
  <> command
       "extract"
       (info
         (   ExtractOptions
         <$> argument auto (metavar "NUM" <> help "Object number")
         <*> argument str  (metavar "IN" <> help "PDF file to analyze")
         )
         (progDesc
           "Extract the stream of a specific object from a PDF file \
           \(the stream is unfiltered)"
         )
       )
  <> command
       "optimize"
       (info
         (   OptimizeOptions
         <$> switch (short 'v' <> long "verbose" <> help "Verbose output")
         <*> argument str (metavar "IN" <> help "PDF file to process")
         <*> argument str (metavar "OUT" <> help "PDF file to create")
         )
         (progDesc "Optimize a PDF file")
       )
  )
