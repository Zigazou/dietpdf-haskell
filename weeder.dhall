{ roots =
    [ "^Paths_.*" -- Ignore Paths_packageName module automatically generated
    , "^Main.main$" -- The main function is not called from the project
    , "^Util.Ascii.ascii.*$" -- Ascii module defines all ASCII chars
    ]
, type-class-roots = True
}
