let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ httpConfig =
    { getHost = "0.0.0.0"
    , getPort = 8083
    }
, executionConfigs =
  [ { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.C3t"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    , { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.GENSt"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    , { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.GEROt"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    , { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.WMTt"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    , { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.MELDt"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    , { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.cNETAt"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    , { maxOutputsPerTx    = 10
      , dripAsset          = "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404.HOSKYt"
      , dripAmount         = 1000000
      , dripAmountLovelace = 1500000
      , changeAddr         = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
      , delay              = 15000000
      , queueSize          = 4096
      }
    ]
, loggingConfig =
    { rootLogLevel   = LogLevel.Debug
    , fileHandlers   = [fileHandlers "./logs/faucet.log" LogLevel.Debug]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
, explorerConfig =
    { explorerUri = "https://explorer.spectrum.fi"
    }
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
    }
, nodeConfig =
    { nodeSocketPath = "/home/bromel/projects/cardano-node/ipc/node.socket"
    }
, walletConfig =
    { secretFile   = "/home/bromel/projects/cardano-dex-backend/wallet1TS.json"
    , keyPass      = "secret"
    , cardanoStyle = False
    }
, outputStoreConfig =
    { storePath       = "./data/funding_store"
    , createIfMissing = True
    }
, reCaptchaSecret = "secret"
, networkConfig = 
    { cardanoNetworkId = 2
    }
}