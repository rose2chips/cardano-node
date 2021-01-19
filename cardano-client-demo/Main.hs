module Main where

import Cardano.Api
import qualified Cardano.Api.IPC as IPC
import qualified Cardano.Chain.Slotting as Byron (EpochSlots(..))

-- TODO: Export this via Cardano.Api
import Ouroboros.Network.Protocol.ChainSync.Client

import Control.Monad (when)
import System.Environment (getArgs)
import System.FilePath ((</>))

-- | Connects to a local cardano node, requests the blocks and prints out the
-- number of transactions. To run this, you must first start a local node e.g.:
--
--     $ cabal run cardano-node:cardano-node -- run \
--        --config        configuration/cardano/mainnet-config.json \
--        --topology      configuration/cardano/mainnet-topology.json \
--        --database-path db \
--        --socket-path   db/node.sock \
--        --host-addr     127.0.0.1 \
--        --port          3001 \
--
-- Then run this with the path to the directory containing node.sock:
--
--     $ cabal run cardano-client-demo -- db
--
main :: IO ()
main = do
  -- Get cocket path from CLI argument.
  socketDir:_ <- getArgs
  let socketPath = socketDir </> "node.sock"

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  IPC.connectToLocalNode
    (connectInfo socketPath)
    protocols
  where
  connectInfo :: FilePath -> IPC.LocalNodeConnectInfo IPC.CardanoMode
  connectInfo socketPath =
      IPC.LocalNodeConnectInfo {
        IPC.localConsensusModeParams = IPC.CardanoModeParams (Byron.EpochSlots 21600),
        IPC.localNodeNetworkId       = Mainnet,
        IPC.localNodeSocketPath      = socketPath
      }

  protocols :: IPC.LocalNodeClientProtocolsInMode IPC.CardanoMode
  protocols =
      IPC.LocalNodeClientProtocols {
        IPC.localChainSyncClient    = Just chainSyncClient,
        IPC.localTxSubmissionClient = Nothing,
        IPC.localStateQueryClient   = Nothing
      }

-- | Defines the client side of the chain sync protocol.
chainSyncClient :: ChainSyncClient
                     (IPC.BlockInMode IPC.CardanoMode)
                     IPC.ChainPoint
                     IPC.ChainTip
                     IO ()
chainSyncClient = ChainSyncClient clientStIdle
  where
    clientStIdle :: IO (ClientStIdle (IPC.BlockInMode IPC.CardanoMode)
                                 IPC.ChainPoint IPC.ChainTip IO ())
    clientStIdle = do
      putStrLn "Chain Sync: requesting next"
      return $ SendMsgRequestNext
        -- There's more to get immediately
        clientStNext

        -- The node is asking us to wait. This is because we reached the
        -- tip. We can certainly carry on here, but for this demo we are
        -- going to stop when we hit the current chain tip.
        clientDone

    clientStNext :: ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                 IPC.ChainPoint IPC.ChainTip IO ()
    clientStNext =
      ClientStNext {
          recvMsgRollForward = \(IPC.BlockInMode (IPC.Block _header transactions) _) _tip ->
            ChainSyncClient $ do
              putStrLn $ "Block # of Transactions:  " ++ show (length transactions)
              clientStIdle

        , recvMsgRollBackward = \_ _ -> ChainSyncClient clientStIdle
        }

    -- We're still in the "Next" state here, but we've decided to stop
    -- as soon as we get the reply, no matter which reply.
    clientDone :: IO (ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                 IPC.ChainPoint IPC.ChainTip IO ())
    clientDone = do
      putStrLn "Chain Sync: done!"
      return $ ClientStNext {
        recvMsgRollForward  = \_ _ -> ChainSyncClient (pure (SendMsgDone ())),
        recvMsgRollBackward = \_ _ -> ChainSyncClient (pure (SendMsgDone ()))
      }
