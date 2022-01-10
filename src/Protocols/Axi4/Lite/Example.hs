{-# LANGUAGE RecordWildCards #-}

module Protocols.Axi4.Lite.Example where

import Clash.Prelude hiding (zip, undefined)
import Prelude hiding ((!!))
import qualified Prelude as P
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite


type BasicAddrWidth = BitVector (Width ('AddrWidth 4))

data BasicAxiMaster
  = BM_Read BasicAddrWidth
  | BM_Write BasicAddrWidth (WriteBusWidthType 'Width32)
  | BM_NoData
  deriving (Show, Generic, NFDataX)

data BasicAxiSlave
  = BS_Read (ReadBusWidthType 'Width32)
  | BS_Idle
  | BS_Busy
  deriving (Show, Generic, NFDataX)

data BasicAxi (dom :: Domain)

instance Protocol (BasicAxi dom) where
  type Fwd (BasicAxi dom) = Signal dom BasicAxiMaster
  type Bwd (BasicAxi dom) = Signal dom BasicAxiSlave

type AxiToBasic dom = Circuit
  (Axi4Lite dom ('AddrWidth 4) 'Width32)
  (BasicAxi dom)


masterTestSigs :: [(M2S_ReadAddress ('AddrWidth 4), M2S_ReadData 'Width32)]
masterTestSigs = zip ra rd
  where
    ra =
      [ M2S_NoReadAddress
      , m2s_ra 3
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , m2s_ra 4
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress]
    rd =
      [ nack, nack, nack, nack, nack, nack, ack, nack, nack, nack, nack, nack, nack, ack ] P.++ P.repeat nack

    m2s_ra addr = M2S_ReadAddress { _araddr = addr, _arprot = (NotPrivileged, NonSecure, Data)}
    ack = M2S_ReadData { _rready = True }
    nack = M2S_ReadData { _rready = False }

sim :: [(M2S_ReadAddress ('AddrWidth 4), M2S_ReadData 'Width32)]
  -> [(S2M_ReadAddress, S2M_ReadData 'Width32)]
sim = simulate @System (bundle . top' . unbundle)
  where
    top :: HiddenClockResetEnable dom =>
      ((Signal dom (M2S_ReadAddress ('AddrWidth 4)),
       Signal dom (M2S_ReadData 'Width32)),
      ())
     -> ((Signal dom S2M_ReadAddress,
          Signal dom (S2M_ReadData 'Width32)),
         ())
    top = toSignals $ convertRead |> basicAxiMem
    -- top' :: forall dom . HiddenClockResetEnable dom =>
    --   (Signal dom (M2S_ReadAddress ('AddrWidth 4)), Signal dom (M2S_ReadData 'Width32)) ->
    --   (Signal dom S2M_ReadAddress, Signal dom (S2M_ReadData 'Width32))
    top' sigs = case top (sigs, ()) of
      (sigs', ()) -> sigs'


-- converter :: AxiToBasic dom
-- converter = Circuit go

-- convertWrite :: Circuit (Axi4LiteWrite dom ('AddrWidth 4) 'Width32) (BasicAxi dom)
-- convertWrite = Circuit go

convertRead :: HiddenClockResetEnable dom =>
  Circuit (Axi4LiteRead dom ('AddrWidth 4) 'Width32) (BasicAxi dom)
convertRead = Circuit go
  where
    go ((ra_data, rd_ack), basicSlave) = ((ra_ack, rd_data), basicMaster)
      where
        (ra_ack, rd_data) = unbundle ra_rd
        (ra_rd, basicMaster) = unbundle $ (machine)
          (bundle (bundle (ra_data, rd_ack), basicSlave))

        machine = mealy convertReadMealy CR_Idle

convertRA :: M2S_ReadAddress ('AddrWidth 4) -> BasicAxiMaster
convertRA M2S_NoReadAddress = BM_NoData
convertRA M2S_ReadAddress {..} = BM_Read _araddr

convertRD :: BasicAxiSlave -> S2M_ReadData 'Width32
convertRD basicSlave = case basicSlave of
  BS_Read d -> S2M_ReadData {
    _rdata = d,
    _rresp = RLOkay
  }
  _ -> S2M_NoReadData

type ConvertReadInput = ((M2S_ReadAddress ('AddrWidth 4), M2S_ReadData 'Width32), BasicAxiSlave)
type ConvertReadOutput = ((S2M_ReadAddress, S2M_ReadData 'Width32), BasicAxiMaster)

data ConvertReadState = CR_Idle | CR_WaitForReady BasicAxiSlave
  deriving (Show, Generic, NFDataX)

convertReadMealy :: ConvertReadState -> ConvertReadInput -> (ConvertReadState, ConvertReadOutput)
convertReadMealy (keepData) ((ra_data, rd_ack), basicSlave) = (keepData', ((ra_ack, rd_data), basicMaster))
  where
    rd_data = case keepData of
      CR_WaitForReady d -> convertRD d
      _ -> convertRD basicSlave
    ra_ack = s2m_ra basicSlave
    basicMaster = convertRA ra_data
    keepData' = case keepData of
      CR_Idle -> case basicSlave of
        BS_Read _ -> CR_WaitForReady basicSlave
        _ -> CR_Idle
      CR_WaitForReady _ -> if masterReady
        then CR_Idle
        else keepData

    masterReady = _rready rd_ack

    s2m_ra slaveCmd = case slaveCmd of
      BS_Idle -> S2M_ReadAddress { _arready = True }
      _ -> S2M_ReadAddress { _arready = False }

basicAxiMem :: HiddenClockResetEnable dom =>
  Circuit (BasicAxi dom) ()
basicAxiMem = Circuit go
  where
    go (master, ()) = (memory master, ())

memory :: HiddenClockResetEnable dom =>
  Signal dom BasicAxiMaster -> Signal dom BasicAxiSlave
memory = mealy memoryMealy emptyMemoryState

data MemState = MemState {
    counter :: Unsigned 2,
    values :: Vec 16 (BitVector 8),
    last_command :: Maybe BasicAxiMaster
  } deriving (Show, NFDataX, Generic)

emptyMemoryState :: MemState
emptyMemoryState = MemState {
    counter = 0,
    values = 0:>1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>13:>14:>15:>Nil,
    last_command = Nothing
  }

memoryMealy :: MemState -> BasicAxiMaster -> (MemState, BasicAxiSlave)
memoryMealy state command = (state', result)
  where
    MemState {..} = state
    state' = state {
      counter = counter + 1,
      last_command = case last_command of
        Nothing -> case command of
          BM_NoData -> Nothing
          _ -> Just command
        Just _ -> if counter == 0
          then case command of
            BM_NoData -> Nothing
            _ -> Just command
          else last_command
    }

    result = if counter == 0
      then case last_command of
        Just cmd -> execute cmd
        Nothing -> BS_Idle
      else case last_command of
        Just _ -> BS_Busy
        Nothing -> BS_Idle

    execute cmd = case cmd of
      BM_Read addr -> BS_Read (0:>0:>0:>(values !! addr):>Nil)
      _ -> BS_Idle
