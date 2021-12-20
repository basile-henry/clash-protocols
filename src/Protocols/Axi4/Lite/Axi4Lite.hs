module Protocols.Axi4.Lite.Axi4Lite where

import Protocols
import Protocols.DfLike
import Protocols.Axi4.Common
import Clash.Prelude as C

import Data.Tuple.Strict (T3)

data BusWidth = Width32 | Width64 deriving (Show, Eq)

type family WriteBusWidthType (bw :: BusWidth) where
    -- The strobe signal is encoded in maybes
  WriteBusWidthType 'Width32 = C.Vec 4 (Maybe (C.BitVector 8))
  WriteBusWidthType 'Width64 = C.Vec 8 (Maybe (C.BitVector 8))

type family ReadBusWidthType (bw :: BusWidth) where
  ReadBusWidthType 'Width32 = C.Vec 4 (C.BitVector 8)
  ReadBusWidthType 'Width64 = C.Vec 8 (C.BitVector 8)

-- | AXI4 Lite protocol
data Axi4Lite
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)



instance Protocol (Axi4Lite dom aw bw) where
  type Fwd (Axi4Lite dom aw bw) = C.Signal dom (M2S_Axi4Lite aw bw)
  type Bwd (Axi4Lite dom aw bw) = C.Signal dom (S2M_Axi4Lite aw bw)


-- instance Backpressure (Axi4Lite dom aw bw) where
--   boolsToBwd _ = C.fromList_lazy . coerce

-- instance DfLike
-- Hoe Axi4Lite nu is is het geen dflike, want dflike wil volgens mij dat op de backward er altijd alleen maar ackjes gaan en geen data

-----------------------------
--- Master to slave wires ---
-----------------------------

data M2S_Axi4Lite
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = M2S_Axi4Lite {
    _write_address :: M2S_WriteAddress aw,
    _write_data :: M2S_WriteData bw,
    _write_response_ack :: M2S_WriteResponse,
    _read_address :: M2S_ReadAddress aw,
    _read_data_ack :: M2S_ReadData
  }

data M2S_WriteAddress
  (aw :: AddrWidth)
  = M2S_NoWriteAddress
  | M2S_WriteAddress {
    -- _awvalid is deduced from the fact that this is not NoWriteAddress
    _awaddr :: !(C.BitVector (Width aw)),
    _awprot :: T3 Privileged Secure InstructionOrData
  }

data M2S_WriteData
  (bw :: BusWidth)
  = M2S_NoWriteData
  | M2S_WriteData {
    -- In AXI4 Lite, strobing is mandatory for masters and interconnects
    _wdata :: !(WriteBusWidthType bw)
  }

data M2S_WriteResponse
  = M2S_WriteResponse {
    _bready :: Bool
  }

data M2S_ReadAddress
  (aw :: AddrWidth)
  = M2S_NoReadAddress
  | M2S_ReadAddress {
    _araddr :: !(C.BitVector (Width aw)),
    _arprot :: T3 Privileged Secure InstructionOrData
  }

data M2S_ReadData
  = M2S_ReadData {
    _rready :: Bool
  }

-----------------------------
--- Slave to master wires ---
-----------------------------

data S2M_Axi4Lite
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = S2M_Axi4Lite {
    _write_address_ack :: S2M_WriteAddress,
    _write_data_ack :: S2M_WriteData,
    _write_response :: S2M_WriteResponse,
    _read_address_ack :: S2M_ReadAddress,
    _read_data :: S2M_ReadData bw
  }

data S2M_WriteAddress
  = S2M_WriteAddress {
    _awready :: Bool
  }

data S2M_WriteData
  = S2M_WriteData {
    _wready :: Bool
  }

data S2M_WriteResponse
  = S2M_NoWriteResponse
  | S2M_WriteResponse {
    _bresp :: RespLite
  }

data S2M_ReadAddress
  = S2M_ReadAddress {
    _arready :: Bool
  }

data S2M_ReadData
  (bw :: BusWidth)
  = S2M_ReadData {
    _rdata :: ReadBusWidthType bw,
    _rresp :: RespLite
  }
