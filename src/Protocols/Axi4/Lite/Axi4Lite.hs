{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Axi4.Lite.Axi4Lite where

import Protocols
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


---------------------------
--- Write address types ---
---------------------------

data M2S_WriteAddress
  (aw :: AddrWidth)
  = M2S_NoWriteAddress
  | M2S_WriteAddress {
    -- _awvalid is deduced from the fact that this is not NoWriteAddress
    _awaddr :: !(C.BitVector (Width aw)),
    _awprot :: PermissionsType 'KeepPermissions
  } deriving (Generic, NFDataX)

deriving instance (C.KnownNat (Width aw))
  => Show (M2S_WriteAddress aw)

data S2M_WriteAddress
  = S2M_WriteAddress {
    _awready :: Bool
  } deriving (Show, Generic, NFDataX)

data Axi4LiteWA
  (dom :: C.Domain)
  (aw :: AddrWidth)

instance Protocol (Axi4LiteWA dom aw) where
  type Fwd (Axi4LiteWA dom aw) = C.Signal dom (M2S_WriteAddress aw)
  type Bwd (Axi4LiteWA dom aw) = C.Signal dom (S2M_WriteAddress)

------------------------
--- Write data types ---
------------------------

data M2S_WriteData
  (bw :: BusWidth)
  = M2S_NoWriteData
  | M2S_WriteData {
    -- In AXI4 Lite, strobing is mandatory for masters and interconnects
    _wdata :: !(WriteBusWidthType bw)
  } deriving (Generic)

deriving instance
  (C.KnownNat (Width bw)
  , Show (WriteBusWidthType bw))
  => Show (M2S_WriteData bw)

deriving instance (NFDataX (WriteBusWidthType bw)) => NFDataX (M2S_WriteData bw)

data S2M_WriteData
  = S2M_WriteData {
    _wready :: Bool
  } deriving (Show, Generic, NFDataX)

data Axi4LiteWD
  (dom :: C.Domain)
  (bw :: BusWidth)

instance Protocol (Axi4LiteWD dom bw) where
  type Fwd (Axi4LiteWD dom bw) = C.Signal dom (M2S_WriteData bw)
  type Bwd (Axi4LiteWD dom bw) = C.Signal dom (S2M_WriteData)

----------------------------
--- Write response types ---
----------------------------

data M2S_WriteResponse
  = M2S_WriteResponse {
    _bready :: Bool
  } deriving (Show, Generic, NFDataX)

data S2M_WriteResponse
  = S2M_NoWriteResponse
  | S2M_WriteResponse {
    _bresp :: RespLite
  } deriving (Show, Generic, NFDataX)

data Axi4LiteWR
  (dom :: C.Domain)

instance Protocol (Axi4LiteWR dom) where
  type Fwd (Axi4LiteWR dom) = C.Signal dom (M2S_WriteResponse)
  type Bwd (Axi4LiteWR dom) = C.Signal dom (S2M_WriteResponse)

--------------------------
--- Read address types ---
--------------------------

data M2S_ReadAddress
  (aw :: AddrWidth)
  = M2S_NoReadAddress
  | M2S_ReadAddress {
    _araddr :: !(C.BitVector (Width aw)),
    _arprot :: PermissionsType 'KeepPermissions
  } deriving (Generic, NFDataX)

deriving instance
  (C.KnownNat (Width aw))
  => Show (M2S_ReadAddress aw)


data S2M_ReadAddress
  = S2M_ReadAddress {
    _arready :: Bool
  } deriving (Show, Generic, NFDataX)

data Axi4LiteRA
  (dom :: C.Domain)
  (aw :: AddrWidth)

instance Protocol (Axi4LiteRA dom aw) where
  type Fwd (Axi4LiteRA dom aw) = C.Signal dom (M2S_ReadAddress aw)
  type Bwd (Axi4LiteRA dom aw) = C.Signal dom (S2M_ReadAddress)

-----------------------
--- Read data types ---
-----------------------

data M2S_ReadData
  (bw :: BusWidth) -- Necessary for the injectivity requirement of Fwd
  = M2S_ReadData {
    _rready :: Bool
  } deriving (Generic, NFDataX)

deriving instance (Show (ReadBusWidthType bw)) => Show (M2S_ReadData bw)

data S2M_ReadData
  (bw :: BusWidth)
  = S2M_NoReadData
  | S2M_ReadData {
    _rdata :: ReadBusWidthType bw,
    _rresp :: RespLite
  } deriving (Generic)

deriving instance (Show (ReadBusWidthType bw)) => Show (S2M_ReadData bw)
deriving instance (NFDataX (ReadBusWidthType bw)) => NFDataX (S2M_ReadData bw)

data Axi4LiteRD
  (dom :: C.Domain)
  (bw :: BusWidth)

instance Protocol (Axi4LiteRD dom bw) where
  type Fwd (Axi4LiteRD dom bw) = C.Signal dom (M2S_ReadData bw)
  type Bwd (Axi4LiteRD dom bw) = C.Signal dom (S2M_ReadData bw)


-- Just the write part of the AXI4 Lite
type Axi4LiteWrite
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = (Axi4LiteWA dom aw, Axi4LiteWD dom bw, Axi4LiteWR dom)

-- Just the read part of AXI4 Lite
type Axi4LiteRead
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = (Axi4LiteRA dom aw, Axi4LiteRD dom bw)

-- Full AXI4 Lite protocol (both read and write channel sets)
type Axi4Lite
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = (Axi4LiteWrite dom aw bw, Axi4LiteRead dom aw bw)
