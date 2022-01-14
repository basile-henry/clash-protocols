{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Defines datatypes for all five channels of the AXI4 Lite protocol. For more
information on AXI4 Lite, see chapter B of the AMBA AXI specification.
-}

module Protocols.Axi4.Lite.Axi4Lite where

import Protocols
import Protocols.Axi4.Common
import Clash.Prelude as C


-- | AXI4 Lite busses are always either 32 bit or 64 bit.
data BusWidth = Width32 | Width64 deriving (Show, Eq)

-- | AXI4 Lite defines a strobe signal to signify which bytes of the input
-- signal should be committed to memory. The strobe signal is encoded in
-- the 'Maybe' data type. Strobing is mandatory in AXI4 Lite.
type family WriteBusWidthType (bw :: BusWidth) where
  WriteBusWidthType 'Width32 = C.Vec 4 (Maybe (C.BitVector 8))
  WriteBusWidthType 'Width64 = C.Vec 8 (Maybe (C.BitVector 8))

type family ReadBusWidthType (bw :: BusWidth) where
  ReadBusWidthType 'Width32 = C.Vec 4 (C.BitVector 8)
  ReadBusWidthType 'Width64 = C.Vec 8 (C.BitVector 8)


---------------------------
--- Write address types ---
---------------------------


-- | The xvalid signals in AXI4 Lite are encoded in the datatype by having two
-- options, e.g. M2S_NoWriteAddress and M2S_WriteAddress. The rest of the channels
-- are fields in the record. Table B1.1 defines which signals AXI4 Lite uses.
data M2S_WriteAddress
  (aw :: AddrWidth)
  = M2S_NoWriteAddress
  | M2S_WriteAddress {
    -- | Address to be written to
    _awaddr :: !(C.BitVector (Width aw)),

    -- | Protection permissions, in AXI4 Lite these are always enabled.
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

-- | Protocol instance for the write address channel.
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
    -- | Write data
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

-- | Protocol instance for the write data channel.
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

-- | Protocol instance for the write response channel.
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

-- | Protocol instance for the read address channel.
instance Protocol (Axi4LiteRA dom aw) where
  type Fwd (Axi4LiteRA dom aw) = C.Signal dom (M2S_ReadAddress aw)
  type Bwd (Axi4LiteRA dom aw) = C.Signal dom (S2M_ReadAddress)

-----------------------
--- Read data types ---
-----------------------

-- | Acknowledges data from the slave component. This data type needs the 'bw' type
-- to fullfil the injectivity requirement of 'Fwd' in 'Protocol'.
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

-- | Protocol instance for the read data channel. Notice that in this protocol
-- data flows over the backward channel, but due to type injectivity the forward
-- channel needs to contain the 'bw' type as well.
instance Protocol (Axi4LiteRD dom bw) where
  type Fwd (Axi4LiteRD dom bw) = C.Signal dom (M2S_ReadData bw)
  type Bwd (Axi4LiteRD dom bw) = C.Signal dom (S2M_ReadData bw)


-- | Protocols for writing to an AXI4 Lite component.
type Axi4LiteWrite
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = (Axi4LiteWA dom aw, Axi4LiteWD dom bw, Axi4LiteWR dom)

-- | Protocols for reading from an AXI4 Lite component.
type Axi4LiteRead
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = (Axi4LiteRA dom aw, Axi4LiteRD dom bw)

-- | Full AXI4 Lite protocol with both read and write channel sets.
type Axi4Lite
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = (Axi4LiteWrite dom aw bw, Axi4LiteRead dom aw bw)
