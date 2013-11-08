{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Description : All message type definitions.
module IHaskell.Types (
  Profile (..),
  Message (..),
  MessageHeader (..),
  MessageType(..),
  Username,
  Metadata,
  Port,
  replyType,
  ExecutionState (..),
  StreamType(..),
  MimeType(..),
  DisplayData(..),
  PayloadData(..),
  PayloadSource(..),
  ExecuteReplyStatus(..),
  ) where

import ClassyPrelude
import Data.Aeson
import IHaskell.Message.UUID


-- | A TCP port.
type Port = Int

-- | A kernel profile, specifying how the kernel communicates.
data Profile = Profile {
  ip :: String,        -- ^ The IP on which to listen.
  transport :: String, -- ^ The transport mechanism.
  stdinPort :: Port,   -- ^ The stdin channel port. 
  controlPort :: Port, -- ^ The control channel port.
  hbPort :: Port,      -- ^ The heartbeat channel port.
  shellPort :: Port,   -- ^ The shell command port.
  iopubPort :: Port,   -- ^ The Iopub port.
  key :: ByteString    -- ^ The HMAC encryption key.
  } deriving Show

-- Convert the kernel profile to and from JSON.
instance FromJSON Profile where
  parseJSON (Object v) = 
    Profile <$> v .: "ip"
            <*> v .: "transport"
            <*> v .: "stdin_port"
            <*> v .: "control_port"
            <*> v .: "hb_port"
            <*> v .: "shell_port"
            <*> v .: "iopub_port"
            <*> v .: "key"
instance ToJSON Profile where
  toJSON profile = object [
                    "ip"          .= ip profile,
                    "transport"   .= transport profile,
                    "stdin_port"  .= stdinPort profile,
                    "control_port".= controlPort profile,
                    "hb_port"     .= hbPort profile,
                    "shell_port"  .= shellPort profile,
                    "iopub_port"  .= iopubPort profile,
                    "key"         .= key profile
                   ]

-- | A message header with some metadata.  
data MessageHeader = MessageHeader {
  identifiers :: [ByteString],         -- ^ The identifiers sent with the message.
  parentHeader :: Maybe MessageHeader, -- ^ The parent header, if present.
  metadata :: Metadata,                -- ^ A dict of metadata.
  messageId :: UUID,                   -- ^ A unique message UUID.
  sessionId :: UUID,                   -- ^ A unique session UUID.
  username :: Username,                -- ^ The user who sent this message.
  msgType :: MessageType               -- ^ The message type.
  } deriving Show

-- Convert a message header into the JSON field for the header.
-- This field does not actually have all the record fields.
instance ToJSON MessageHeader where
  toJSON header = object [
                    "msg_id"  .= messageId header,
                    "session" .= sessionId header,
                    "username" .= username header,
                    "msg_type" .= show (msgType header)
                  ]

-- | A username for the source of a message.
type Username = ByteString

-- | A metadata dictionary.
type Metadata = Map ByteString ByteString

-- | The type of a message, corresponding to IPython message types.
data MessageType = KernelInfoReplyMessage
                 | KernelInfoRequestMessage
                 | ExecuteReplyMessage
                 | ExecuteRequestMessage
                 | StatusMessage
                 | StreamMessage
                 | DisplayDataMessage
                 | OutputMessage
                 | InputMessage
                 | CompleteRequestMessage
                 | CompleteReplyMessage
                 | ObjectInfoRequestMessage
                 | ObjectInfoReplyMessage

instance Show MessageType where
  show KernelInfoReplyMessage     = "kernel_info_reply"
  show KernelInfoRequestMessage   = "kernel_info_request"
  show ExecuteReplyMessage        = "execute_reply"
  show ExecuteRequestMessage      = "execute_request"
  show StatusMessage              = "status"
  show StreamMessage              = "stream"
  show DisplayDataMessage         = "display_data"
  show OutputMessage              = "pyout"
  show InputMessage               = "pyin"
  show CompleteRequestMessage     = "complete_request"
  show CompleteReplyMessage       = "complete_reply"
  show ObjectInfoRequestMessage   = "object_info_request"
  show ObjectInfoReplyMessage     = "object_info_reply"

instance FromJSON MessageType where
  parseJSON (String s) = case s of
    "kernel_info_reply"   -> return KernelInfoReplyMessage
    "kernel_info_request" -> return KernelInfoRequestMessage
    "execute_reply"       -> return ExecuteReplyMessage
    "execute_request"     -> return ExecuteRequestMessage
    "status"              -> return StatusMessage
    "stream"              -> return StreamMessage
    "display_data"        -> return DisplayDataMessage
    "pyout"               -> return OutputMessage
    "pyin"                -> return InputMessage
    "complete_request"    -> return CompleteRequestMessage
    "complete_reply"      -> return CompleteReplyMessage
    "object_info_request" -> return ObjectInfoRequestMessage
    "object_info_reply"   -> return ObjectInfoReplyMessage
    _                     -> fail ("Unknown message type: " ++ show s)
  parseJSON _ = fail "Must be a string."


-- | A message used to communicate with the IPython frontend.
data Message 
  -- | A request from a frontend for information about the kernel.
  = KernelInfoRequest { header :: MessageHeader }
  -- | A response to a KernelInfoRequest.
  | KernelInfoReply { header :: MessageHeader }
               
  -- | A request from a frontend to execute some code.
  | ExecuteRequest {
      header :: MessageHeader,
      getCode :: ByteString,             -- ^ The code string.
      getSilent :: Bool,                 -- ^ Whether this should be silently executed.
      getStoreHistory :: Bool,           -- ^ Whether to store this in history.
      getAllowStdin :: Bool,             -- ^ Whether this code can use stdin.

      getUserVariables :: [ByteString],  -- ^ Unused.
      getUserExpressions :: [ByteString] -- ^ Unused.
    }

  -- | A reply to an execute request.
  | ExecuteReply {
      header :: MessageHeader,
      status :: ExecuteReplyStatus,         -- ^ The status of the output.
      payload :: [PayloadData],              -- ^ The payload for extra information.
      executionCounter :: Int               -- ^ The execution count, i.e. which output this is.
    }

  | PublishStatus {
      header :: MessageHeader,
      executionState :: ExecutionState      -- ^ The execution state of the kernel.
    }

  | PublishStream {
      header :: MessageHeader,
      streamType :: StreamType,             -- ^ Which stream to publish to.
      streamContent :: String               -- ^ What to publish.
    }

  | PublishDisplayData {
      header :: MessageHeader,
      source :: String,                     -- ^ The name of the data source.
      displayData :: [DisplayData]          -- ^ A list of data representations.
    }

  | PublishOutput {
      header :: MessageHeader,
      reprText :: String,                   -- ^ Printed output text.
      executionCount :: Int                 -- ^ Which output this is for.
    }

  | PublishInput {
      header :: MessageHeader,
      inCode :: String,                     -- ^ Submitted input code.
      executionCount :: Int                 -- ^ Which input this is.
    }

  | CompleteRequest {
      header :: MessageHeader,
      getCode :: ByteString, {- ^
            The entire block of text where the line is.  This may be useful in the
            case of multiline completions where more context may be needed.  Note: if
            in practice this field proves unnecessary, remove it to lighten the
            messages. json field @block@  -}
      getCodeLine :: ByteString, -- ^ just the line with the cursor. json field @line@
      getCursorPos :: Int -- ^ position of the cursor (index into the line?). json field @cursor_pos@

    }

  | CompleteReply {
     header :: MessageHeader,
     completionMatches :: [ByteString],
     completionMatchedText :: ByteString,
     completionText :: ByteString,
     completionStatus :: Bool
  }
      {- ^
# The list of all matches to the completion request, such as
# ['a.isalnum', 'a.isalpha'] for the above example.
'matches' : list,

# the substring of the matched text
# this is typically the common prefix of the matches,
# and the text that is already in the block that would be replaced by the full completion.
# This would be 'a.is' in the above example.
'text' : str,

# status should be 'ok' unless an exception was raised during the request,
# in which case it should be 'error', along with the usual error message content
# in other messages.
'status' : 'ok'
} -}
  | ObjectInfoRequest {
      header :: MessageHeader,
      objectName :: ByteString, -- ^ name of object to be searched for
      detailLevel :: Int       -- ^ level of detail desired. default (0) 
                                --  is equivalent to typing foo?, (1) is foo?? (don't know yet what this means for haskell)
    }
  | ObjectInfoReply {
      header :: MessageHeader, 
      objectName :: ByteString, 
      objectFound :: Bool, -- ^ was the object found? 
      objectTypeString :: ByteString, -- ^ type info string
      objectDocString  :: ByteString
    }

    deriving Show

-- | Possible statuses in the execution reply messages.
data ExecuteReplyStatus = Ok | Err | Abort

instance Show ExecuteReplyStatus where
  show Ok = "ok"
  show Err = "error"
  show Abort = "abort"

-- | The execution state of the kernel.
data ExecutionState = Busy | Idle | Starting deriving Show

-- | Data for display: a string with associated MIME type.
data DisplayData = Display MimeType String deriving Show

-- | Possible MIME types for the display data.
data MimeType = PlainText | MimeHtml deriving Eq

instance Show MimeType where
  show PlainText = "text/plain"
  show MimeHtml  = "text/html"

-- | Payload dict. Used to send data to e.g. the pager in the notebook. The payloadData
-- | type must implement a ToJSON instance.
data PayloadData = forall a. ToJSON a => PayloadData {payloadSource :: PayloadSource, payloadData :: a }

instance Show PayloadData where
  show (PayloadData src dta) = "Source: " ++ show src ++ ", " ++ (show . toJSON $ dta)

-- | The types of sources for the payload. Currently only "pager is a source."
data PayloadSource = Pager

instance Show PayloadSource where
  show Pager = "pager"


-- | Input and output streams.
data StreamType = Stdin | Stdout deriving Show

-- | Get the reply message type for a request message type.
replyType :: MessageType -> MessageType
replyType KernelInfoRequestMessage = KernelInfoReplyMessage
replyType ExecuteRequestMessage = ExecuteReplyMessage
replyType CompleteRequestMessage = CompleteReplyMessage
replyType ObjectInfoRequestMessage = ObjectInfoReplyMessage
replyType messageType = error $ "No reply for message type " ++ show messageType
