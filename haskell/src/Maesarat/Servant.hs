-- | Provides the 'HTML' content type for servant.
--
-- @since 0.1.0.0
module Maesarat.Servant
  ( HTML,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Conversions (UTF8 (..))
import Data.Text.Conversions qualified as TConv
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeUnrender (..))

-- | Servant content type for HTML. This is intended for receiving @text/html@
-- and then converting to text, so we only give 'MimeUnrender' instances for
-- 'String' and 'Text'.
--
-- @since 0.1.0.0
data HTML

-- | @since 0.1.0.0
instance Accept HTML where
  contentType _ = "text" // "html"

-- | @since 0.1.0.0
instance MimeUnrender HTML String where
  mimeUnrender _ = decodeUtf8String

-- | @since 0.1.0.0
instance MimeUnrender HTML Text where
  mimeUnrender _ = decodeUtf8Text

decodeUtf8String :: ByteString -> Either String String
decodeUtf8String = maybe (Left err) Right . TConv.decodeConvertText . UTF8
  where
    err = "Could not decode UTF-8"

decodeUtf8Text :: ByteString -> Either String Text
decodeUtf8Text = maybe (Left err) Right . TConv.decodeConvertText . UTF8
  where
    err = "Could not decode UTF-8"
