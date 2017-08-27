{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Views.Common.Rss(
  renderFeed
) where
import Data.Time (UTCTime)

import Text.Feed.Types
import Text.Feed.Util
import Text.RSS.Export
import Text.RSS.Syntax
import Text.XML.Light.Output

renderFeed :: String -> String -> String ->[(String,String,String,UTCTime)] -> String
renderFeed host name desc items =
    showTopElement $ xmlRSS feed
  where
    feed :: RSS
    feed = RSS "2.0" [] channel []
    channel :: RSSChannel
    channel =
      (nullChannel name host)
            {rssDescription = desc
            ,rssItems = map convertItem items
            }
    convertItem (url,title,summary,time) =
      (nullItem "")
        {rssItemTitle = Just $ title
        ,rssItemLink = Just $ url
        ,rssItemDescription = Just $ summary
        ,rssItemGuid = Just RSSGuid
                {rssGuidPermanentURL = Just False
                ,rssGuidAttrs = []
                ,rssGuidValue = url
                }
        ,rssItemPubDate = Just $ toFeedDateStringUTC (RSSKind $ Just "2.0") $ time
        }
