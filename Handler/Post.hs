module Handler.Post where

import Import

postForm :: Form Post
postForm = renderDivs $ Post
           <$> areq textField "Title" Nothing
           <*> areq textField "Content" Nothing

getPostR :: PostId -> Handler RepHtml
getPostR postId = do
  post <- runDB $ get postId
  defaultLayout [whamlet|#{show post}|]

getBlogR :: Handler RepHtml
getBlogR = defaultLayout [whamlet|<h1>Blog|]

postBlogR :: Handler RepHtml
postBlogR = do
  ((result, widget), enctype) <- runFormPost postForm
  case result of
    FormSuccess post -> do
      postId <- runDB $ insert post
      defaultLayout [whamlet|#{show post}|]
    _ -> defaultLayout [whamlet|
<p>Invalid
<form method=post action=@{PostR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]
